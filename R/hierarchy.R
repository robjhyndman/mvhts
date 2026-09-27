# ====================================================================
# Hierarchies and the stacked layout used throughout R/
#
# A hierarchy is given by its summing matrix S (n x n_b): one row per
# series, aggregates first (Total, then any intermediate level) and the
# n_b bottom-level series last, with row and column names giving the
# node names.
#
# Long data have columns time, node (the position in the hierarchy),
# series (the variable: A/B in the simulations, Admissões/Demissões in
# the application) and a value. With m variables, every vector and
# matrix column in R/ is in variable-major order: all n series of the
# first variable in the row order of S, then all n series of the second,
# and so on. This is vec(Y_t) for the n x m matrix Y_t, so the stacked
# summing and constraint matrices are S* = I_m (x) S and C* = I_m (x) C.
# Columns are named "variable.node", for example "A.agg_1".
# ====================================================================

# Simulation hierarchy: Total, two aggregates and five bottom-level series
small_hierarchy <- function() {
  S <- rbind(
    rep(1, 5),
    c(1, 1, 0, 0, 0),
    c(0, 0, 1, 1, 1),
    diag(5)
  )
  colnames(S) <- seq(5)
  rownames(S) <- c("Total", "agg_1", "agg_2", colnames(S))
  S
}

# Brazil: Total, the five regions (in region_meta order, named
# agg_<Região>) and the 27 federative units (in state_meta order)
brazil_hierarchy <- function(state_meta, region_meta) {
  regions <- region_meta |>
    dplyr::filter(Região != "Total") |>
    dplyr::arrange(order) |>
    dplyr::pull(Região)
  states <- state_meta$UF
  # Region x state incidence matrix
  A <- outer(regions, state_meta$Região, "==") * 1
  S <- rbind(Total = 1, A, diag(length(states)))
  dimnames(S) <- list(c("Total", paste0("agg_", regions), states), states)
  S
}

# Constraint matrix C = [I, -A] for S = [A; I], so that C S = 0
make_C <- function(S) {
  n_b <- NCOL(S)
  n_a <- NROW(S) - n_b
  if (!isTRUE(all.equal(unname(S[-seq_len(n_a), , drop = FALSE]), diag(n_b)))) {
    stop("S must have its identity block in the last n_b rows.")
  }
  cbind(diag(n_a), -S[seq_len(n_a), , drop = FALSE])
}

# I_m (x) X: S* from S, or C* from C
stack_matrix <- function(X, m) kronecker(diag(m), X)

# Rows/columns of variable j in the stacked system
var_index <- function(j, n) (j - 1) * n + seq_len(n)

# Block (j, k) of a stacked matrix
block <- function(X, j, k, n_row, n_col = n_row) {
  X[var_index(j, n_row), var_index(k, n_col), drop = FALSE]
}

# Column names "variable.node" in stacked order
stacked_names <- function(S, variables) {
  paste0(
    rep(variables, each = NROW(S)),
    ".",
    rep(rownames(S), times = length(variables))
  )
}

node_level <- function(node) {
  dplyr::case_when(
    node == "Total" ~ "Total",
    grepl("^agg_", node) ~ "Regions",
    TRUE ~ "States"
  )
}

# --------------------------------------------------------------------
# Every series of the hierarchy from the bottom-level series.
# bottom: long data (time, node, series, value) with node in colnames(S).
# Series i is the sum of the bottom-level series with S[i, ] == 1, so the
# result is coherent by construction.
# --------------------------------------------------------------------
aggregate_hierarchy <- function(bottom, S) {
  bottom <- tibble::as_tibble(bottom)
  missing <- setdiff(colnames(S), bottom$node)
  unknown <- setdiff(bottom$node, colnames(S))
  if (length(missing) > 0 || length(unknown) > 0) {
    stop(
      "Bottom-level nodes do not match colnames(S). Missing: ",
      paste(missing, collapse = ", "),
      "; unknown: ",
      paste(unknown, collapse = ", ")
    )
  }
  purrr::map(rownames(S), \(i) {
    bottom |>
      dplyr::filter(node %in% colnames(S)[S[i, ] == 1]) |>
      dplyr::group_by(time, series) |>
      dplyr::summarise(value = sum(value), .groups = "drop") |>
      dplyr::mutate(node = i, .after = time)
  }) |>
    dplyr::bind_rows()
}
