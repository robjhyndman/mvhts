compute_S <- function(Dados) {
  n <- length(unique(Dados$node))

  # Number of series: Admissions and Dismissals
  m <- length(unique(Dados$series))

  # Number of states within each region
  count_agg <- Dados |>
    as_tibble() |>
    select(Região, node) |>
    distinct() |>
    filter(!node %in% c("Total", paste0("agg_", unique(Região)))) |>
    count(Região)

  # Number of intermediate nodes (regions)
  n_agg <- NROW(count_agg)

  # Regions for each state (used to build the aggregation matrix)
  regioes <- Dados |>
    as_tibble() |>
    select(node, Região) |>
    distinct() |>
    filter(!node %in% c("Total", paste0("agg_", unique(Região))))

  # ============================================================
  # Build summing matrix S (hierarchical structure)
  # ============================================================

  # Aggregation matrix for regions → states
  state_region <- rep(seq_len(n_agg), count_agg$n)
  matrix_agg <- t(model.matrix(~ factor(state_region) - 1))
  attributes(matrix_agg)$assign <- NULL
  attributes(matrix_agg)$contrasts <- NULL
  rownames(matrix_agg) <- c(
    "Norte",
    "Nordeste",
    "Centro-Oeste",
    "Sudeste",
    "Sul"
  )
  colnames(matrix_agg) <- c(
    regioes$node[regioes$Região == "Norte"],
    regioes$node[regioes$Região == "Nordeste"],
    regioes$node[regioes$Região == "Centro-Oeste"],
    regioes$node[regioes$Região == "Sudeste"],
    regioes$node[regioes$Região == "Sul"]
  )

  # Full summing matrix
  S <- rbind(
    rep(1, n - n_agg - 1), # Total (Brazil)
    matrix_agg, # Regions
    diag(1, n - n_agg - 1) # States (bottom level)
  )
  rownames(S)[1] <- "Total"
  rownames(S)[(1 + n_agg) + seq_len(n - n_agg - 1)] <- colnames(matrix_agg)
  return(S)
}
