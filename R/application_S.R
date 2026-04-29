compute_S <- function(Dados) {
  if (!("node" %in% names(Dados))) {
    stop("Dados must contain a 'node' column")
  }
  if (!("Região" %in% names(Dados))) {
    stop("Dados must contain a 'Região' column")
  }

  all_nodes <- unique(Dados$node)

  agg_nodes <- sort(all_nodes[startsWith(all_nodes, "agg_")])
  bottom_nodes <- sort(setdiff(all_nodes, c("Total", agg_nodes)))

  if (length(bottom_nodes) == 0) {
    stop("No bottom-level nodes detected (states).")
  }

  # Map each bottom node (state) to exactly one region.
  mapping <- Dados |>
    as_tibble() |>
    select(node, Região) |>
    distinct() |>
    filter(node %in% bottom_nodes)

  check_map <- mapping |>
    count(node, name = "n_region") |>
    filter(n_region != 1)

  if (nrow(check_map) > 0) {
    stop(
      "Some bottom nodes map to zero or multiple regions: ",
      paste(check_map$node, collapse = ", ")
    )
  }

  # Expected agg nodes based on mapping
  regions <- sort(unique(mapping$Região))
  expected_agg <- sort(paste0("agg_", regions))

  if (!all(expected_agg %in% agg_nodes)) {
    missing <- setdiff(expected_agg, agg_nodes)
    stop(
      "Missing aggregated nodes for some regions. Expected but not found: ",
      paste(missing, collapse = ", ")
    )
  }

  # Use deterministic region/agg order (sorted by agg node name).
  agg_nodes <- sort(intersect(agg_nodes, expected_agg))

  # Region -> state incidence matrix
  matrix_agg <- matrix(
    0,
    nrow = length(agg_nodes),
    ncol = length(bottom_nodes),
    dimnames = list(agg_nodes, bottom_nodes)
  )

  for (i in seq_len(nrow(mapping))) {
    r <- paste0("agg_", mapping$Região[[i]])
    c <- mapping$node[[i]]
    matrix_agg[r, c] <- 1
  }

  # Full summing matrix: Total, region aggregations, identity for bottom nodes
  S <- rbind(
    Total = rep(1, length(bottom_nodes)),
    matrix_agg,
    diag(length(bottom_nodes))
  )

  colnames(S) <- bottom_nodes

  # Name identity rows with bottom node names
  bottom_row_idx <- (nrow(S) - length(bottom_nodes) + 1):nrow(S)
  rownames(S)[bottom_row_idx] <- bottom_nodes

  S
}
