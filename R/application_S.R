compute_S <- function(Dados, state_meta, region_meta) {
  # ------------------------------------------------------------
  # Use metadata (single source of truth)
  # ------------------------------------------------------------
  bottom_nodes <- state_meta$UF

  # keep region order consistent with plots/tables
  regions <- region_meta |>
    dplyr::filter(Região != "Total") |>
    dplyr::arrange(order) |>
    dplyr::pull(Região)

  # ------------------------------------------------------------
  # validation
  # ------------------------------------------------------------
  missing_UF <- setdiff(bottom_nodes, unique(Dados$node))

  if (length(missing_UF) > 0) {
    warning(
      "States in metadata but missing from data: ",
      paste(missing_UF, collapse = ", ")
    )
  }

  # ------------------------------------------------------------
  # Region → state incidence matrix
  # ------------------------------------------------------------
  matrix_agg <- matrix(
    0,
    nrow = length(regions),
    ncol = length(bottom_nodes),
    dimnames = list(paste0("agg_", regions), bottom_nodes)
  )

  for (i in seq_len(nrow(state_meta))) {
    r <- paste0("agg_", state_meta$Região[i])
    c <- state_meta$UF[i]
    matrix_agg[r, c] <- 1
  }

  # ------------------------------------------------------------
  # Full summing matrix: Total, region aggregations, identity for bottom nodes
  # ------------------------------------------------------------
  S <- rbind(
    Total = rep(1, length(bottom_nodes)),
    matrix_agg,
    diag(length(bottom_nodes))
  )

  colnames(S) <- bottom_nodes

  # Name identity rows with bottom node names
  bottom_row_idx <- (nrow(S) - length(bottom_nodes) + 1):nrow(S)
  rownames(S)[bottom_row_idx] <- bottom_nodes

  return(S)
}
