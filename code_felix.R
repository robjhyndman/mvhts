# ====================================================================
# Robustness of the Wilks test: interactive script (no targets needed)
#
# Run from the repository root, section by section. All the functions
# live in R/ and are the same ones the targets pipeline uses, so results
# here match the paper up to simulation noise (the seeds differ).
#
#   sim_errors()        errors with covariance W: "normal", "t", "GARCH", "AR"
#   cross_test()        Wilks test with Rao's F approximation (and Pillai,
#                       Hotelling-Lawley, Roy)
#   cross_test_boot()   wild bootstrap p-value for the Wilks test
#   boot_size()         rejection rates of Rao's F and the bootstrap for
#                       one scenario (Supplementary Table S2)
#   boot_size_scenarios()  the full grid of scenarios for Table S2
#   kappa_power()       rejection rates of all four statistics
# ====================================================================

library(dplyr)

# Load every function in R/ (as the pipeline does) and build the two
# hierarchies
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
S_brazil <- brazil_hierarchy(state_meta, region_meta)
hierarchies <- list(small = small_hierarchy(), brazil = S_brazil)

# Covariance designs of Experiment 1: F0 (dial 0.7) and F1 (dial 2) are
# the nulls; F2 and F3 are departures
sample_families()

# --------------------------------------------------------------------
# 1. Quick checks for one scenario (seconds)
# --------------------------------------------------------------------
set.seed(1)
S <- hierarchies$small
W <- exp1_covariance(S, "F0", 0.7)

# One sample of GARCH errors and the two p-values
e <- sim_errors(T = 200, W = W, error = "GARCH", param = 0.1)
cross_test(e, S)$p_value_rao
cross_test_boot(e, S, B = 199)$p_value_boot

# Rejection rates over replications for one scenario (small hierarchy,
# t errors with 5 df): Rao's F over-rejects, the bootstrap does not
boot_size(S, "small", "F0", 0.7, T = 200, error = "t", param = 5, reps = 100)

# All four statistics (Wilks/Rao, Pillai, Hotelling-Lawley, Roy)
kappa_power(S, "small", "F0", 0.7, T = 200, error = "t", param = 5, reps = 100)

# --------------------------------------------------------------------
# 2. Your new test: CR2 with the HTZ correction (clubSandwich)
#
# For variable j, regress its bottom-level errors Y on X_full = (1, own
# incoherences, other variable's incoherences) and test that the
# coefficients of the other variable's incoherences are zero. The data
# set-up below is the same as in cross_test(). Return the Bonferroni
# p-value over the m variables, like cross_test().
# Once it works, add it to boot_size() in R/diagnostic.R as a third
# p-value next to cross_test() and cross_test_boot().
# --------------------------------------------------------------------
cross_test_cr2 <- function(e, S, m = 2) {
  C <- make_C(S)
  n <- NROW(S)
  n_b <- NCOL(S)
  n_a <- NROW(C)
  z <- lapply(seq_len(m), \(j) e[, var_index(j, n), drop = FALSE] %*% t(C))
  p_each <- vapply(
    seq_len(m),
    \(j) {
      Y <- e[, var_index(j, n)[n_a + seq_len(n_b)], drop = FALSE]
      z_own <- z[[j]]
      z_other <- do.call(cbind, z[-j])
      # TODO: fit lm(Y ~ z_own + z_other) (an mlm), compute
      # clubSandwich::vcovCR(fit, cluster = seq_len(NROW(Y)), type = "CR2"),
      # and test the z_other coefficients for every response with
      # clubSandwich::Wald_test(fit, constraints = ..., vcov = ..., test = "HTZ").
      # Return the p-value.
      NA_real_
    },
    numeric(1)
  )
  min(1, m * min(p_each))
}

cross_test_cr2(e, S)

# --------------------------------------------------------------------
# 3. The full grid of Supplementary Table S2, in parallel
#
# Each scenario runs `reps` replications of Rao's F and the bootstrap.
# With reps = 1000 the full grid takes about 18 CPU hours (about 50
# minutes on 22 cores); the Brazilian hierarchy at large T is the slow
# part. While developing, run the small hierarchy only and fewer reps.
# --------------------------------------------------------------------
reps <- 200
n_cores <- max(1, parallel::detectCores() - 1)

scen <- boot_size_scenarios() |>
  filter(hierarchy == "small") # remove this line for the full grid
scen

cl <- parallel::makeCluster(n_cores)
invisible(parallel::clusterEvalQ(cl, {
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
}))
parallel::clusterExport(cl, c("hierarchies", "scen", "reps"))
parallel::clusterSetRNGStream(cl, 2026)
boot <- parallel::parLapply(cl, seq_len(NROW(scen)), \(i) {
  s <- scen[i, ]
  boot_size(
    hierarchies[[s$hierarchy]],
    s$hierarchy,
    s$family,
    s$dial,
    s$T,
    error = s$error,
    param = s$param,
    reps = reps
  )
}) |>
  bind_rows()
parallel::stopCluster(cl)

saveRDS(boot, "boot_felix.rds")

# Size (F0, F1) for each error distribution: Rao's F vs bootstrap
boot |>
  filter(family %in% c("F0", "F1")) |>
  select(hierarchy, family, error, param, T, rejection_rao, rejection_boot) |>
  arrange(hierarchy, family, error, param, T) |>
  print(n = Inf)

# Plot: rejection rate against T, one panel per error distribution
null <- boot |>
  filter(family %in% c("F0", "F1")) |>
  mutate(errors = ifelse(is.na(param), error, paste(error, param)))
panels <- unique(null[c("hierarchy", "family", "errors")])
pdf("boot_felix.pdf", width = 10, height = 10)
par(mfrow = c(4, 4))
for (i in seq_len(NROW(panels))) {
  d <- null |>
    filter(
      hierarchy == panels$hierarchy[i],
      family == panels$family[i],
      errors == panels$errors[i]
    ) |>
    arrange(T)
  plot(
    d$T,
    d$rejection_rao,
    type = "b",
    log = "x",
    ylim = c(0, 1),
    main = paste(panels$hierarchy[i], panels$family[i], panels$errors[i]),
    xlab = "T",
    ylab = "rejection rate"
  )
  points(d$T, d$rejection_boot, type = "b", col = "blue")
  abline(h = 0.05, col = "grey")
}
legend("topleft", c("Rao's F", "Wild bootstrap"), lty = 1, col = c("black", "blue"))
dev.off()
