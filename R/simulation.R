library(fpp3)
library(fable.prophet)

source(here::here("R/fun_sim.R"))
source(here::here("R/function_rec2.R"))

# ============================================================
# Definition of the aggregation structure matrix (S matrix)
# ============================================================

S <- rbind(
  rep(1, 5),
  c(1, 1, 0, 0, 0),
  c(0, 0, 1, 1, 1),
  diag(5)
)

# ============================================================
# Autoregressive parameter
# ============================================================
Phi <- matrix(c(0.7, 0.2, 0.2, 0.7), 2)

# ================================================================
# Different correlation structures for V (between series A and B)
# ================================================================

# No correlation
V1 <- diag(2)

# Positive correlation
V2 <- matrix(c(1, 0.7, 0.7, 1), 2)

# Negative correlation
V3 <- matrix(c(1, -0.7, -0.7, 1), 2)

# ==================================================================
# Different correlation structures for Sigma (between bottom nodes)
# ==================================================================

# No correlation between bottom nodes
Sigma1 <- diag(5)

# Positive correlation
Sigma2 <- rbind(
  c(1, 0.7, 0, 0, 0),
  c(0.7, 1, 0, 0, 0),
  c(0, 0, 1, 0.7, 0.7),
  c(0, 0, 0.7, 1, 0.7),
  c(0, 0, 0.7, 0.7, 1)
)

# Negative correlation
Sigma3 <- rbind(
  c(1, -0.4, 0, 0, 0),
  c(-0.4, 1, 0, 0, 0),
  c(0, 0, 1, -0.4, -0.4),
  c(0, 0, -0.4, 1, -0.4),
  c(0, 0, -0.4, -0.4, 1)
)

# ============================================================
# Simulation design
# Each block changes the correlation structure of V and Sigma
# ============================================================

### V1 and Sigma1 ################

set.seed(30)

fc1 <- simulacao(Phi, V1, Sigma1, S, 1000)

save(fc1, file = "sim_rec1.RData")


### V1 and Sigma2  ################

set.seed(31)

fc2 <- simulacao(Phi, V1, Sigma2, S, 1000)

save(fc2, file = "sim_rec2.RData")


### V1 and Sigma3 ################

set.seed(32)

fc3 <- simulacao(Phi, V1, Sigma3, S, 1000)

save(fc3, file = "sim_rec3.RData")


### V2 and Sigma1 ################

set.seed(33)

fc4 <- simulacao(Phi, V2, Sigma1, S, 1000)

save(fc4, file = "sim_rec4.RData")


### V2 and Sigma2  ################

set.seed(34)

fc5 <- simulacao(Phi, V2, Sigma2, S, 1000)

save(fc5, file = "sim_rec5.RData")


### V2 and Sigma3  ###############

set.seed(35)

fc6 <- simulacao(Phi, V2, Sigma3, S, 1000)

save(fc6, file = "sim_rec6.RData")


### V3 and Sigma1  ##############

set.seed(36)

fc7 <- simulacao(Phi, V3, Sigma1, S, 1000)

save(fc7, file = "sim_rec7.RData")


### V3 and Sigma2  ############

set.seed(37)

fc8 <- simulacao(Phi, V3, Sigma2, S, 1000)

save(fc8, file = "sim_rec8.RData")
