# library(dplyr)
# library(psych)
# library(clubSandwich)

library(tidyverse)
library(rugarch)


# pre-stuff for the test statistics:
# H <- E0 - E

# Wilks lambda:


# Pillai:
# tr(H(H+E)^-1) = tr(H * E0^-1)
# solve(E0) %*% H ist das gleiche wie solve(E0, H) 

# Zwischenschritte:
H %*% solve(E0) # Matrix ist anders, aber trace ist gleich
solve(E0, H)
solve(E0) %*% H

tr(solve(E0, H))

lam <- eigen(solve(E, H))$values
sum(lam / (1+lam)) # das ist das gleiche

# Lawley-Hotelling:
tr(solve(E) %*% (E0-E))
tr(solve(E, H))

sum(eigen(solve(E) %*% H)$values)


# Roy:
max(Re(eigen(solve(E, H))$values))

################################
## Code from targets.R:
# load relevant functions:
file_names <- c("R/hierarchy.R", "R/experiment1.R", "R/application_data.R", 
                "R/theory.R", "R/simulation_functions.R", "R/diagnostic.R", 
                "R/reconciliation.R")
lapply(file_names, source)



# hierarchies:
S_brazil <- brazil_hierarchy(state_meta, region_meta)
hierarchies <- list(small = small_hierarchy(), brazil = S_brazil)

# experiment 1:
exp1_pop <- exp1_population(hierarchies)
exp1_scenarios <- exp1_sample_scenarios()

# only first 2 scenarios to check if the code runs:
exp1_samp <- lapply(X = 1:2, function(X) {exp1_sample(hierarchies[[exp1_scenarios$hierarchy[X]]], 
                         exp1_scenarios$hierarchy[X], 
                         exp1_scenarios$family[X], 
                         exp1_scenarios$dial[X], 
                         exp1_scenarios$T[X], 
                         reps = 1000)})

exp1_samp <- dplyr::bind_rows(exp1_samp[[1]], exp1_samp[[2]])
exp1_summary <- exp1_sample_summary(exp1_samp)
# num_exp1 <- numbers_exp1(exp1_pop, exp1_summary, exp1_samp)



# Diagnostics:
power_scenarios <- kappa_power_scenarios()

power <- t(sapply(X = 1:64, function(X) {kappa_power(hierarchies[[power_scenarios$hierarchy[X]]],
                                                     power_scenarios$hierarchy[X], 
                                                     power_scenarios$family[X], 
                                                     power_scenarios$dial[X], 
                                                     power_scenarios$T[X], 
                                                     reps = 1000)}
))
power



power_df <- data.frame(sapply(1:2, function(X)unlist(power[,X])),
           sapply(3:17, function(X)as.numeric(unlist(power[,X]))))
colnames(power_df) <- colnames(power)
power_df$fam <- paste0(power_df$family, "_", power_df$dial)


# Plots Niveau and Power:
pdf("Power_plots.pdf")
par(mfrow = c(2,2))
for(j in c("small", "brazil")){
  for(i in c("F0", "F1")){
  power_dummy <- power_df %>% filter(family == i, hierarchy == j)
  plot(power_dummy$T, power_dummy$rejection_rao, type = "b", 
       ylim = range(power_dummy[,c(8,10,12,14)]), 
       main = paste0("Scenario ", i, ", hierarchy ", j), 
       xlab = "T", ylab = "rejection rate")
  points(power_dummy$T, power_dummy$rejection_pillai, type = "b", col = "blue")
  points(power_dummy$T, power_dummy$rejection_LH, type = "b", col = "red")
  points(power_dummy$T, power_dummy$rejection_roy, type = "b", col = "orange")
  abline(h = 0.05, col = "grey")
  }
}
legend("left", c("Rao", "Pillai", "Hotelling-Lawley", "Roy"), 
       lty = 1, col = c("black", "blue", "red", "orange"))

par(mfrow = c(2,3))
for(j in c("small", "brazil")){
  for(i in c("F2_0.25", "F2_0.5", "F2_1", "F3_0.2", "F3_0.4", "F3_0.6")){
    power_dummy <- power_df %>% filter(fam == i, hierarchy == j)
    plot(power_dummy$T, power_dummy$rejection_rao, type = "b", 
         ylim = range(power_dummy[,c(8,10,12,14)]), 
         main = paste0("Scenario ", i, ", hierarchy ", j), 
         xlab = "T", ylab = "rejection rate")
    points(power_dummy$T, power_dummy$rejection_pillai, type = "b", col = "blue")
    points(power_dummy$T, power_dummy$rejection_LH, type = "b", col = "red")
    points(power_dummy$T, power_dummy$rejection_roy, type = "b", col = "orange")
    abline(h = 0.05, col = "grey")
  }
  legend("bottomright", c("Rao", "Pillai", "Hotelling-Lawley", "Roy"), 
         lty = 1, col = c("black", "blue", "red", "orange"))
}

dev.off()

# Tables supplement:
# hierarchy small, T = 100 and T = 200
cbind(power_df %>% filter(hierarchy == "small", T == 100) %>% select(family, dial, kappa, rejection_rao, rejection_pillai, rejection_LH),
      power_df %>% filter(hierarchy == "small", T == 200) %>% select(rejection_rao, rejection_pillai, rejection_LH))
# hierarchy brazil, T = 100 and T = 200
cbind(power_df %>% filter(hierarchy == "brazil", T == 100) %>% select(family, dial, kappa, rejection_rao, rejection_pillai, rejection_LH),
      power_df %>% filter(hierarchy == "brazil", T == 200) %>% select(rejection_rao, rejection_pillai, rejection_LH))
## -> similar values as in the paper


# power_long <- power_df %>% 
#   pivot_longer(
#     cols = c(rejection_rao, rejection_pillai, rejection_LH, rejection_roy),
#     names_to = "metric",
#     values_to = "rejection_rate"
#   )

################################
# different error scenarios:

scen_errors <- data.frame(index = 1:9, 
                          rbind(data.frame(error = "normal", param = NA), 
                          tidyr::expand_grid(error = "t", param = c(5,10)), 
                          tidyr::expand_grid(error = "GARCH", param = c(0.05, 0.1, 0.2)),
                          tidyr::expand_grid(error = "AR", param = c(0.1, 0.2, 0.3))))

scen_main <- tidyr::expand_grid(index = 1:9, 
            family = c("F0", "F1"), 
            hierarchy = c("small"),
            T = c(100, 200, 500, 1000))

scen_dial <- data.frame(family = c("F0", "F1"), dial = c(0.7, 0.2))

scenarios_merge1 <- merge(scen_main, scen_errors, by = "index")
scenarios_error_sim <- merge(scenarios_merge1, scen_dial, by = "family")


# secification of GARCH model, now in the mygarch() fct in kappa_power:
# uspec <- ugarchspec(
#   variance.model = list(model = "sGARCH",   # Standard‑GARCH
#                         garchOrder = c(1,1)),
#   mean.model     = list(armaOrder = c(0,0), # reiner Fehlerprozess
#                         include.mean = FALSE),
#   distribution.model = "norm",            # Normalverteilung
#   fixed.pars = list(omega = 0.05, alpha1 = 0.1, beta1 = 0.85)
# )


mygarch <- function(param, W,  T){
  # specification of the GARCH model
  uspec <- ugarchspec(
    variance.model = list(model = "sGARCH",
                          garchOrder = c(1,1)),
    mean.model     = list(armaOrder = c(0,0), 
                          include.mean = FALSE),
    distribution.model = "norm",
    fixed.pars = list(omega = param, alpha1 = 0.1, beta1 = 0.85)
  )
  
  # simulate:
  garch_obj <- rugarch::ugarchpath(uspec, 
                                   n.sim = T,
                                   m.sim = NCOL(W),
                                   n.start = 200)
  
  # calculate W^(1/2) with eigenvalues and -vectors
  W_eigen <- eigen(W)
  W_sqrt <- W_eigen$vectors %*% diag(sqrt(W_eigen$values)) %*% t(W_eigen$vectors)
  
  #W^(1/2) * GARCH-errors:
  t(W_sqrt %*% t(fitted(garch_obj)))
}

# apply kappa_power function for one setting:
kappa_power(S = hierarchies$small, 
            hierarchy = "small", 
            family = "F1", 
            dial = 0.2,
            error = "GARCH",
            param = 0.1,
            T = 500, 
            reps = 1000)



# apply kappa_power function for all settings:
errors <- t(sapply(X = 1:72, function(X){tryCatch(expr = {kappa_power(hierarchies[[scenarios_error_sim$hierarchy[X]]], 
                                                      hierarchy = scenarios_error_sim$hierarchy[X], 
                                                      family = scenarios_error_sim$family[X], 
                                                      dial = scenarios_error_sim$dial[X],
                                                      error = scenarios_error_sim$error[X],
                                                      param = scenarios_error_sim$param[X],
                                                      T = scenarios_error_sim$T[X], 
                                                      reps = 1000)},
                                                  error = function(e){
                                                    tibble::tibble(
                                                      hierarchy = scenarios_error_sim$hierarchy[X],
                                                      family = scenarios_error_sim$family[X],
                                                      dial = scenarios_error_sim$dial[X],
                                                      error = scenarios_error_sim$error[X],
                                                      param = scenarios_error_sim$param[X],
                                                      T = scenarios_error_sim$T[X],
                                                      kappa = NA,
                                                      gain = NA,
                                                      reps = NA,
                                                      rejection_rao = NA,
                                                      rejection_rao_se = NA,
                                                      rejection_pillai = NA,
                                                      rejection_pillai_se = NA,
                                                      rejection_LH = NA,
                                                      rejection_LH_se = NA,
                                                      rejection_roy = NA,
                                                      rejection_roy_se = NA,
                                                      kappa_hat = NA,
                                                      gain_hat = NA
                                                    )
                                                  }
                                                      
)}))
errors[,-c(11,13,15,17)]


# transform simulation results to make plots:
errors_df <- data.frame(sapply(1:2, function(X)unlist(errors[,X])),
                        as.numeric(unlist(errors[,3])),
                        unlist(errors[,4]),
                        sapply(5:19, function(X)as.numeric(unlist(errors[,X]))))
colnames(errors_df) <- colnames(errors)


pdf("error_scenarios.pdf")
par(mfrow = c(3,3))
for(j in c("F0", "F1")){
  for(i in 1:9){
    if(i == 1){
      error_dummy <- errors_df %>% filter(error == scen_errors[i,"error"], family == j)
    }
    else{
      error_dummy <- errors_df %>% filter(error == scen_errors[i,"error"], param == scen_errors[i, "param"], family == j)
    }
    error_dummy <- error_dummy[order(error_dummy$T),]
    text_main <- paste0(j, ", ", scen_errors[i, "error"], " errors, param ", scen_errors[i, "param"])

    plot(error_dummy$T, error_dummy$rejection_rao, type = "b", 
         ylim = c(0,1),# range(error_dummy[,c(10,12,14,16)]), 
         main = text_main, 
         xlab = "T", ylab = "rejection rate")
    points(error_dummy$T, error_dummy$rejection_pillai, type = "b", col = "blue")
    points(error_dummy$T, error_dummy$rejection_LH, type = "b", col = "red")
    points(error_dummy$T, error_dummy$rejection_roy, type = "b", col = "orange")
    abline(h = 0.05, col = "grey")
  }
}
legend("topleft", c("Wilks", "Pillai", "Hotelling-Lawley", "Roy"), 
       lty = 1, col = c("black", "blue", "red", "orange"))
dev.off()


errors_df %>% filter(error == "AR", family == "F0") %>% arrange(param, T)
