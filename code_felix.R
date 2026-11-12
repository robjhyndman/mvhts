library(dplyr)
library(psych)
library(clubSandwich)


# H <- E0 - E

# Wilks labda:




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


## Code targets.R:
# hierarchie
S_brazil <- brazil_hierarchy(state_meta, region_meta)
hierarchies <- list(small = small_hierarchy(), brazil = S_brazil)

# experiment 1
exp1_pop <- exp1_population(hierarchies)
exp1_scenarios <- exp1_sample_scenarios()

# X <- 1
exp1_samp <- lapply(X = 1:2, function(X) {exp1_sample(hierarchies[[exp1_scenarios$hierarchy[X]]], 
                         exp1_scenarios$hierarchy[X], 
                         exp1_scenarios$family[X], 
                         exp1_scenarios$dial[X], 
                         exp1_scenarios$T[X], 
                         reps = 1000)})

exp1_samp <- bind_rows(exp1_samp[[1]], exp1_samp[[2]])

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



