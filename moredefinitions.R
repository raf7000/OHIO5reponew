library(lme4)
library(MASS)
library(parallel)
library(ggplot2)
library(tidyr)
library(dplyr)

# Defining the BIC functions
bic_balding <- function(model) {
  logLik_val <- as.numeric(logLik(model))
  n_params <- length(fixef(model)) + sum(sapply(VarCorr(model), function(x) prod(dim(x))))
  N <- length(unique(model@frame$group_id)) # Assuming N is the number of groups
  BIC_value <- -2 * (logLik_val - 0.5 * n_params * log(N))
  return(BIC_value)
}

bic_pinheiro <- function(model) {
  logLik_val <- as.numeric(logLik(model))
  n_params <- length(fixef(model)) + sum(sapply(VarCorr(model), function(x) prod(dim(x))))
  N <- nrow(model@frame)
  BIC_value <- -2 * logLik_val + n_params * log(N)
  return(BIC_value)
}

bic_lai_gao <- function(model) {
  logLik_val <- as.numeric(logLik(model))
  n_params <- length(fixef(model)) + sum(sapply(VarCorr(model), function(x) prod(dim(x))))
  N <- nrow(model@frame)
  BIC_value <- -2 * logLik_val + n_params * log(N)
  return(BIC_value)
}

# Updated simulation function with a parameter for the BIC function
simulate_and_fit_models <- function(ni, m, beta, random_effects_var, true_model, noise_level = 1, bic_func) {
  # Data generation (unchanged)
  group_id <- rep(1:m, each = ni)
  x1 <- rnorm(ni * m)
  x2 <- rnorm(ni * m)
  x3 <- rnorm(ni * m)
  random_effect <- rep(rnorm(m, 0, sqrt(random_effects_var)), each = ni)
  epsilon <- rnorm(ni * m, 0, noise_level)
  
  y <- random_effect + epsilon
  if (true_model == "full" || true_model == "reduced1" || true_model == "reduced2") y <- y + beta[1] * x1
  if (true_model == "full" || true_model == "reduced1" || true_model == "reduced3") y <- y + beta[2] * x2
  if (true_model == "full" || true_model == "reduced2" || true_model == "reduced3") y <- y + beta[3] * x3
  
  outlier_indices <- sample(1:(ni*m), size = round(0.01 * ni * m))
  y[outlier_indices] <- y[outlier_indices] + rnorm(length(outlier_indices), 0, 5)
  
  data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, group_id = factor(group_id))
  
 