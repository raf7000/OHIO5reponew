library(lme4)
library(MASS)
library(parallel)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

# Defining the BIC functions, looking at three definitions 
bic_fitzmaurice <- function(model) {
  logLik_val <- as.numeric(logLik(model))
  n_params <- length(fixef(model)) + sum(sapply(VarCorr(model), function(x) prod(dim(x))))
  N <- length(unique(model@frame$group_id)) # Number of subjects
  BIC_value <- -2 * logLik_val + log(N) * n_params
  return(BIC_value)
}

bic_normal <- function(model) {
  logLik_val <- as.numeric(logLik(model))
  n_params <- length(fixef(model)) + sum(sapply(VarCorr(model), function(x) prod(dim(x))))
  N <- nrow(model@frame) # Total number of observations
  BIC_value <- -2 * logLik_val + log(N) * n_params
  return(BIC_value)
}

bic_hybrid <- function(model) {
  logLik_val <- as.numeric(logLik(model))
  n_fixed <- length(fixef(model))
  n_random <- sum(sapply(VarCorr(model), function(x) prod(dim(x))))
  N <- nrow(model@frame) # Total number of observations
  m <- length(unique(model@frame$group_id)) # Number of groups
  BIC_value <- -2 * logLik_val + n_fixed * log(N) + n_random * log(m)
  return(BIC_value)
}

# Data generation function
generate_data <- function(ni, m, beta, random_effects_var) {
  group_id <- rep(1:m, each = ni)
  x1 <- rnorm(ni * m)
  x2 <- rnorm(ni * m)
  x3 <- rnorm(ni * m)
  epsilon <- rnorm(ni * m)
  random_effect <- rep(rnorm(m, mean = 0, sd = sqrt(random_effects_var)), each = ni)
  y <- beta[1] * x1 + beta[3] * x3 + rep(random_effect, each = ni) + epsilon
  
  data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, group_id = factor(group_id))
  return(data)
}

# Model fitting function
fit_models <- function(data) {
  models <- list(
    full = lmer(y ~ x1 + x2 + x3 + (1|group_id), data = data),
    reduced1 = lmer(y ~ x1 + x2 + (1|group_id), data = data),
    reduced2 = lmer(y ~ x1 + x3 + (1|group_id), data = data),
    reduced3 = lmer(y ~ x2 + x3 + (1|group_id), data = data)
  )
  return(models)
}

# BIC calculation function
calculate_bic <- function(models, bic_func) {
  bic_values <- sapply(models, bic_func)
  return(bic_values)
}

# Simulate and fit models
simulate_and_fit_models <- function(ni, m, beta, random_effects_var, i) {
  message("Starting simulation ", i)
  data <- generate_data(ni, m, beta, random_effects_var)
  models <- fit_models(data)
  
  bic_fitzmaurice_values <- calculate_bic(models, bic_fitzmaurice)
  bic_normal_values <- calculate_bic(models, bic_normal)
  bic_hybrid_values <- calculate_bic(models, bic_hybrid)
  
  selected_model_fitzmaurice <- names(which.min(bic_fitzmaurice_values))
  selected_model_normal <- names(which.min(bic_normal_values))
  selected_model_hybrid <- names(which.min(bic_hybrid_values))
  
  results_df <- data.frame(
    true_model = "reduced2",
    model_name = names(models),
    bic_fitzmaurice = bic_fitzmaurice_values,
    bic_normal = bic_normal_values,
    bic_hybrid = bic_hybrid_values,
    selected_model_fitzmaurice = selected_model_fitzmaurice,
    selected_model_normal = selected_model_normal,
    selected_model_hybrid = selected_model_hybrid,
    correct_model_fitzmaurice = selected_model_fitzmaurice == "reduced2",
    correct_model_normal = selected_model_normal == "reduced2",
    correct_model_hybrid = selected_model_hybrid == "reduced2"
  )
  
  message("Ending simulation ", i)
  return(results_df)
}

# Function to run multiple simulations
run_simulation <- function(num_simulations, ni, m, beta, random_effects_var) {
  cat("Running simulations...\n")
  results <- mclapply(1:num_simulations, function(i) {
    simulate_and_fit_models(ni, m, beta, random_effects_var, i)
  }, mc.cores = detectCores())
  
  return(do.call(rbind, results))
}

# Modified function to run simulations for different subject numbers
run_simulations_for_subject_numbers <- function(subject_numbers, num_simulations, m, beta, random_effects_var) {
  results_list <- list()
  
  for (ni in subject_numbers) {
    cat(sprintf("\nRunning simulations for subject number ni = %d...\n", ni))
    results <- run_simulation(num_simulations, ni, m, beta, random_effects_var)
    results <- na.omit(results)
    results_list[[paste0("ni_", ni)]] <- results
  }
  
  return(results_list)
}
