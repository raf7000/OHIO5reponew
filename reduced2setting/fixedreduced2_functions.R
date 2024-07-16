library(lme4)
library(MASS)
library(parallel)
library(ggplot2)
library(tidyr)
library(dplyr)

# Defining the BIC functions
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

simulate_and_fit_models <- function(ni, m, beta, random_effects_var, bic_func, i) {
  tryCatch({
    message("Starting simulation ", i)
    # Data generation
    group_id <- rep(1:m, each = ni)
    x1 <- rnorm(ni * m, mean = 0, sd = 1)
    x2 <- rnorm(ni * m, mean = 1, sd = 1)
    x3 <- rnorm(ni * m, mean = 2, sd = 1)
    random_effect <- rep(rnorm(m, mean = 0, sd = sqrt(random_effects_var)), each = ni)
    
    y <- random_effect + beta[1] * x1 + beta[3] * x3 # True model is always reduced2
    
    data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, group_id = factor(group_id))
    
    # Check for non-finite values before fitting models
    if (any(!is.finite(y))) {
      stop("Non-finite values detected in 'y' before fitting models")
    }
    
    # Fit models
    model_full <- lmer(y ~ x1 + x2 + x3 + (1|group_id), data = data)
    model_reduced1 <- lmer(y ~ x1 + x2 + (1|group_id), data = data)
    model_reduced2 <- lmer(y ~ x1 + x3 + (1|group_id), data = data)
    model_reduced3 <- lmer(y ~ x2 + x3 + (1|group_id), data = data)
    
    # Check for convergence
    if (!is.finite(logLik(model_full)) || !is.finite(logLik(model_reduced1)) || 
        !is.finite(logLik(model_reduced2)) || !is.finite(logLik(model_reduced3))) {
      stop("Model did not converge")
    }
    
    # Calculate BIC values
    bic_full <- bic_func(model_full)
    bic_reduced1 <- bic_func(model_reduced1)
    bic_reduced2 <- bic_func(model_reduced2)
    bic_reduced3 <- bic_func(model_reduced3)
    
    # Select best model
    bics <- c(bic_full, bic_reduced1, bic_reduced2, bic_reduced3)
    model_names <- c("full", "reduced1", "reduced2", "reduced3")
    selected_model <- model_names[which.min(bics)]
    
    message("Ending simulation ", i)
    return(data.frame(
      true_model = "reduced2",
      selected_model = selected_model,
      bic_full = bic_full,
      bic_reduced1 = bic_reduced1,
      bic_reduced2 = bic_reduced2,
      bic_reduced3 = bic_reduced3
    ))
  }, error = function(e) {
    message("Error in simulation ", i, ": ", e)
    return(data.frame(
      true_model = NA,
      selected_model = NA,
      bic_full = NA,
      bic_reduced1 = NA,
      bic_reduced2 = NA,
      bic_reduced3 = NA
    ))
  })
}

# Run simulations for a single BIC definition
run_simulation <- function(bic_func, bic_name, num_simulations, ni, m, beta, random_effects_var) {
  cat("Running simulations for", bic_name, "BIC definition...\n")
  results <- mclapply(1:num_simulations, function(i) {
    simulate_and_fit_models(ni, m, beta, random_effects_var, bic_func, i)
  }, mc.cores = detectCores())
  
  # Combine results into a single data frame
  results_df <- do.call(rbind, results)
  results_df$BIC_definition <- bic_name
  
  return(results_df)
}

# Function to run simulations for different subject numbers
run_simulations_for_subject_numbers <- function(subject_numbers) {
  results_list <- list()
  
  for (ni in subject_numbers) {
    results_fitzmaurice <- run_simulation(bic_fitzmaurice, "Fitzmaurice", num_simulations, ni, m, beta, random_effects_var)
    results_normal <- run_simulation(bic_normal, "Normal", num_simulations, ni, m, beta, random_effects_var)
    results_hybrid <- run_simulation(bic_hybrid, "Hybrid", num_simulations, ni, m, beta, random_effects_var)
    
    all_results <- rbind(results_fitzmaurice, results_normal, results_hybrid)
    all_results <- na.omit(all_results)
    
    results_list[[paste0("ni_", ni)]] <- all_results
  }
  
  return(results_list)
}

