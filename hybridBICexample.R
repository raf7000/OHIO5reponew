library(lme4)
library(MASS)



# New BIC function based on the hybrid definition
hybrid_BIC <- function(model, is_mixed = FALSE) {
  logLik_val <- logLik(model)
  n_params <- length(fixef(model)) # Number of fixed effects parameters
  N <- length(unique(model@frame$group_id)) # Number of groups
  n_tot <- nrow(model@frame) # Total number of observations
  
  if (is_mixed) {
    n_random_params <- length(VarCorr(model)) # Number of random effects parameters
    n_fixed_params <- n_params - n_random_params
    BIC_value <- -2 * logLik_val + n_random_params * log(N) + n_fixed_params * log(n_tot)
  } else {
    BIC_value <- -2 * logLik_val + n_params * log(n_tot) #hybrid definition 
  }
  
  return(as.numeric(BIC_value))
}

simulate_and_fit_models <- function(ni, m, beta, true_model, random_effects_var) {
  n <- ni * m
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  X3 <- rnorm(n)
  group_effect <- if (true_model == "mixed") rnorm(m, mean = 0, sd = sqrt(random_effects_var)) else rep(0, n)
  epsilon <- rnorm(n)
  y <- X1 * beta[1] + X2 * beta[2] + X3 * beta[3] + rep(group_effect, each = ni) + epsilon
  
  data <- data.frame(y = y, X1 = X1, X2 = X2, X3 = X3, group_id = as.factor(rep(1:m, each = ni)))
  model_full <- lmer(y ~ X1 + X2 + X3 - 1 + (1 | group_id), data = data)
  model_reduced1 <- lmer(y ~ X1 + X2 - 1 + (1 | group_id), data = data)
  model_reduced2 <- lmer(y ~ X1 + X3 - 1 + (1 | group_id), data = data)
  model_reduced3 <- lmer(y ~ X2 + X3 - 1 + (1 | group_id), data = data)
  
  bic_full <- hybrid_BIC(model_full, is_mixed = TRUE)
  bic_reduced1 <- hybrid_BIC(model_reduced1, is_mixed = TRUE)
  bic_reduced2 <- hybrid_BIC(model_reduced2, is_mixed = TRUE)
  bic_reduced3 <- hybrid_BIC(model_reduced3, is_mixed = TRUE)
  
  bics <- c(bic_full, bic_reduced1, bic_reduced2, bic_reduced3)
  model_names <- c("full", "reduced1", "reduced2", "reduced3")
  selected_model <- model_names[which.min(bics)]
  
  return(list(true_model = true_model, selected_model = selected_model, bic_full = bic_full, bic_reduced1 = bic_reduced1, bic_reduced2 = bic_reduced2, bic_reduced3 = bic_reduced3))
}

# Example usage
set.seed(123)
results <- simulate_and_fit_models(ni = 10, m = 30, beta = c(1, 1, 0), true_model = "mixed", random_effects_var = 1)
print(results)
