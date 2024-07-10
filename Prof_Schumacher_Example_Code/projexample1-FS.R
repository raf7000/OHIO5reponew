library(lme4)
library(MASS)

## package mvtnorm
## package skewlmm

simulate_and_fit_models <- function(ni, m, beta, true_model, random_effects_var) {
  # data alg 
  n <- ni*m
  X <- rnorm(n)
  group_effect <- if (true_model == "mixed") rnorm(m, mean = 0, sd = sqrt(random_effects_var)) else rep(0, n)
  ## one random number per subject
  epsilon <- rnorm(n)
  y <- X * beta + rep(group_effect, each = ni) + epsilon
  
  data <- data.frame(y = y, X = X, group_id = as.factor(rep(1:m,each = ni)))
  model_fixed <- lm(y ~ X-1, data = data)
  model_mixed <- lmer(y ~ X -1 + (1 | group_id), data = data)
  
  bic_fixed <- BIC(model_fixed)
  bic_mixed <- BIC(model_mixed)
  
  #which model has the lower BIC
  selected_model <- ifelse(bic_fixed < bic_mixed, "fixed", "mixed")
  
  return(list(true_model = true_model, selected_model = selected_model, bic_fixed = bic_fixed, bic_mixed = bic_mixed))
}

