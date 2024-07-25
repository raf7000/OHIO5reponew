
library(MASS)  

generate_data_mixed_correlated <- function(n, groups, beta, random_effects_cov_matrix, skewness_factor, kurtosis_factor) {
  # Group random effects with correlation
  group_effect <- mvrnorm(n, mu = rep(0, ncol(random_effects_cov_matrix)), Sigma = random_effects_cov_matrix)
  group_id <- sample(1:groups, n, replace = TRUE)
  random_effects <- group_effect[group_id, ]
  
  # data alg
  epsilon <- rnorm(n) ^ skewness_factor  # Skewness
  X <- rnorm(n) * kurtosis_factor        # Kurtosis
  
  #random effects
  y <- X * beta + rowSums(random_effects) + epsilon
  
  data.frame(y = y, X = X, group_id = as.factor(group_id))
}


fit_model_mixed_and_compare <- function(data) {
  fit <- lmer(y ~ X + (1 | group_id), data = data)
  c(BIC = BIC(fit), AIC = AIC(fit))
}

set.seed(123)
random_effects_cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)  # Covariance matrix for random effects
data <- generate_data_mixed_correlated(n = 100, groups = 10, beta = 0.5, random_effects_cov_matrix = random_effects_cov_matrix,
                                       skewness_factor = 2, kurtosis_factor = 2)
criteria <- fit_model_mixed_and_compare(data)
print(criteria)


n_simulations <- 100
results <- data.frame()
skewness_levels <- c(1, 2, 3)
kurtosis_levels <- c(1, 2, 3)
models <- c("fixed", "mixed")

for (skew in skewness_levels) {
  for (kurt in kurtosis_levels) {
    for (model in models) {
      sim_results <- replicate(n_simulations, {
        data <- generate_data_mixed_correlated(n = 100, groups = 10, beta = 0.5, random_effects_cov_matrix = random_effects_cov_matrix,
                                               skewness_factor = skew, kurtosis_factor = kurt)
        fit_model_mixed_and_compare(data)
      }, simplify = FALSE)
      
      correct_selections_bic <- sum(sapply(sim_results, function(x) x["BIC"] < x["AIC"]))
      correct_selections_aic <- sum(sapply(sim_results, function(x) x["AIC"] < x["BIC"]))
      
      # Store results
      new_row <- data.frame(Skewness = skew, Kurtosis = kurt, Model = model, 
                            CorrectSelectionsBIC = correct_selections_bic,
                            CorrectSelectionsAIC = correct_selections_aic,
                            Total = n_simulations,
                            CorrectRateBIC = correct_selections_bic / n_simulations,
                            CorrectRateAIC = correct_selections_aic / n_simulations,
                            stringsAsFactors = FALSE)
      results <- rbind(results, new_row)
    }
  }
}

print(results)
