library(lme4)
library(MASS)


simulate_and_fit_models <- function(n, beta, true_model, random_effects_var, skewness_factor, kurtosis_factor) {
  # data alg 
  X <- rnorm(n) * kurtosis_factor
  group_effect <- if (true_model == "mixed") rnorm(n, mean = 0, sd = sqrt(random_effects_var)) else rep(0, n)
  epsilon <- rnorm(n) ^ skewness_factor
  y <- X * beta + group_effect + epsilon
  
  data <- data.frame(y = y, X = X, group_id = as.factor(sample(1:10, n, replace = TRUE)))
  model_fixed <- lm(y ~ X, data = data)
  model_mixed <- lmer(y ~ X + (1 | group_id), data = data)
  
  bic_fixed <- BIC(model_fixed)
  bic_mixed <- BIC(model_mixed)
  
  #which model has the lower BIC
  selected_model <- ifelse(bic_fixed < bic_mixed, "fixed", "mixed")
  
  return(list(true_model = true_model, selected_model = selected_model, bic_fixed = bic_fixed, bic_mixed = bic_mixed))
}


n_simulations <- 1000 
results <- data.frame(Skewness = integer(),  
                      Kurtosis = integer(), 
                      Model = character(), 
                      CorrectSelections = integer(),
                      Total = integer(), 
                      CorrectRate = numeric(),
                      stringsAsFactors = FALSE)  # To ensure data consistency

skewness_levels <- c(1, 2, 3)
kurtosis_levels <- c(1, 2, 3)
models <- c("fixed", "mixed")

for (skew in skewness_levels) {
  for (kurt in kurtosis_levels) {
    for (model in models) {
      sim_results <- replicate(n_simulations, 
                               simulate_and_fit_models(n = 1000, beta = 0.5, true_model = model, 
                                                       random_effects_var = 1, skewness_factor = skew, 
                                                       kurtosis_factor = kurt),
                               simplify = FALSE)
      
      # Calculate the number of correct selections
      correct_selections <- sum(sapply(sim_results, function(x) x$true_model == x$selected_model))
      
      # Append to results
      new_row <- data.frame(Skewness = skew, Kurtosis = kurt, Model = model, 
                            CorrectSelections = correct_selections, 
                            Total = n_simulations, 
                            CorrectRate = correct_selections / n_simulations,
                            stringsAsFactors = FALSE)
      results <- rbind(results, new_row)
    }
  }
}

print(results)

