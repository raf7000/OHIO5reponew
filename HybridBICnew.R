library(lme4)
library(MASS)
library(parallel)

# Function to calculate hybrid BIC (unchanged)
hybrid_BIC <- function(model) {
  logLik_val <- as.numeric(logLik(model))
  n_fixed <- length(fixef(model))
  n_random <- sum(sapply(VarCorr(model), function(x) prod(dim(x))))
  N <- nrow(model@frame)
  m <- nlevels(model@frame$group_id)
  BIC_value <- -2 * logLik_val + n_fixed * log(N) + n_random * log(m)
  return(BIC_value)
}

# Function to simulate data and fit models
simulate_and_fit_models <- function(ni, m, beta, random_effects_var, true_model, noise_level = 1) {
  # Simulate data
  group_id <- rep(1:m, each = ni)
  x1 <- rnorm(ni * m)
  x2 <- rnorm(ni * m)
  x3 <- rnorm(ni * m)
  random_effect <- rep(rnorm(m, 0, sqrt(random_effects_var)), each = ni)
  
  # Increase noise level in epsilon
  epsilon <- rnorm(ni * m, 0, noise_level)
  
  # Generate y based on the true model
  y <- random_effect + epsilon  # Start with random effect and increased noise
  if (true_model == "full" || true_model == "reduced1" || true_model == "reduced2") y <- y + beta[1] * x1
  if (true_model == "full" || true_model == "reduced1" || true_model == "reduced3") y <- y + beta[2] * x2
  if (true_model == "full" || true_model == "reduced2" || true_model == "reduced3") y <- y + beta[3] * x3
  
  # Add non-linear terms
  y <- y + 0.5 * x1^2 + 0.5 * sin(x2)
  
  # Add interaction term
  y <- y + 0.5 * x1 * x2
  
  # Add outliers (replace 1% of the data with extreme values)
  outlier_indices <- sample(1:(ni*m), size = round(0.01 * ni * m))
  y[outlier_indices] <- y[outlier_indices] + rnorm(length(outlier_indices), 0, 5)
  
  # Create data frame
  data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, group_id = factor(group_id))
  
  # Fit models
  model_full <- lmer(y ~ x1 + x2 + x3 + (1|group_id), data = data)
  model_reduced1 <- lmer(y ~ x1 + x2 + (1|group_id), data = data)
  model_reduced2 <- lmer(y ~ x1 + x3 + (1|group_id), data = data)
  model_reduced3 <- lmer(y ~ x2 + x3 + (1|group_id), data = data)
  
  # Calculate BIC values
  bic_full <- hybrid_BIC(model_full)
  bic_reduced1 <- hybrid_BIC(model_reduced1)
  bic_reduced2 <- hybrid_BIC(model_reduced2)
  bic_reduced3 <- hybrid_BIC(model_reduced3)
  
  # Select best model
  bics <- c(bic_full, bic_reduced1, bic_reduced2, bic_reduced3)
  model_names <- c("full", "reduced1", "reduced2", "reduced3")
  selected_model <- model_names[which.min(bics)]
  
  return(list(true_model = true_model, 
              selected_model = selected_model, 
              bic_full = bic_full, 
              bic_reduced1 = bic_reduced1, 
              bic_reduced2 = bic_reduced2,
              bic_reduced3 = bic_reduced3))
}

# Function to run multiple simulations
run_simulations <- function(num_simulations, ni, m, beta, random_effects_var, true_model, noise_level) {
  results <- mclapply(1:num_simulations, function(i) {
    simulate_and_fit_models(ni, m, beta, random_effects_var, true_model, noise_level)
  }, mc.cores = detectCores())
  return(results)
}

# Example usage
set.seed(123)
num_simulations <- 100
ni <- 10
m <- 30
beta <- c(1, 1, 1)  # One coefficient for each predictor
random_effects_var <- 3
true_model <- "reduced1"  # We decide this is our true model for this simulation
noise_level <- 2  # Increased noise level

simulation_results <- run_simulations(num_simulations, ni, m, beta, random_effects_var, true_model, noise_level)

# Analyze results
model_selection_counts <- table(sapply(simulation_results, function(x) x$selected_model))
print("Model selection counts:")
print(model_selection_counts)

correct_selections <- sum(sapply(simulation_results, function(x) x$selected_model == x$true_model))
print(paste("Correct selections:", correct_selections, "out of", num_simulations))
print(paste("Accuracy:", correct_selections / num_simulations))

# Calculate average BIC values
avg_bic_full <- mean(sapply(simulation_results, function(x) x$bic_full))
avg_bic_reduced1 <- mean(sapply(simulation_results, function(x) x$bic_reduced1))
avg_bic_reduced2 <- mean(sapply(simulation_results, function(x) x$bic_reduced2))
avg_bic_reduced3 <- mean(sapply(simulation_results, function(x) x$bic_reduced3))

print("Average BIC values:")
print(data.frame(
  full = avg_bic_full,
  reduced1 = avg_bic_reduced1,
  reduced2 = avg_bic_reduced2,
  reduced3 = avg_bic_reduced3
))
