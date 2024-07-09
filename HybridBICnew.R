library(lme4)
library(MASS)
library(parallel)
library(ggplot2)
library(tidyr)
library(dplyr)

# Hybrid BIC function (unchanged)
hybrid_BIC <- function(model) {
  logLik_val <- as.numeric(logLik(model))
  n_fixed <- length(fixef(model))
  n_random <- sum(sapply(VarCorr(model), function(x) prod(dim(x))))
  N <- nrow(model@frame)
  m <- nlevels(model@frame$group_id)
  BIC_value <- -2 * logLik_val + n_fixed * log(N) + n_random * log(m)
  return(BIC_value)
}

# Updated simulation function
simulate_and_fit_models <- function(ni, m, beta, random_effects_var, true_model, noise_level = 1) {
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
  
  y <- y + 0.5 * x1^2 + 0.5 * sin(x2) + 0.5 * x1 * x2
  
  outlier_indices <- sample(1:(ni*m), size = round(0.01 * ni * m))
  y[outlier_indices] <- y[outlier_indices] + rnorm(length(outlier_indices), 0, 5)
  
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
  
  return(data.frame(
    true_model = true_model,
    selected_model = selected_model,
    bic_full = bic_full,
    bic_reduced1 = bic_reduced1,
    bic_reduced2 = bic_reduced2,
    bic_reduced3 = bic_reduced3
  ))
}

# Run simulations for all permutations
run_all_simulations <- function(num_simulations, ni, m, beta, random_effects_var, noise_level) {
  true_models <- c("full", "reduced1", "reduced2", "reduced3")
  
  results <- lapply(true_models, function(true_model) {
    do.call(rbind, mclapply(1:num_simulations, function(i) {
      simulate_and_fit_models(ni, m, beta, random_effects_var, true_model, noise_level)
    }, mc.cores = detectCores()))
  })
  
  do.call(rbind, results)
}

# Run simulations
set.seed(123)
num_simulations <- 100
ni <- 10
m <- 30
beta <- c(1, 1, 1)
random_effects_var <- 3
noise_level <- 2

all_results <- run_all_simulations(num_simulations, ni, m, beta, random_effects_var, noise_level)

# Create violin plot
ggplot(pivot_longer(all_results, cols = starts_with("bic_"), names_to = "model", values_to = "BIC"),
       aes(x = true_model, y = BIC, fill = model)) +
  geom_violin() +
  facet_wrap(~ model) +
  theme_minimal() +
  labs(title = "BIC Distribution by True Model and Fitted Model",
       x = "True Model", y = "BIC Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("violin_plot.png", width = 12, height = 8)

# Create bar plot
ggplot(all_results, aes(x = true_model, fill = selected_model)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Model Selection Proportions by True Model",
       x = "True Model", y = "Proportion",
       fill = "Selected Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("bar_plot.png", width = 10, height = 6)

# Print summary
summary_table <- all_results %>%
  group_by(true_model, selected_model) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = selected_model, values_from = count, values_fill = 0)

print(summary_table)
