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
  
  # Fit models
  model_full <- lmer(y ~ x1 + x2 + x3 + (1|group_id), data = data)
  model_reduced1 <- lmer(y ~ x1 + x2 + (1|group_id), data = data)
  model_reduced2 <- lmer(y ~ x1 + x3 + (1|group_id), data = data)
  model_reduced3 <- lmer(y ~ x2 + x3 + (1|group_id), data = data)
  
  # Calculate BIC values
  bic_full <- bic_func(model_full)
  bic_reduced1 <- bic_func(model_reduced1)
  bic_reduced2 <- bic_func(model_reduced2)
  bic_reduced3 <- bic_func(model_reduced3)
  
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
run_all_simulations <- function(num_simulations, ni, m, beta, random_effects_var, noise_level, bic_func) {
  true_models <- c("full", "reduced1", "reduced2", "reduced3")
  
  results <- lapply(true_models, function(true_model) {
    do.call(rbind, mclapply(1:num_simulations, function(i) {
      simulate_and_fit_models(ni, m, beta, random_effects_var, true_model, noise_level, bic_func)
    }, mc.cores = detectCores()))
  })
  
  do.call(rbind, results)
}

# Parameters for the simulation
set.seed(123)
num_simulations <- 100
ni <- 10
m <- 30
beta <- c(1, 0, 1)
random_effects_var <- 3
noise_level <- 2

# Run simulations for each BIC definition
bic_functions <- list(bic_balding, bic_pinheiro, bic_lai_gao)
bic_names <- c("Balding", "Pinheiro", "Lai_Gao")

all_results <- lapply(seq_along(bic_functions), function(i) {
  cat("Running simulations for", bic_names[i], "BIC definition...\n")
  results <- run_all_simulations(num_simulations, ni, m, beta, random_effects_var, noise_level, bic_functions[[i]])
  results$BIC_definition <- bic_names[i]
  return(results)
})

all_results <- do.call(rbind, all_results)

# Create violin plot
ggplot(pivot_longer(all_results, cols = starts_with("bic_"), names_to = "model", values_to = "BIC"),
       aes(x = true_model, y = BIC, fill = model)) +
  geom_violin() +
  facet_wrap(~ BIC_definition) +
  theme_minimal() +
  labs(title = "BIC Distribution by True Model and Fitted Model",
       x = "True Model", y = "BIC Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("violin_plot.png", width = 12, height = 8)

# Create bar plot
ggplot(all_results, aes(x = true_model, fill = selected_model)) +
  geom_bar(position = "fill") +
  facet_wrap(~ BIC_definition) +
  theme_minimal() +
  labs(title = "Model Selection Proportions by True Model",
       x = "True Model", y = "Proportion",
       fill = "Selected Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("bar_plot.png", width = 10, height = 6)

# Print summary
summary_table <- all_results %>%
  group_by(BIC_definition, true_model, selected_model) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = selected_model, values_from = count, values_fill = 0)

print(summary_table)
