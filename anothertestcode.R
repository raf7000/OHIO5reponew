library(lme4)
library(MASS)
library(ggplot2)
library(dplyr)

simulate_and_fit_models <- function(ni, m, beta, true_model, random_effects_var, num_simulations = 100) {
  results <- data.frame(model = character(), BIC = numeric(), correct = logical(), stringsAsFactors = FALSE)
  
  for (i in 1:num_simulations) {
    n <- ni * m
    X <- rnorm(n)
    group_effect <- if (true_model == "mixed") rnorm(m, mean = 0, sd = sqrt(random_effects_var)) else rep(0, m)
    epsilon <- rnorm(n)
    y <- X * beta + rep(group_effect, each = ni) + epsilon
    
    data <- data.frame(y = y, X = X, group_id = as.factor(rep(1:m, each = ni)))
    model_fixed <- lm(y ~ X - 1, data = data)
    model_mixed <- lmer(y ~ X - 1 + (1 | group_id), data = data)
    
    bic_fixed <- BIC(model_fixed)
    bic_mixed <- BIC(model_mixed)
    
    selected_model <- ifelse(bic_fixed < bic_mixed, "fixed", "mixed")
    is_correct <- selected_model == true_model
    
    results <- rbind(results, data.frame(model = "fixed", BIC = bic_fixed, correct = selected_model == "fixed" & true_model == "fixed"))
    results <- rbind(results, data.frame(model = "mixed", BIC = bic_mixed, correct = selected_model == "mixed" & true_model == "mixed"))
  }
  
  return(results)
}

set.seed(123)
results <- simulate_and_fit_models(ni = 10, m = 30, beta = 1, true_model = "mixed", random_effects_var = 0.1, num_simulations = 100)

# Print results
print(results)

# Bar plot for correct model selection counts
correct_counts <- results %>%
  group_by(model, correct) %>%
  summarize(count = n())

ggplot(correct_counts, aes(x = model, y = count, fill = correct)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Correct vs. Incorrect Model Selections",
       x = "Model Type",
       y = "Count") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_minimal()

# Violin plot for BIC values
ggplot(results, aes(x = model, y = BIC, fill = correct)) +
  geom_violin() +
  labs(title = "Violin Plot of BIC Values for Fixed and Mixed Models",
       x = "Model Type",
       y = "BIC Value") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_minimal()

results