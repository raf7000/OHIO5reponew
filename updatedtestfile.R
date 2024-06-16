library(ggplot2)

set.seed(123) 

simulate_t_tests <- function(n_sims, n1, n2, true_diff) {
  p_values <- numeric(n_sims)
  test_stats <- numeric(n_sims)
  estimates <- numeric(n_sims)
  rejections <- logical(n_sims)
  
  for (i in 1:n_sims) {
    x1 <- rnorm(n1, mean = 0, sd = 1)
    x2 <- rnorm(n2, mean = 0.5, sd = 1)
    
    test_result <- t.test(x1, x2)
    
    p_values[i] <- test_result$p.value
    test_stats[i] <- test_result$statistic
    estimates[i] <- test_result$estimate[2] - test_result$estimate[1] 
    rejections[i] <- test_result$p.value < 0.05
  }
  
  data.frame(
    p_values = p_values,
    test_stats = test_stats,
    estimates = estimates,
    rejections = rejections,
    bias = estimates - true_diff
  )
}

simulation_data <- simulate_t_tests(n_sims = 10000, n1 = 400, n2 = 600, true_diff = 0.5)

p_value_plot <- ggplot(simulation_data, aes(x = p_values)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of P-values", x = "P-value", y = "Frequency")

estimate_plot <- ggplot(simulation_data, aes(x = estimates)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Estimates", x = "Estimate", y = "Frequency")

scatter_plot <- ggplot(simulation_data, aes(x = test_stats, y = estimates, color = rejections)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("red", "blue"), labels = c("Rejected", "Not Rejected")) +
  labs(title = "Scatter Plot of Test Statistics vs. Estimates",
       x = "Test Statistic", y = "Estimate", color = "Hypothesis Test Result")

print(p_value_plot)
print(estimate_plot)



