library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

# Load simulation results
results_list <- readRDS("simulation_results_list.rds")

# Initialize lists to store plots
violin_plots <- list()
bar_plots <- list()

for (name in names(results_list)) {
  all_results <- results_list[[name]]
  
  # Convert BIC columns to long format for plotting
  bics_long <- pivot_longer(all_results, cols = starts_with("bic_"), 
                            names_to = "BIC_definition", values_to = "BIC")
  
  # Convert selected model columns to long format for plotting
  selected_models_long <- pivot_longer(all_results, cols = starts_with("selected_model_"), 
                                       names_to = "BIC_definition", values_to = "selected_model")
  
  # Violin plot for BIC values
  violin_plot <- ggplot(bics_long, aes(x = model_name, y = BIC, fill = model_name)) +
    geom_violin() +
    facet_wrap(~ BIC_definition) +
    theme_minimal() +
    labs(title = paste("BIC Distribution by True Model and Fitted Model -", name),
         x = "Model", y = "BIC Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(0, 0, 0, 0))
  
  violin_plots[[name]] <- violin_plot
  
  # Bar plot for selected models
  bar_plot <- ggplot(selected_models_long, aes(x = true_model, fill = selected_model)) +
    geom_bar(position = "fill", width = 1) +  # Make sure bars are adjacent
    facet_wrap(~ BIC_definition) +
    theme_minimal() +
    labs(title = paste("Model Selection Proportions by True Model -", name),
         x = "True Model", y = "Proportion",
         fill = "Selected Model") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(0, 0, 0, 0))
  
  bar_plots[[name]] <- bar_plot
  
  # Print summary
  summary_table <- selected_models_long %>%
    group_by(BIC_definition, true_model, selected_model) %>%
    summarise(count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = selected_model, values_from = count, values_fill = list(count = 0))
  
  print(paste("Summary for", name))
  print(summary_table)
}

# Combine all violin plots using patchwork
combined_violin_plot <- wrap_plots(violin_plots, ncol = 1) & theme(plot.margin = margin(0, 0, 0, 0))
combined_bar_plot <- wrap_plots(bar_plots, ncol = 1) & theme(plot.margin = margin(0, 0, 0, 0))

# Display combined plots
print(combined_violin_plot)
print(combined_bar_plot)

