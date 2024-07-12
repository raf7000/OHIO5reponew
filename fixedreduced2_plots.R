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
  
  # violin plot
  bics_long <- pivot_longer(all_results, cols = c(bic_full, bic_reduced1, bic_reduced2, bic_reduced3), 
                            names_to = "model", values_to = "BIC")
  
  violin_plot <- ggplot(bics_long, aes(x = true_model, y = BIC, fill = model)) +
    geom_violin() +
    facet_wrap(~ BIC_definition) +
    theme_minimal() +
    labs(title = paste("BIC Distribution by True Model and Fitted Model -", name),
         x = "True Model", y = "BIC Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  violin_plots[[name]] <- violin_plot
  
  # bar plot
  bar_plot <- ggplot(all_results, aes(x = true_model, fill = selected_model)) +
    geom_bar(position = "fill") +
    facet_wrap(~ BIC_definition) +
    theme_minimal() +
    labs(title = paste("Model Selection Proportions by True Model -", name),
         x = "True Model", y = "Proportion",
         fill = "Selected Model") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  bar_plots[[name]] <- bar_plot
  
  # Print summary
  summary_table <- all_results %>%
    group_by(BIC_definition, true_model, selected_model) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = selected_model, values_from = count, values_fill = 0)
  
  print(paste("Summary for", name))
  print(summary_table)
}

# Combine and save violin plots
combined_violin_plot <- wrap_plots(violin_plots) + plot_layout(ncol = 1)
ggsave("combined_violin_plot.png", plot = combined_violin_plot, width = 12, height = 12)

# Combine and save bar plots
combined_bar_plot <- wrap_plots(bar_plots) + plot_layout(ncol = 1)
ggsave("combined_bar_plot.png", plot = combined_bar_plot, width = 10, height = 12)


