# Load functions
source("fixedreduced2_functions.R")

# Parameters for the simulation
set.seed(123)
num_simulations <- 100
m <- 30
beta <- c(1, 0, 1)
random_effects_var <- 1
subject_numbers <- c(10, 30, 50)

# Run simulations and save results
results <- run_simulations_for_subject_numbers(subject_numbers)
saveRDS(results, file = "simulation_results_list.rds")




