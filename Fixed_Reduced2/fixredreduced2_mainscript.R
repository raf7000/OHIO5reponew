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

set.seed(123)  
ni <- 30      
m <- 30      
beta <- c(1, 0, 1)  
random_effects_var <- 1

num_sims_list <- c(10, 100, 500)

num_sims_results <- run_simulations_with_user_defined_sims(num_sims_list)
saveRDS(num_sims_results, file = "num_sims_simulation_results_list.rds")



