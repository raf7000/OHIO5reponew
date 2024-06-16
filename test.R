#monte carlo estimating e 

e_estimation = function(num_of_trials) { #defining function to estimate e 
  counts = numeric(num_of_trials) #defining the number of counts as the number of trials 
  cumulative_averages <- numeric(num_of_trials) #defining cumulative averages 
  for (i in 1:num_of_trials) { #setting a for loop 
    sum = 0 #initializing sum 
    count = 0 #initializing count 
    
    while (sum <= 1) { #if the sum <= 1, we will add a number between 0 and 1 to the sum 
      sum = sum + runif(1)
      count <- count + 1 #incrementing count
    }
    
    counts[i] <- count
    cumulative_averages[i] <- mean(counts[1:i]) #cum average is the mean of the counts 
  }
    plot(cumulative_averages, type = 'l', col = 'blue', lwd = 2, #plotting graph to see estimated e's behaviour 
         xlab = "Number of Trials", ylab = "Estimated Value of e",
         main = "Convergence of Monte Carlo Estimation of e")
    abline(h = exp(1), col = 'red', lwd = 2, lty = 2)
    legend("bottomright", legend = c("Estimated e", "Actual e"), col = c("blue", "red"), lty = 1:2, lwd = 2)
    
    return(cumulative_averages[num_of_trials])  # Returning the final estimate of e
}
  

set.seed(123) #for reproducility 
num_of_trials = 10000 #setting a large number of trials 
estimated_e = e_estimation(num_of_trials) 
print(estimated_e)


#t test simulation example 
n_sims <- 10000
n1 <- 400
n2 <- 600
true_diff = 0.5

p_values <- numeric(n_sims)
test_stats <- numeric(n_sims)
estimates <- numeric(n_sims)
rejections <- logical(n_sims)

for(i in 1:n_sims){
  
  #data being produced randomly from two different normal distributions 
  x1 <- rnorm(n1, mean = 0, sd = 1)
  x2 <- rnorm(n2, mean = 0.5, sd = 1)
  
  test_result <- t.test(x1, x2)
  
  p_values[i] <- test_result$p.value
  test_stats[i] <- test_result$statistic
  estimates[i] <- test_result$estimate[2] - test_result$estimate[1] 
  rejections[i] <- test_result$p.value < 0.05
  
}

bias <- mean(estimates - true_diff)

TPR <- mean(rejections)

mean_p_value <- mean(p_values)
mean_test_stat <- mean(test_stats)
power <- mean(p_values < 0.05)

print(mean_p_value)
print(mean_test_stat)
print(power)
print(TPR)
print(bias) 



