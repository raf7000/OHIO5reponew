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
num_of_trials = 20000 #setting a large number of trials 
estimated_e = e_estimation(num_of_trials) 
print(estimated_e)





 





