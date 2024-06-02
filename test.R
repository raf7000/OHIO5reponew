#monte carlo estimating e 

e_estimation = function(num_of_trials) {
  counts = numeric(num_of_trials)
  for (i in 1:num_of_trials) {
    sum = 0
    count = 0 
    
    while (sum <= 1) {
      sum = sum + runif(1)
      count <- count + 1
    }
    
    counts[i] <- count
  }
  mean(counts)  # The average count should converge to e
}


 





