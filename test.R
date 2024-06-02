#monte carlo estimating e 

e_estimation = function(num_of_trials) { #defining function to estimate e 
  counts = numeric(num_of_trials) #defining the number of counts as the number of trials 
  for (i in 1:num_of_trials) { #setting a for loop 
    sum = 0 #initializing sum 
    count = 0 #initializing count 
    
    while (sum <= 1) { #if the sum <= 1, we will add a number between 0 and 1 to the sum 
      sum = sum + runif(1)
      count <- count + 1 #incrementing count
    }
    
    counts[i] <- count
  }
  mean(counts)  #The average count should converge to e
}

set.seed(123) #for reproducility 
num_of_trials = 100000 #setting a large number of trials 
estimated_e = e_estimation(num_of_trials) 
print(estimated_e)





 





