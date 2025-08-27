# So to calculate the VAR and Prob of ruin, there is actually a very simple
# approach.

# Explaining approach
{
  # VAR
  # The VAR will be calculated on the 95% percentile of all the 
  # possible losses the fund has made.
  
  # The loss of one simulation is the difference between the 
  # initial fund value, and the final fund value.
  
  # So what we need is a vector that records all the losses of the fund
  # and the 95% highest loss will be our VAR.
  
  
  # Prob of ruin
  # This is even easier. "Ruin" is if the fund decreases to a value below 0
  # in each simulation. That means we only need a vector of 1's and 0's
  # that records if the final fund value is below 0.
  
}
  
# Creating empty loss and ruin vectors
loss <- c()
ruin <- c()

# Parameter controlling the amount of simulations

sim <- 100

# Creating loop to run multiple simulations

for(l in 1:sim) {
  
  # Creating a dummy fund calculation
  {
  Fund <- c(sample(c(90:115), size = 1))
  years <- 10
  for(k in 2:years){ 
    Fund <- c(Fund, (Fund[k-1] - runif(1,10,14))) }
  }
  
  # Recording ruin and loss for the l th simulation
  loss <- c(loss, Fund[1] - Fund[length(Fund)])
  ruin <- c(ruin, ifelse(Fund[length(Fund)] <= 0,1,0))
  
}

# And now for the VAR@85% conf and Prob of Ruin calculation

VAR <- as.numeric(quantile(loss, prob = 0.95))
Prob.of.ruin <- mean(ruin)

