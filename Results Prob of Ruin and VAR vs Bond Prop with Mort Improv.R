# This script is used for testing the parameters in the sims controller sheet.
# This usually regards to setting assumptions and sensitivity testing.

# Since this uses the original Sims Controller script as a template,
# I will be removing all comments and features that are already explained
# in the original sheet.

# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R

# Script relies on starting with an empty environment
rm(list = ls())

# Pre - loop settings
tests <- 10 # integer at least 1
Feature <- as.data.frame(matrix(0, ncol = length(c(0:tests)) ))

for(j in 0:3) {
Sensitivity_tests_loop_counter <- j #This is used to preserve the j value
results <- c()
for(i in 0:tests){
  
  
  # Parameters that can be changed: 
  
  simulations <- 10 #  integer at least 1
  inital.members <- 100 #  integer at least 10
  Bond.Proportion <- 0 + (100/tests)*(i) # value between 0 to 100
  improv.factor <- 0 + 50*Sensitivity_tests_loop_counter # value between -100 to +100
  coupon.rate <- 12.5 # value between 0 to 100
  interest <- 10.63 # value between 0 to 100
  fixed_increase_rate <- 5.5 # value between 0 and 100
  rate_for_discounting <- interest # value between 0 and 100
  EPV.mort.risk.margin <- 43.29 # value between 0 to 100
  reference.population.age <- 65 # integer between 16 to 120
  Fund.Monitor.Total.columns <- 70 # integer at least 60
  
  
# Targeted Parameter: Risk margin
# Target Parameter: Prob of ruin 99%

#EPV.mort.risk.margin <-  26 + i*2 


# Do not touch variables:
{

Original.Fund <- c()
Fund <- c()
FUND <- as.data.frame(matrix(0,ncol = Fund.Monitor.Total.columns))
BOND <- as.data.frame(matrix(0,ncol = Fund.Monitor.Total.columns))
timing <- system.time(source("Merged scripts with Controls.R"))
time_elapsed <- as.numeric(timing["elapsed"])
{
  FUND <- FUND[-1,]
  FUND <- FUND[,1: (max(  apply(FUND!= 0,MARGIN = 1,FUN = sum) )) ]
  colnames(FUND) <- paste0("Y",seq_along(FUND[1,]))
  rownames(FUND) <- paste0("SIM",c(1:(simulations)))
} #Cleaning up Fund Value monitor

if(Bond.Prop > 0){
  BOND <- BOND[-1,]
  BOND <- BOND[,1: (max(  apply(BOND!= 0,MARGIN = 1,FUN = sum) )) ]
  colnames(BOND) <- paste0("Y",seq_along(BOND[1,]))
  rownames(BOND) <- paste0("SIM",c(1:(simulations)))
}else{remove(BOND)} #Cleaning up Bond Value monitor

Original.Fund.Avg <- mean(Original.Fund)
results2 <- c()
results3 <- c()
}

# Crafting and cleaning result
{result <- 0



#View(result)
#View(FUND)
} 


# Results of testing
sim.and.result <- c(round(Prob.of.ruin*100, digits = 3),round((100*VAR/Original.Fund.Avg),digits = 3) )
results <- cbind(results, sim.and.result)
}

colnames(results) <- seq_along(results[1,])
rownames(results) <- c("Prob of Ruin", "VAR %")

#View(results)

# To get around using a list, I'm instead sandwiching the results
# Such that the newest Prob of ruin result gets added on top of the
# previous results, and the newest VAR result gets added to the bottom
# of the previous result. The row of 0's will be an indicator dividing
# the inversed Prob of ruins and the normal VAR results.

Feature <- rbind(results[1,], Feature, results[2,])

}


# Now that we have our results, let's decompose the dataset into the appropriate 
# results

result.rows <- length(c(0:Sensitivity_tests_loop_counter))
result.columns <- length(0:tests)

Bond.props <- 0 + (100/tests)*(0:tests)
Improv.factors <- 0 + 50*(0:Sensitivity_tests_loop_counter)

Result.Prob.of.Ruin <- Feature[rev(1:result.rows),]
Result.VAR.as.percentage <- Feature[(result.rows+2):nrow(Feature),]

colnames(Result.Prob.of.Ruin) <- paste0("Bond Prop ", Bond.props, "%")
colnames(Result.VAR.as.percentage) <- paste0("Bond Prop ", Bond.props, "%")

rownames(Result.Prob.of.Ruin) <- paste0("Improv ", Improv.factors, "%")
rownames(Result.VAR.as.percentage) <- paste0("Improv ", Improv.factors, "%")

# Plotting results
{#plot(x = results[1,]*100, y = results[3,], type = "n",
#     main = "Prob of ruin and Var vs Proportion invested into Longevity Bond",
#     xlab = "Bond Proportion %",
#     ylab = "%",
#     ylim = c(0,max(results)))
#lines(x = results[1,]*100, y = results[2,], col = "blue", lty = 1)
#lines(x = results[1,]*100, y = results[3,], col = "red", lty = 2)
#legend("center",
#       legend = c("VAR as %", "Prob of Ruin"),
#       col = c( "red","blue"),
#       lty = c(2,1),
#       cex = 0.6,
#       bty = "n")
}