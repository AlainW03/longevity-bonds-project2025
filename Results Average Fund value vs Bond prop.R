# This script is used for testing the parameters in the sims controller sheet.
# This usually regards to setting assumptions and sensitivity testing.

# Since this uses the original Sims Controller script as a template,
# I will be removing all comments and features that are already explained
# in the original sheet.

# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R

# Pre - loop settings
tests <- 1 # integer at least 1
results <- c()

for(i in 1:tests){
  
  
  # Parameters that can be changed: 
  
  simulations <- 200 #  integer at least 1
  inital.members <- 100 #  integer at least 10
  Bond.Proportion <- 0 # value between 0 to 100
  improv.factor <- 0 # value between -100 to +100
  coupon.rate <- 12.5 # value between 0 to 100
  interest <- 10.63 # value between 0 to 100
  fixed_increase_rate <- 5.5 # value between 0 and 100
  rate_for_discounting <- interest # value between 0 and 100
  EPV.mort.risk.margin <- 43.29 # value between 0 to 100
  reference.population.age <- 65 # integer between 16 to 120
  Fund.Monitor.Total.columns <- 70 # integer at least 60
  
  Sensitivity_tests_loop_counter <- i
# Targeted Parameter: Risk margin
# Target Parameter: Prob of ruin 99%

#EPV.mort.risk.margin <-  26 + i*2 


# Do not touch variables:
{
Feature <- 0 # Should remain 0
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
{result <- as.data.frame(rbind(round(simulations,digits = 0),
                              round(inital.members,digits = 0),
                              round(Prob.of.ruin*100, digits = 3),
                              round(VAR,digits = 3),
                              round((100*VAR/Original.Fund.Avg),digits = 3),
                              Bond.Proportion,
                              improv.factor,
                              coupon.rate,
                              ifelse(Feature==1, "Yes","No"),
                              round(time_elapsed,digits = 3)))

colnames(result) <- "Results"
rownames(result) <- c("Simulations",
                      "Initial Nr of Members",
                      "Prob of Ruin %",
                      "VAR @ 95%",
                      "VAR as % of Original Fund",
                      "Prop invested in Bond",
                      "Mort Improvement Factor",
                      "Coupon Rate %",
                      "Feature active?",
                      "Runtime")


#View(result)
#View(FUND)
} 


# Results of testing
sim.and.result <- c(EPV.mort.risk.margin, round(Prob.of.ruin*100, digits = 3),round((100*VAR/Original.Fund.Avg),digits = 3) )
results <- cbind(results, sim.and.result)
}

colnames(results) <- seq_along(results[1,])
rownames(results) <- c("Risk Margin", "Prob of Ruin", "VAR %")

#View(results)

# The next part was me overthinking the average fund
{
# Averaging the Fund values over the years, making special allowance for
# exluding zeros

#Sum.Fund.Value.per.year <- apply(FUND, MARGIN = 2, FUN = sum)
#Logic.for.non.zero <- apply(FUND != 0 , MARGIN = 2, FUN = sum)
#Avg.Fund.Value <- as.data.frame(Sum.Fund.Value.per.year/Logic.for.non.zero)
#rownames(Avg.Fund.Value) <- paste0("Y",seq_along(Avg.Fund.Value[,1]))
#colnames(Avg.Fund.Value) <- c(paste(c("Average Fund value with ", Bond.Prop*100,"% Bond Prop"), collapse = ""))

#plot(x = seq_along(Avg.Fund.Value[,1]), y = Avg.Fund.Value[,1], type = "l")

# So the plot looks horrible, since the latter years dominate the values and 
# grow exponentially. Rather, let's find the closest representing Fund
# value among the simulations:


# Compute the Euclidean distance between each row and the average vector
#avg_vector <- Avg.Fund.Value[-c(40:nrow(Avg.Fund.Value)),1]
#distances <- apply(FUND[,-c(40:ncol(FUND))], 1, function(row) sqrt(sum((row - avg_vector)^2)))

# Find the index of the row with the smallest distance
#closest_row_index <- which.min(distances)

# Extract the closest row
#closest_row <- FUND[closest_row_index, ]

#plot(x = seq_along(closest_row), y = closest_row, type = "l")
#plot(x = seq_along(FUND[which.max(distances), ]), y = FUND[which.max(distances), ], type = "l")
}

# Here is the average fund dataset
avg.fund <- as.data.frame(apply(FUND, MARGIN = 2, FUN = mean))
rownames(avg.fund) <- paste0("Y",seq_along(avg.fund[,1]))
colnames(avg.fund) <- c(paste(c("Average Fund value with ", Bond.Prop*100,"% Bond Prop"), collapse = ""))
plot(x = seq_along(avg.fund[,1]), y = avg.fund[,1], type = "l", xlab = "Years", ylab = "Avg Fund Value", main = paste("Average Fund Value over the years of the simulation with ", Bond.Prop*100,"% of the starting fund invested into the Longevity Bond"))


#Optionally you can also look at the simulation that has values that was the 
# closest to the average fund values
{
# And the closest row to that?

#avg_vector <- avg.fund
#distances <- apply(FUND, 1, function(row) sqrt(sum((row - avg_vector)^2)))

# Find the index of the row with the smallest distance
#closest_row_index <- which.min(distances)

# Extract the closest row
#closest_row_normal_avg <- FUND[closest_row_index, ]

#plot(x = seq_along(closest_row_normal_avg), y = closest_row_normal_avg, type = "l")
}