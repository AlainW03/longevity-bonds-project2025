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
  
  simulations <- 118 #  integer at least 1
  inital.members <- 100 #  integer at least 10
  Bond.Proportion <- 50 # value between 0 to 100
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




# To calculate the average liabilities without making significant changes
# to the main model, we can actually reverse calculate it.

# After a bunch of testing, this formula below can accurately retrace the 
# liability cashflows overtime

benefit.values <- c(0)

for(i in 2:(ncol(benefit.base)-3)){
  
  benefit.values[i] <- (Fund[i-1])  * (1+(interest * ifelse(Fund[i-1] > 0, 1, 0)/100)) - Fund[i]  + longevity_bonds_cashflows[i-1]
  
  
}

benefit.values <- benefit.values[-1]


# So let's scale this calculation up to the level of FUND and BOND

BENEFIT.VALUES <- cbind(0,FUND*0)

for(i in 2:(ncol(BENEFIT.VALUES)-1)){
  
  BENEFIT.VALUES[,i] <- (FUND[,i-1])  * (1+(interest * ifelse(FUND[,i-1] > 0, 1, 0)/100)) - FUND[,i]  + BOND[,i-1]
  
  
}

BENEFIT.VALUES <- BENEFIT.VALUES[,-1]


# Just one small tweak as to stop the benefits at the appropriate point

for(a in 1:nrow(BENEFIT.VALUES)){
  for(b in 1:ncol(BENEFIT.VALUES)){
    BENEFIT.VALUES[a,b] <- max(BENEFIT.VALUES[a,b],0)
  }
}

for(a in 1:nrow(BENEFIT.VALUES)){
  for(b in 26:ncol(BENEFIT.VALUES)){
    current.value <- BENEFIT.VALUES[a,b]
    previous.value <- BENEFIT.VALUES[a,b-1]
    if(previous.value>0){
    change <- (current.value/previous.value) - 1
    real.change <- fixed_increase_rate*1.5
    if(change > real.change){
    BENEFIT.VALUES[a,b] <- 0
    }
  }
}
}

# A quick check with a forward calculation implies the values are correct...

# I can't believe this worked LOL

avg.benefits <- as.data.frame(apply(BENEFIT.VALUES, MARGIN = 2, FUN = mean))
rownames(avg.benefits) <- paste0("Y",seq_along(avg.benefits[,1]))
colnames(avg.benefits) <- "Average Liabilities per year"
plot(x = seq_along(avg.benefits[,1]), y = avg.benefits[,1], type = "l", xlab = "Years", ylab = "Avg Liability Value", main = "Average Liabilities over the entire simulation" )
