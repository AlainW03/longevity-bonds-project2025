# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R

simulations <- 10
Bond.Proportion <- 100 #Controls what prop of fund gets invested into a Longevity Bond
improv.factor <- 50 # factor that messes with kappa's drift value
# It takes a value between -100 to +100. The higher (lower) the value
# the greater (smaller) the effect of the trend will be.
results <- c()
tests <- 10
for(i in 1:tests) {
coupon.rate <- 12.44 + i/50
{


}# Playing with the coupon rate 

inital.members <- 100
Original.Fund <- c()
reference.population.age <- 65
Sensitivity_tests_loop_counter <- i

# I think I got confused with the rate of return, and the discount rate

interest <- 10.63  # value between 0 and 100, sweetspot is around 22
# select mortality risk is reflected in the interest rate. Since, say you have
# 1000 members, then your select mortality risk is small. You'll see that your i
# can be anything and it will barely have an affect with 1000 members (PoR of 50%)
fixed_increase_rate <- 5.5 # value between 0 and 100
rate_for_discounting <- interest # value between 0 and 100
EPV.mort.risk.margin <- 43.29 # value between 0 and 100, allows for trend in EPV
# 45 is the best estimate, with a 50% PoR with 1000 members (which theoretically 
#restricts select risk)
Fund <- c()
{
  # Over here I am adding a feature control
  # This feature controls the coupon
  # If the feature is 1, then the script will ignore the 
  # coupon rate above, and set the coupon rate to the prop of what 
  # the first year's liabilities are to the total fund.
  
  Feature <- 0
  # The feature was useless, since the first year's liabilities were so
  # small compared to the original fund, the coupon was lower than 0.1% EVERY TIME!
} # This is the useless feature

# Creating Fund value monitor
{
  Fund.Monitor.Total.columns <- 70
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
  
}
Original.Fund.Avg <- mean(Original.Fund)
result <- as.data.frame(rbind(round(simulations,digits = 0),
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

prob.plus.coupon <- c(coupon.rate, Prob.of.ruin)
results <- cbind(results, prob.plus.coupon)
}
View(result)
#View(FUND)
