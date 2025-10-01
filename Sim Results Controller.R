# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R

simulations <- 100
Bond.Proportion <- 0 #Controls what prop of fund gets invested into a Longevity Bond
improv.factor <- -150 # factor that messes with kappa's drift value
# It takes a value between -100 to +100. The higher (lower) the value
# the greater (smaller) the effect of the trend will be.

coupon.rate <- 12.5 # value between 0 and 100
{
  # The Prob of Ruin varies between 12% and 0% between
# a coupon rate of 0.1225 and 0.1275
# That is with:
#Bond.Proportion <- 100 #Controls what prop of fund gets invested into a Longevity Bond
#improv.factor <- 0 
#interest <- 7 
#fixed_increase_rate <- 4
#EPV.mort.risk.margin <- 10
#reference.population.age <- 65
# Original coupon rate was set at 0.1425
}# Playing with the coupon rate 

inital.members <- 100
Original.Fund <- c()
reference.population.age <- 65

# I think I got confused with the rate of return, and the discount rate

interest <- 7 # value between 0 and 100
fixed_increase_rate <- 4 # value between 0 and 100
rate_for_discounting <- interest # value between 0 and 100
EPV.mort.risk.margin <- 10 # value between 0 and 100
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
  Total.columns <- 70
  
  FUND <- as.data.frame(matrix(0,ncol = Total.columns))
  
  
  
}

timing <- system.time(source("Merged scripts with Controls.R"))

time_elapsed <- as.numeric(timing["elapsed"])

{
  FUND <- FUND[-1,]
  FUND <- FUND[,1: (max(  apply(FUND!= 0,MARGIN = 1,FUN = sum) )) ]
  colnames(FUND) <- paste0("Y",seq_along(FUND[1,]))
  rownames(FUND) <- paste0("SIM",c(1:(simulations)))
} #Cleaning up Fund Value monitor

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

View(result)
#View(FUND)
