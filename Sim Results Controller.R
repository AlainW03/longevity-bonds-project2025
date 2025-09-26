# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R

simulations <- 20
Bond.Proportion <- 0.75 #Controls what prop of fund gets invested into a Longevity Bond
improv.factor <- 0 # factor that messes with kappa's drift value
# It takes a value between -100 to +100. The higher (lower) the value
# the greater (smaller) the effect of the trend will be.
coupon.rate <- 0.12
# Original coupon rate was set at 0.1425
inital.members <- 100
Original.Fund <- c()
interest_rate <- 7 # value between 0 and 100
fixed_increase_rate <- 4 # value between 0 and 100
EPV.mort.risk.margin <- 7 # value between 0 and 100

# Over here I am adding a feature control
# This feature controls the coupon
# If the feature is 1, then the script will ignore the 
# coupon rate above, and set the coupon rate to the prop of what 
# the first year's liabilities are to the total fund.

Feature <- 0

# The feature was useless, since the first year's liabilities were so
# small compared to the original fund, the coupon was lower than 0.1% EVERY TIME!


timing <- system.time(source("Merged scripts with Controls.R"))

time_elapsed <- as.numeric(timing["elapsed"])

Original.Fund.Avg <- mean(Original.Fund)
result <- as.data.frame(rbind(round(simulations,digits = 0),
                              round(inital.members,digits = 0),
                              Prob.of.ruin,
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
                      "Prob of Ruin",
                      "VAR @ 95%",
                      "VAR as % of Original Fund",
                      "Prop invested in Bond",
                      "Mort Improvement Factor",
                      "Coupon Rate",
                      "Feature active?",
                      "Runtime")
View(result)
