# This script is used for testing the parameters in the sims controller sheet.
# This usually regards to setting assumptions and sensitivity testing.

# Since this uses the original Sims Controller script as a template,
# I will be removing all comments and features that are already explained
# in the original sheet.

# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R


# Parameters that can be changed: 
simulations <- 10 # integer at least 1
Bond.Proportion <- 0 # value between 0 to 100
improv.factor <- 0 # value between -100 to +100
results <- c()
coupon.rate <- 12.5 # value between 0 to 100
interest <- 22 # value between 0 to 100
fixed_increase_rate <- 4 # value between 0 and 100
rate_for_discounting <- interest # value between 0 and 100
EPV.mort.risk.margin <- 45 # value between 0 to 100
inital.members <- 1000 # integer at least 10
reference.population.age <- 65 # integer between 16 to 120
tests <- 1 # integer at least 1
Fund.Monitor.Total.columns <- 70 # integer at least 60


# Do not touch variables:
{
Feature <- 0 # Should remain 0
Original.Fund <- c()
Fund <- c()
FUND <- as.data.frame(matrix(0,ncol = Total.columns))
timing <- system.time(source("Merged scripts with Controls.R"))
time_elapsed <- as.numeric(timing["elapsed"])
{
  FUND <- FUND[-1,]
  FUND <- FUND[,1: (max(  apply(FUND!= 0,MARGIN = 1,FUN = sum) )) ]
  colnames(FUND) <- paste0("Y",seq_along(FUND[1,]))
  rownames(FUND) <- paste0("SIM",c(1:(simulations)))
} #Cleaning up Fund Value monitor
Original.Fund.Avg <- mean(Original.Fund)
}

# Crafting and cleaning results
{result <- as.data.frame(rbind(round(simulations,digits = 0),
                              round(inital.members,digits = 0),
                              round(Prob.of.ruin*100, digits = 3),
                              round(VAR,digits = 3),
                              round((100*VAR/Original.Fund.Avg),digits = 3),
                              Bond.Proportion,
                              improv.factor,
                              coupon.rate*100,
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