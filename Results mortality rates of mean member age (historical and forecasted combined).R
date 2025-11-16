# This script is used for testing the parameters in the sims controller sheet.
# This usually regards to setting assumptions and sensitivity testing.

# Since this uses the original Sims Controller script as a template,
# I will be removing all comments and features that are already explained
# in the original sheet.

# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R

# This script also relies on starting with an empty environment
rm(list = ls())

# Pre - loop settings
tests <- 1 # integer at least 1
results <- c()

for(i in 1:tests){
  
  
  # Parameters that can be changed: 
  
  simulations <- 1 #  integer at least 1
  inital.members <- 7 #  integer at least 10
  Bond.Proportion <- 0 # value between 0 to 100
  improv.factor <- 50 # value between -100 to +100
  coupon.rate <- 13.17 # value between 0 to 100
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

#I pumped up the amount of members for a better mean age representation

mean.age <- round(mean(member.base[,2]),digits = 0)


Male.mean.age.mort <- Male.Mortality.Table[mean.age - 15 ,]
Female.mean.age.mort <- Female.Mortality.Table[mean.age - 15 ,]

# Plotting result of mortality
{
plot(x = seq_along(Male.mean.age.mort[1,-1]), y = Male.mean.age.mort[1,-1],
     main = paste(c("Extract of mortality rates of age ",mean.age, " (forecasting starts at t = 15)"))  ,
     xlab = "Time in years (t)",
     ylab = "Mortality rate q_x",
     type = "n",
     ylim = c(0,(ceiling(max(Male.mean.age.mort[1,-1])*100)/100)   )   )

lines(x = seq_along(Male.mean.age.mort[1,-1]), y = Male.mean.age.mort[1,-1],
      col = "blue", lty = 2)
lines(x = seq_along(Female.mean.age.mort[1,-1]), y = Female.mean.age.mort[1,-1],
      col = "violetred1", lty = 2)
lines(x = seq_along(Male.mean.age.mort[1,-c(1,17:ncol(Male.mean.age.mort))]),
      y = Male.mean.age.mort[1,-c(1,17:ncol(Male.mean.age.mort))],
      col = "blue", lty = 1)
lines(x = seq_along(Female.mean.age.mort[1,-c(1,17:ncol(Male.mean.age.mort))]),
      y = Female.mean.age.mort[1,-c(1,17:ncol(Male.mean.age.mort))],
      col = "violetred1", lty = 1)
lines(x = seq_along(Female.mean.age.mort[1,-1]), y = Female.mean.age.mort[1,-1],
      col = "violetred1", lty = 2)
lines(x = c(15,15), y = c(0,(ceiling(max(Male.mean.age.mort[1,-1])*100)/100)   )) 
legend("topright", legend = c("Male mortality rate", "Female mortality rate"), 
       col = c("blue", "violetred1"),
       lty = 2,
       cex = 0.75)


}

# Ok, let's loop the simulation to get the spread of our mortality forecasts

# We'll be using "Feature" as a vehicle to store the data throughout the loops
# since it doesn't get erased and it isn't really being used for anything right now


# I want to preserve the mortality rates of the male and female datasets
# and I want to keep the mean age, so we'll store them as 3 items in a list.
# Think of Feature as a grocery basket that we are carrying throughout the loops.
# I can add things to the basket without needing to change the main script.

Feature <- list("Deactivate seed", mean.age, Male.mean.age.mort, Female.mean.age.mort)


# Now to loop the simulation
for(j in 1:1000){
  # In the merged script, I deactivated the seed if it sees Feature as a list,
  # so that every run gives me a new random forecast.
  # This doesn't effect other scripts since this is the only one sofar
  # that uses a list with a unique identifier.

  {
  # Pre - loop settings
  tests <- 1 # integer at least 1
  results <- c()
  
  for(i in 1:tests){
    
    
    # Parameters that can be changed: 
    
    simulations <- 1 #  integer at least 1
    inital.members <- 7 #  integer at least 10
    Bond.Proportion <- 0 # value between 0 to 100
    improv.factor <- improv.factor # value between -100 to +100
    coupon.rate <- coupon.rate # value between 0 to 100
    interest <- interest # value between 0 to 100
    fixed_increase_rate <- fixed_increase_rate # value between 0 and 100
    rate_for_discounting <- interest # value between 0 and 100
    EPV.mort.risk.margin <- EPV.mort.risk.margin # value between 0 to 100
    reference.population.age <- reference.population.age # integer between 16 to 120
    Fund.Monitor.Total.columns <- 70 # integer at least 60
    
    Sensitivity_tests_loop_counter <- i
    # Targeted Parameter: Risk margin
    # Target Parameter: Prob of ruin 99%
    
    #EPV.mort.risk.margin <-  26 + i*2 
    
    
    # Do not touch variables:
    {
      #Feature <- 0 # Should remain 0
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
    sim.and.result <- c(EPV.mort.risk.margin, round(Prob.of.ruin*100, digits = 3),round((100*VAR/Original.Fund.Avg),digits = 3) )
    results <- cbind(results, sim.and.result)
  }
  
  colnames(results) <- seq_along(results[1,])
  rownames(results) <- c("Risk Margin", "Prob of Ruin", "VAR %")
  
  #View(results)
} # End of the simulation run
  
  # Now to add in the new mortality rates
  mean.age <- Feature[[2]]
  Male.mean.age.mort <- Male.Mortality.Table[mean.age - 15 ,]
  Female.mean.age.mort <- Female.Mortality.Table[mean.age - 15 ,]
  
  Feature[[3]] <- rbind(Feature[[3]], Male.mean.age.mort)
  Feature[[4]] <- rbind(Feature[[4]], Female.mean.age.mort)
  
  
}


# Ok, now to extract all that data
mean.age <- as.numeric(Feature[[2]])
Male.mean.age.mort <- as.data.frame(Feature[[3]])
Female.mean.age.mort <- as.data.frame(Feature[[4]])

Feature <- 0

rownames(Male.mean.age.mort) <- paste0("Sim", seq_along(Male.mean.age.mort[,1]))
rownames(Female.mean.age.mort) <- paste0("Sim", seq_along(Female.mean.age.mort[,1]))


# removing the age column, since we already know that we are looking at the age
# stored as mean.age
if(colnames(Male.mean.age.mort)[1] == "Age"){
Male.mean.age.mort <- Male.mean.age.mort[,-1]
Female.mean.age.mort <- Female.mean.age.mort[,-1]
}

colnames(Male.mean.age.mort) <- c(0:(ncol(Male.mean.age.mort)-1))
colnames(Female.mean.age.mort) <- c(0:(ncol(Female.mean.age.mort)-1))



# Now to create a dataset that shows the average, upper and lower bounds of the forecasts

Mean.Male.mort <- apply(Male.mean.age.mort, MARGIN = 2, mean)
Mean.Female.mort <- apply(Female.mean.age.mort, MARGIN = 2, mean)

Upper.Male.mort <- apply(Male.mean.age.mort, MARGIN = 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
Upper.Female.mort <- apply(Female.mean.age.mort, MARGIN = 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))

Lower.Male.mort <- apply(Male.mean.age.mort, MARGIN = 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
Lower.Female.mort <- apply(Female.mean.age.mort, MARGIN = 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))




# Since it's tedious to draw all of those elements manually, and twice over, I'll
# make use of a function instead.



library(ggplot2)

draw_fan_chart <- function(data, time_col, Mean_col, lower_col, upper_col,
                           title, line_color, area_color, forecast_start) {
  
  # Extract the actual time vector
  time_vector <- data[[time_col]]
  
  # Split data into pre-forecast and forecast segments
  pre_forecast <- data[time_vector <= forecast_start, ]
  post_forecast <- data[time_vector >= forecast_start, ]
  
  # Create ggplot object
  p <- ggplot(data, aes_string(x = time_col)) +
    # Shaded confidence interval
    geom_ribbon(aes_string(ymin = lower_col, ymax = upper_col, fill = "'95% CI of Forecast Range'"), alpha = 0.4) +
    # Solid line for pre-forecast
    geom_line(data = pre_forecast, aes_string(y = Mean_col, color = "'Observed'"), size = 0.7) +
    # Dashed line for forecast
    geom_line(data = post_forecast, aes_string(y = Mean_col, color = "'Forecast'"), linetype = 2, size = 0.7) +
    # Vertical line at forecast start
    geom_vline(xintercept = forecast_start, color = "black", linetype = 1) +
    # Labels and theme
    labs(title = title, x = "Time", y = "Mortality rate (q_x)") +
    scale_color_manual(values = c("Observed" = line_color, "Forecast" = line_color)) +
    scale_fill_manual(values = c("95% CI of Forecast Range" = area_color)) +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(p)
}



# Now that the function is created, let's transform the data into a suitable
# set of data to be plotted
{
Plot.Male.mort <- as.data.frame(cbind(c(0:(ncol(Male.mean.age.mort)-1)),
                        Mean.Male.mort, 
                        Upper.Male.mort, 
                        Lower.Male.mort))

Plot.Female.mort <- as.data.frame(cbind(c(0:(ncol(Female.mean.age.mort)-1)),
                        Mean.Female.mort, 
                        Upper.Female.mort, 
                        Lower.Female.mort))

colnames(Plot.Male.mort) <- c("Time", "Mean", "Upper", "Lower")
colnames(Plot.Female.mort) <- c("Time", "Mean", "Upper", "Lower")
}

# And finally now to plot the data

{
draw_fan_chart(data = Plot.Male.mort,
               time_col = "Time",
               Mean_col = "Mean",
               upper_col = "Upper",
               lower_col = "Lower", 
               title = paste(c("Observed and Forecasted ortality rates of Males age ",mean.age, " (forecasting starts at t = 15)"),collapse = ""), 
               forecast_start = 15, line_color = "blue", area_color = "skyblue")

draw_fan_chart(data = Plot.Female.mort,
               time_col = "Time",
               Mean_col = "Mean",
               upper_col = "Upper",
               lower_col = "Lower", 
               title = paste(c("Observed and Forecasted ortality rates of Females age ",mean.age, " (forecasting starts at t = 15)"), collapse = ""), 
               forecast_start = 15, line_color = "red3", area_color = "violet")
}
