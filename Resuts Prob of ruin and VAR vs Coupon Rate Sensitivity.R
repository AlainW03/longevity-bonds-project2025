# This script is used for testing the parameters in the sims controller sheet.
# This usually regards to setting assumptions and sensitivity testing.

# Since this uses the original Sims Controller script as a template,
# I will be removing all comments and features that are already explained
# in the original sheet.

# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R

# So in this script I have to start with a clean environment







# The dataset you're looking for, after running this script, is called "results"





rm(list = ls())


# Pre - loop settings
tests <- 1 # integer at least 1
results <- c()

for(i in 1:tests){
  
  
  # Parameters that can be changed: 
  
  simulations <- 200 #  integer at least 1
  inital.members <- 100 #  integer at least 10
  Bond.Proportion <- 50 # value between 0 to 100
  improv.factor <- 50 # value between -100 to +100
  coupon.rate <- 11 # value between 0 to 100
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
  {
    result <- 0
    sim.and.result <- as.data.frame(rbind(round(simulations,digits = 0),
                                 round(inital.members,digits = 0),
                                 round(Prob.of.ruin*100, digits = 3),
                                 round(VAR,digits = 3),
                                 round((100*VAR/Original.Fund.Avg),digits = 3),
                                 Bond.Proportion,
                                 improv.factor,
                                 coupon.rate,
                                 round(time_elapsed,digits = 3)))
    
    results <- sim.and.result
    
    
    rownames(results) <- c("Simulations",
                          "Initial Nr of Members",
                          "Prob of Ruin %",
                          "VAR @ 95%",
                          "VAR as % of Original Fund",
                          "Prop invested in Bond",
                          "Mort Improvement Factor",
                          "Coupon Rate %",
                          "Runtime")
    
    
    #View(result)
    #View(FUND)
  } 
  
  
  # Results of testing
  # sim.and.result <- c(EPV.mort.risk.margin, round(Prob.of.ruin*100, digits = 3),round((100*VAR/Original.Fund.Avg),digits = 3) )
  # results <- cbind(results, sim.and.result)
}

# colnames(results) <- seq_along(results[1,])
# rownames(results) <- c("Risk Margin", "Prob of Ruin", "VAR %")

#View(results)


# Here is the average fund dataset
# avg.fund <- as.data.frame(apply(FUND, MARGIN = 2, FUN = mean))
# rownames(avg.fund) <- paste0("Y",seq_along(avg.fund[,1]))
# colnames(avg.fund) <- c(paste(c("Average Fund value with ", Bond.Prop*100,"% Bond Prop"), collapse = ""))
# plot(x = seq_along(avg.fund[,1]), y = avg.fund[,1], type = "l", xlab = "Years", ylab = "Avg Fund Value", main = paste("Average Fund Value over the years of the simulation with ", Bond.Prop*100,"% of the starting fund invested into the Longevity Bond"))
# 
# 



# So, since I don't need the Feature variable here, I'm using it as a place holder
# for the results overtime, since it doesn't get erased.
vector <- c(results[,1])
Feature <- as.data.frame(matrix(vector, ncol = 1))

# Now to repeat the simulation for different bond prop




# Pre - loop settings
tests <- 4 # integer at least 1
results <- c()

for(i in 1:tests){
  
  
  # Parameters that can be changed: 
  
  simulations <- simulations # simulations variable also doesn't get erased
  inital.members <- 100 #  integer at least 10
  Bond.Proportion <- Bond.Proportion # value between 0 to 100
  improv.factor <- improv.factor # value between -100 to +100
  coupon.rate <- 11 + i # value between 0 to 100
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
      FUND <- FUND[,1: (max(  apply(FUND[,-1]!= 0,MARGIN = 1,FUN = sum) )+1) ]
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
  { 
    result <- 0
    sim.and.result <- as.data.frame(rbind(round(simulations,digits = 0),
                                         round(inital.members,digits = 0),
                                         round(Prob.of.ruin*100, digits = 3),
                                         round(VAR,digits = 3),
                                         round((100*VAR/Original.Fund.Avg),digits = 3),
                                         Bond.Proportion,
                                         improv.factor,
                                         coupon.rate,
                                         round(time_elapsed,digits = 3)))
    
    #results <- cbind(results, sim.and.result)
    
    
    # rownames(results) <- c("Simulations",
    #                        "Initial Nr of Members",
    #                        "Prob of Ruin %",
    #                        "VAR @ 95%",
    #                        "VAR as % of Original Fund",
    #                        "Prop invested in Bond",
    #                        "Mort Improvement Factor",
    #                        "Coupon Rate %",
    #                        "Runtime")
    # #View(result)
    #View(FUND)
  } 
  
  
  # Results of testing
  # sim.and.result <- c(EPV.mort.risk.margin, round(Prob.of.ruin*100, digits = 3),round((100*VAR/Original.Fund.Avg),digits = 3) )
  # results <- cbind(results, sim.and.result)
  
  
  #colnames(results) <- seq_along(results[1,])
  #rownames(results) <- c("Risk Margin", "Prob of Ruin", "VAR %")
  
  #View(results)
  
  
  
  # avg.fund <- as.data.frame(apply(FUND, MARGIN = 2, FUN = mean))
  # avg.fund.adj <- avg.fund[,1]
  # Now to combine the results sofar with the placekeeper variable:
  #  if(ncol(Feature) > nrow(avg.fund)){
  #    avg.fund.adj <- c(avg.fund[,1], rep(0,ncol(Feature)-length(avg.fund[,1])))
  #  }else{avg.fund.adj <- avg.fund[,1]}
  
  Feature <- cbind(Feature, sim.and.result)
  
  
  
  # Now to combine the results sofar with the placekeeper variable:
  
  #Feature <- rbind(Feature, avg.fund[,1])
  
  
}


# Now to neatly compile the dataset

# Avg.Fund.Values <- as.data.frame(Feature)
# Feature <- 0
# colnames(Avg.Fund.Values) <- paste0("Y", seq_along(Avg.Fund.Values[1,]))
# Bond.props <- 0 + (100/tests)*(0:tests)
# rownames(Avg.Fund.Values) <- paste0(Bond.props,"%")

results <- Feature

rownames(results) <- c("Simulations",
                       "Initial Nr of Members",
                       "Prob of Ruin %",
                       "VAR @ 95%",
                       "VAR as % of Original Fund",
                       "Prop invested in Bond",
                       "Mort Improvement Factor",
                       "Coupon Rate %",
                       "Runtime")

colnames(results) <- paste0("Result ", seq_along(results[1,]))





# Now to plot the results
{
# First, generate distinct colours

#generate_rgb_colours <- function(n) {
  # Generate hues evenly spaced around the color wheel
#   hues <- seq(0, 360, length.out = n + 1)[-1]  # remove last to avoid duplicate hue
#   
#   # Convert HCL to RGB using grDevices::hcl and then to hex
#   colours_vec <- grDevices::hcl(h = hues, c = 150, l = 55)
#   
#   return(colours_vec)
# }
# 
# # Generating colours
# n <- nrow(Avg.Fund.Values)
# colours_vec <- generate_rgb_colours(n)
# 
# 
# 
# plot(x = seq_along(Avg.Fund.Values[1,]), y = Avg.Fund.Values[1,], 
#      type = "n",
#      xlab = "Years", 
#      ylab = "Avg Fund Value", 
#      main = "Average Fund Value over the years of the simulation with different % of the starting fund invested into the Longevity Bond",
#      ylim = c(min(Avg.Fund.Values),max(Avg.Fund.Values)*1.5)
# )
# 
# for(k in 1:n) {
#   
#   lines(x = seq_along(Avg.Fund.Values[k,]), y = Avg.Fund.Values[k,], col = colours_vec[k])
# }
# 
# text_width <- strwidth("100% invested in Longevity Bond", cex = 0.5)
# legend_text <- paste0(Bond.props,"% invested in Longevity Bond")
# 
# legend("topright",
#        legend = legend_text,
#        col = colours_vec,
#        lty = 1,
#        cex = 0.5, bty = "n",
#        y.intersp = 0.3,
#        text.width = text_width*0.5)
}

write.csv(results, "Summary Statistics.csv")
