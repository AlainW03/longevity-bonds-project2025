# This script is used for testing the parameters in the sims controller sheet.
# This usually regards to setting assumptions and sensitivity testing.

# Since this uses the original Sims Controller script as a template,
# I will be removing all comments and features that are already explained
# in the original sheet.

# This is the model's main parameter controls sheet
# Set the parameters, and run this script at once
# by pressing Ctrl+Alt+R

# Pre - loop settings
tests <- 10 # integer at least 1
results <- c()

for(i in 1:tests){


# Parameters that can be changed: 

simulations <- 10 #  integer at least 1
inital.members <- 100 #  integer at least 10
Bond.Proportion <- 100 # value between 0 to 100
improv.factor <- 0 # value between -100 to +100
#coupon.rate <- 12.5 # value between 0 to 100
interest <- 10.63 # value between 0 to 100
fixed_increase_rate <- 5.5 # value between 0 and 100
rate_for_discounting <- interest # value between 0 and 100
EPV.mort.risk.margin <- 43.29 # value between 0 to 100
reference.population.age <- 65 # integer between 16 to 120
Fund.Monitor.Total.columns <- 70 # integer at least 60

Sensitivity_tests_loop_counter <- i

# Targeted Parameter: Coupon rate
# Target Parameter: Prob of ruin 1%

coupon.rate <-  0 + i*5


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
sim.and.result <- c(coupon.rate, round(Prob.of.ruin*100, digits = 3),round((100*VAR/Original.Fund.Avg),digits = 3) )
results <- cbind(results, sim.and.result)




}

colnames(results) <- seq_along(results[1,])
rownames(results) <- c("Coupon rate", "Prob of Ruin", "VAR %")



# Eplaining how the risk margin is calculated
{
  # So, the best estimate of the risk margin is that where with the
  # current model parameters, the risk margin is toggled to set the
  # probability of ruin to 50%
  
  # The reason why the probability of ruin of 50% represents the best
  # estimate parameters:
  {
  # The definition of a best estimate assumption is:
  # - An assumption made by an Actuary on their best estimate of the future
  # experience, based on their professional judgement and expertise
  # - Or otherwise, an estimate such that there is an equal probability
  # of over and under estimating future experience.
  
  # The fund itself is a prospective reserve, and represents future expectations
  # of the liabilities. If the fund's value is set to have equally likely to 
  # over and underestimate future liabilities, then it would 
  # adhere to the second point on the definition of being a best estimate of 
  # future liabilities.
  # In a single simulation, if the fund over estimates the liabilities, 
  # it will not reach ruin throughout the simulation. If it underestimates the 
  # liabilities, it will reach ruin somewhere throughout the simulation.
  
  # But, we have to acknowledge that this approach will also set a best estimate
  # of the parameters that are adjusted for future coupon cashflows.
  # Thus, when testing the effects of the longevity bond itself, 
  # the base risk margin parameter should be calculated on a best or optimistic
  # estimate where there is no longevity bond in play, i.e. the proportion of
  # the fund invested into the longevity bond at onset should be 0%.
  }
  
  # What I'm essentially trying to build is similar to the
  # goal seek function in Excel, where I change the risk margin with
  # the goal to set the probability of ruin to 50%
  
  # To do this, I'm following an indexing approach learned in first year
  # Mathematical modelling class.
  
  # The idea is:
  {
  # Step 1: Run 10 tests with 100 simulations each, each test setting the risk margin
  # 5% higher and recording the resulting probability of ruin.
  
  # Step 2: Set the risk margin to the value just before the value that first
  # surpasses the target probability of ruin, which is 50%. For example,
  # if a risk margin of 30% results in a 100% Prob of Ruin, and 35% results in
  # 45% Prob of ruin, then the risk margin is set to 30%.
  
  # Not that the higher the risk margin, the lower the Prob of Ruin, hence
  # the results are theoretically strictly decreasing (subject to random variation)
  
  
  # Step 3: Now we have narrowed down the risk margin to a 5% interval. From here
  # we increase the amount of simulations, and decrease the subsequent increases
  # in the risk margin. For step 3, we run 10 tests, with 250 simulations, each test
  # setting the risk margin 0.5% higher.
  
  # Step 4: Repeat step 2 after step 3, but, if the risk margins tested results
  # in the Prob of ruin not being above and below 50% during the 10 tests, then the
  # simulation must be stopped and the operator must be notified that the
  # simulations in step 1 must be increased.
  
  # Step 5: Now we have narrowed down the best estimate of the risk margin
  # to within 0.5%. For the last step, run 10 tests with 1000 simulations each,
  # each increasing the risk margin by 0.05%
  
  # Step 6: The best estimate of the risk margin is now the one that sets the 
  # Prob of ruin the closest to 50%.
  }
  
}

# Calculating the best estimate of the risk margin
# Warning, hardcoding involved!
{
  
  # results should be above this line
  
  results2 <- rbind(results[1,],(results[2,] - 1)/1)
  
  first_negative_position <- as.numeric(which(results2[2,] < 0)[1])
  
  coupon.rate <- as.numeric(results2[1,first_negative_position - 1]) - 0.5
  
  # Run simulation again
  
  {
    # Pre - loop settings
    tests <- 10 # integer at least 1
    results <- c()
    
    for(i in 1:tests){
      
      
      # Parameters that can be changed: 
      
      simulations <- 40 #  integer at least 1
      inital.members <- 100 #  integer at least 10
      Bond.Proportion <- Bond.Proportion# value between 0 to 100
      improv.factor <- 0 # value between -100 to +100
      #coupon.rate <- 12.5 # value between 0 to 100
      interest <- 10.63 # value between 0 to 100
      fixed_increase_rate <- 5.5 # value between 0 and 100
      rate_for_discounting <- interest # value between 0 and 100
      reference.population.age <- 65 # integer between 16 to 120
      Fund.Monitor.Total.columns <- 70 # integer at least 60
      EPV.mort.risk.margin <- 43.29
      Sensitivity_tests_loop_counter <- i
      
      # Targeted Parameter: Coupon rate
      # Target Parameter: Prob of ruin 1%
      
      coupon.rate <-  coupon.rate + i*0.5 
      
      
      # Do not touch variables:
      {
        Feature <- 0 # Should remain 0
        Original.Fund <- c()
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
      sim.and.result <- c(coupon.rate, round(Prob.of.ruin*100, digits = 3),round((100*VAR/Original.Fund.Avg),digits = 3) )
      results <- cbind(results, sim.and.result)
      
      
      
      
    }
    
    colnames(results) <- seq_along(results[1,])
    rownames(results) <- c("Risk Margin", "Prob of Ruin", "VAR %")
  }
  
  
  # results should be above this line
  
  results3 <- rbind(results[1,],(results[2,] - 1)/1)
  
  first_negative_position <- as.numeric(which(results3[2,] < 0)[1])
  
  coupon.rate <- as.numeric(results3[1,first_negative_position - 1]) - 0.05 
  
  # Run simulation the last time
  
  {
    # Pre - loop settings
    tests <- 10 # integer at least 1
    results <- c()
    
    for(i in 1:tests){
      
      
      # Parameters that can be changed: 
      
      simulations <- 160 #  integer at least 1
      inital.members <- 100 #  integer at least 10
      Bond.Proportion <- Bond.Proportion # value between 0 to 100
      improv.factor <- 0 # value between -100 to +100
      #coupon.rate <- 12.5 # value between 0 to 100
      interest <- 10.63 # value between 0 to 100
      fixed_increase_rate <- 5.5 # value between 0 and 100
      rate_for_discounting <- interest # value between 0 and 100
      reference.population.age <- 65 # integer between 16 to 120
      Fund.Monitor.Total.columns <- 70 # integer at least 60
      EPV.mort.risk.margin <- 43.29
      Sensitivity_tests_loop_counter <- i
      
      # Targeted Parameter: Coupon rate
      # Target Parameter: Prob of ruin 1%
      
      coupon.rate <-  coupon.rate + i*0.05 
      
      
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
      sim.and.result <- c(coupon.rate, round(Prob.of.ruin*100, digits = 3),round((100*VAR/Original.Fund.Avg),digits = 3) )
      results <- cbind(results, sim.and.result)
      
      
      
      
    }
    
    colnames(results) <- seq_along(results[1,])
    rownames(results) <- c("Risk Margin", "Prob of Ruin", "VAR %")
    
    
  }
  y1 <- results[1, as.numeric(which(results[2,] <= 1)[1]) ]
  x1 <- results[2, as.numeric(which(results[2,] <= 1)[1]) ]
  y2 <- results[1, as.numeric(which(results[2,] <= 1)[1])-1 ]
  x2 <- results[2, as.numeric(which(results[2,] <= 1)[1])-1 ]
  
  coupon.rate <- as.numeric(approx(x = c(x1,x2), y = c(y1,y2), xout = 1 )$y)

  
}

View(results)