# This is the merged model of the separate individual models.
# Press Alt+0 to collapse all fold-able sections, it makes it a lot easier to read
# Why do we do this?
{
# Well, to measure the effect of the longevity bonds, we need to first 
# get measures that longevity influences.

# In our case, longevity causes our fund to pay more benefits than expected, so
# we expect to see a faster depletion and thus a higher risk of the fund not 
# being able to cover member's benefits
# That is exactly what the VAR @ 95% level and the Probability of Ruin measures,
# but to measure that, we can either do it theoretically or empirically.

# We can't possibly include all the complexities in a theoretical approach, hence
# we have to follow an empirical approach. And that's why we need everything on
# one script.

# I don't have the computing power right now to be able to run the full script,
# so for debugging purposes, I'm only going to consider a fund with 10 members

# Also, we can't just copy and paste everything and expect it to work.
# I mean, it can work theoretically, but the models will need to be linked to
# each other. This means no creating or importing csv files

# Luckily for me, my style of coding has set the places where the links should 
# happen close to the start or end of the script, with very few exceptions here
# and there.
}

# Let's go!

# First, let's set a script wide seed for consistent results. As it is now, 
# the seeds are giving us only one possible result for each piece of the model.

if(all(class(Feature) != "list")){set.seed(780)}else if(Feature[[1]] != "Deactivate seed"){set.seed(780)}



# This is the part where we integrate the first half of the VAR and Prob of Ruin

# So to calculate the VAR and Prob of ruin, there is actually a very simple
# approach.

# Explaining approach
{
  # VAR
  # The VAR will be calculated on the 95% percentile of all the 
  # possible losses the fund has made.
  
  # The loss of one simulation is the difference between the 
  # initial fund value, and the final fund value.
  
  # So what we need is a vector that records all the losses of the fund
  # and the 95% highest loss will be our VAR.
  
  
  # Prob of ruin
  # This is even easier. "ruin" is if the fund decreases to a value below 0
  # in each simulation. That means we only need a vector of 1's and 0's
  # that records if the final fund value is below 0.
  
}

# Creating empty loss and ruin vectors
loss <- c()
ruin <- c()



# Creating loop to run multiple simulations

for(sim in 1:simulations) {
  


#Second, we need our mortality table and Lee - Carter Model

{
  
  # So, this R script will be used to create the lee carter model,
  # or rather fit the parameters of the Lee Carter model, based on the 
  # mortality data gathered from the CMI.
  
  # Then, it will create our custom mortality base, where we can control the 
  # the rate at which mortality decreases.
  
  #There will essentially be 2 models, one for male mortality, the other for female.
  
  #I first start off by gathering and organising the data received from the CMI team.
  
  library(readxl)
  library(demography)
  library(forecast)
  
  #/////////////////////////////////////////////////////////////////////////////
 
  
  
  #Since I do not have all the data I need to build it, I'll make dummy data
  # in the mean time and just get the model up and running, leaving space
  # for the correct data to be inserted.
  
  # I will have a variable, called improv.factor (improvement factor),
  # which will be a value between -100 and 100
  # A value below 0 causes a worsening mortality rate, and a value above 0 causes a
  # better mortality rate over time.
  
  improv.factor <-   improv.factor # Looks funny cause the variable is already
  # called improv.factor in the controller sheet, but I wanted to keep this line
  # here for completeness of the model
  
  
  # Here I used to create the dummy data, which will be called Male.Mort.data

  # Here I used to create the dummy data, which will be called Female.Mort.data

  
  
  #A few months later, I have the actual data, this is how I import them
  # With this, I can start constructing the Lee carter model 
  
  # Importing the mortality data
  {
    library(readxl)
    
    Male.mort.data.unclean <- read.csv("Male_Mort_data.csv")
    Female.mort.data.unclean <- read.csv("Female_Mort_data.csv")
    
    colnames(Male.mort.data.unclean) <- c("Age", 2008:2023)
    colnames(Female.mort.data.unclean) <- c("Age", 2008:2023)
    
  Male.Mort.data <- Male.mort.data.unclean
  Female.Mort.data <- Female.mort.data.unclean
    
  }
  
  #First I explain the Lee Carter model basics and some assumptions
  {
  # So basically, the Lee Carter model is ln(m_x,t) = a_x + b_x * k_t
  # a_x is the average mortality for age x
  # b_x is the sensitivity to changes of mortality of age_x to k_x
  # k_t is the time-varying mortality index
  
  # So this approach assumes that mortality decreases at a random but steady rate,
  # this is just one of the drawbacks of the Lee-Carter model.
  
  # Intuitively, mortality should be decreasing at a decreasing rate, since
  # humans can only live that long. Medical advances and higher quality of life
  # only reduces the amount of causes of death, but it doesn't make us immortal,
  # hence we can expect there to be an upper limit to which current drivers of
  # mortality improvements can improve mortality, and the closer we get to
  # that limit, the more difficult it is to improve mortality an additional, 
  # say 1%. However, determining at which rate the rate at which mortality 
  # decreases itself decreases, is something at least beyond the time constraint
  # we have for this research project.
  
  
  # But determining those parameters is too much trouble to do manually
  # Great thing about R, is that there is already a package that can aid us 
  # with this forecast.
  
  # The package is called demography, and it uses the lca()
  # function to implement the Lee-Carter model.
  
  #Though there isn't a direct parameter in this function to control the
  # improvements in mortality, this can be done indirectly by
  # manually overwriting the k_t estimates of the fit before forecasting
  # which is as easy as replacing one column with your own custom column of
  # the same dimensions.
  }
  # This is where we determined the parameters of the Lee-Carter Model
  # and forecasted Male Mortality data

  {
  #This is the parameters being identified
  {
  #First transform the data to only contain the rates:
  ages <- c(Male.Mort.data[,1])
  mort.rates.data <- as.matrix(Male.Mort.data[,-1])
  
  # Of course, we'll have to decide the forecast length:
  
  forecast.length <- nrow(Male.Mort.data) - (ncol(Male.Mort.data)-1)
  
  #Now we have to create a model data set that demography can use.
  # The transformation is done with the demodata function.
  
  
  demo <- demogdata(
    data =  mort.rates.data,
    pop = matrix(1000000, ncol = ncol(mort.rates.data), nrow = nrow(mort.rates.data)),
    #Just made up a large population base so that the model can do its thing
    ages = ages,
    years = c(2008:2023),
    type = "mortality",
    label = "YourData",
    name = "mortality rates"
  )
  
  #Now to fit the parameters of the Lee-Carter model:
  
  lca_data <- lca(
    demo,
    series = names(demo$rate)[1],
    ages = demo$age,
    years = demo$year,
    max.age = max(demo$age),
    adjust = "none",
    chooseperiod = FALSE,
    minperiod = forecast.length,
    scale = FALSE,
    restype = "rates", interpolate = FALSE
  )
  
  
  
  # Now we have the parameters of the model, and there is even a function
  # that can do the forecasting for us, but I want to incorporate my own kt's
  # to control the mortality improvement.
  
  # So first I extract the ax and bx parameters:
  
  ax <- as.numeric(lca_data$ax)
  bx <- as.numeric(lca_data$bx)
  
  # To forecast the kt's, I'm going to need the historical kt's too
  
  kt_hist <- as.numeric(lca_data$kt)
  
  }
  
  
  #Now that I have my parameters, and my historical kt's, I can start 
  #forecasting the custom kt's
  
  # I explain how it is done here
  {
  # I can either do this manually, or by clever use of manual overriding.
  # Thing is, I can just make a kt model, adjust the drift parameter, and continue
  # using the forecasting function with the adjusted model settings.
  
  # Reason is that to allow the adjustment to be simple enough to control, while
  # not too simple as to make it too unrealistic.
  }

  
  #Now let's forecast the kt's
  {
    
    library(forecast)
    if(sum(abs(c(kt_hist)))==0) {
      drift <- 0
      sigma2 <- 0
    }else{
    model <- Arima(c(kt_hist), order = c(0,1,0), include.drift = TRUE)
    
    # This gives the drift (mean) parameter
    drift <- model$coef["drift"] * (1 + improv.factor/100)
    sigma2<- model$sigma2
    }
    #set.seed(780)
    kt_forecast_data <- kt_hist[length(kt_hist)]
    for(i in 2:(forecast.length+1)) {
      kt_min1 <- kt_forecast_data[i-1]
      kt_sim <- drift + kt_min1 + rnorm(1, mean = 0, sd = sqrt(sigma2))
      kt_forecast_data <- c(kt_forecast_data,kt_sim)
    }
    kt_forecast <- as.numeric(kt_forecast_data[-1])
    #kt_forecast_data <- forecast(model, h = forecast.length)
    
    #I used the following code to visually investigate how large the jump is 
    # between the last historical value and the first forecasted value
    #y_value <- c(as.numeric(kt_hist)[(length(kt_hist)-3):length((kt_hist))], as.numeric(kt_forecast)[1:4])
    #x_value <- 1:length(y_value)
    #plot(x = x_value, y = y_value, type = "s")
    
    }
    
  # Now I have all the components to forecast my mortality
  
  mortality.forecast <- exp(ax + bx %*% t(kt_forecast))
  
  {# Though as it is now, the mortality rate can exceed 1.
    # The mortality improvement factor only strengthens (or weakens)
    # he overall trend, meaning if one of the random simulations result
    # in worsening mortality (which one of them do), the improvement factor
    # can cause the mortality to go higher than 1.
    
    # Luckily we don't need to make allowance for mortality going below
    # 0, since it is a natural boundary of the exponential function
  }
  
  # Now to make sure that mortality doesn't go greater than 1
  mortality.forecast <- exp(ax + bx %*% t(kt_forecast))
  for(MORTALITY in 1:length(mortality.forecast)){
    mortality.forecast[MORTALITY] <- min(mortality.forecast[MORTALITY],1)
  }
  
  #Let's tidy it up:
  
  mortality.forecast <- cbind(ages,mortality.forecast)
  colnames(mortality.forecast) <- c("Ages",2024:(2023+ncol(mortality.forecast)-1))
    
    
  
  # To check the data, let's see the mortality rates of different ages, with the historical
  # and forecasted values combined
  
  {
  #plot((as.numeric(c(Male.Mort.data[(16-15),-1],mortality.forecast[(16-15),-1]))), type = "n",
  #    ylim = c(min(mortality.forecast[,-1]),0.0007))
  #lines((as.numeric(c(Male.Mort.data[(16-15),-1],mortality.forecast[(16-15),-1]))), col = "blue")
  #lines((as.numeric(c(Male.Mort.data[(30-15),-1],mortality.forecast[(30-15),-1]))), col = "red")
  
  }
  
  # Here I plot a 3D graph to check the data
  {
    #combined <- cbind(Male.Mort.data[-105,-1],mortality.forecast[-105,-1])
    #mat <- as.matrix(-log(combined))
    
    
    # Load necessary library
    #library(plotly)
    
    # Create a matrix with values > 1
    
    # Create grid for x and y axes
    #x <- 2014:(2024+ forecast.length)
    #y <- 16:120
    
    
    
    # Plot using plotly
    #plot_ly(
    #  x = ~x, y = ~y, z = ~mat,
    #  type = "surface"
    #) %>%
    #  layout(
     #   title = "3D Surface Plot of Matrix",
     #   scene = list(
     #     xaxis = list(title = "Columns"),
     #     yaxis = list(title = "Rows"),
     #     zaxis = list(title = "Values")
      #  )
     # )
    
  }
  
  # Now we complete our dummy mortality data by combining the dummy rates
  # and the forecasted rates
  
  Final.Male.Mort.table <- as.data.frame(cbind(Male.Mort.data, mortality.forecast[,-1]))
  
  #write.csv(Final.Mort.table, "LC Mortality Data.csv", row.names = FALSE)
  }
  
  # And here I do the same for the Female Mortality Data
  {
    #This is the parameters being identified
    {
      #First transform the data to only contain the rates:
      ages <- c(Female.Mort.data[,1])
      mort.rates.data <- as.matrix(Female.Mort.data[,-1])
      
      # Of course, we'll have to decide the forecast length:
      
      forecast.length <- nrow(Female.Mort.data) - (ncol(Female.Mort.data)-1)
      
      #Now we have to create a model data set that demography can use.
      # The transformation is done with the demodata function.
      
      
      demo <- demogdata(
        data =  mort.rates.data,
        pop = matrix(1000000, ncol = ncol(mort.rates.data), nrow = nrow(mort.rates.data))
        #Just made up a large population base so that the model can do its thing
        ,ages = ages,
        years = 2008:2023,
        type = "mortality",
        label = "YourData",
        name = "mortality rates"
      )
      
      #Now to fit the parameters of the Lee-Carter model:
      
      lca_data <- lca(
        demo,
        series = names(demo$rate)[1],
        ages = demo$age,
        years = demo$year,
        max.age = max(demo$age),
        adjust = "none",
        chooseperiod = FALSE,
        minperiod = forecast.length,
        scale = FALSE,
        restype = "rates", interpolate = FALSE
      )
      
      
      
      # Now we have the parameters of the model, and there is even a function
      # that can do the forecasting for us, but I want to incorporate my own kt's
      # to control the mortality improvement.
      
      # So first I extract the ax and bx parameters:
      
      ax <- as.numeric(lca_data$ax)
      bx <- as.numeric(lca_data$bx)
      
      # To forecast the kt's, I'm going to need the historical kt's too
      
      kt_hist <- as.numeric(lca_data$kt)
      
    }
    
    
    #Now that I have my parameters, and my historical kt's, I can start 
    #forecasting the custom kt's
    
    # I explain how it is done here
    {
      # I can either do this manually, or by clever use of manual overriding.
      # Thing is, I can just make a kt model, adjust the drift parameter, and continue
      # using the forecasting function with the adjusted model settings.
      
      # Reason is that to allow the adjustment to be simple enough to control, while
      # not too simple as to make it too unrealistic.
    }
    
    
    #Now let's forecast the kt's
    {
      library(forecast)
      
      library(forecast)
      if(sum(abs(c(kt_hist)))==0) {
        drift <- 0
        sigma2 <- 0
      }else{
        model <- Arima(c(kt_hist), order = c(0,1,0), include.drift = TRUE)
        
        # This gives the drift (mean) parameter
        drift <- model$coef["drift"] * (1 + improv.factor/100)
        sigma2<- model$sigma2
      }
      #set.seed(780)
      kt_forecast_data <- kt_hist[length(kt_hist)]
      for(i in 2:(forecast.length+1)) {
        kt_min1 <- kt_forecast_data[i-1]
        kt_sim <- drift + kt_min1 + rnorm(1, mean = 0, sd = sqrt(sigma2))
        kt_forecast_data <- c(kt_forecast_data,kt_sim)
      }
      kt_forecast <- as.numeric(kt_forecast_data[-1])
      #kt_forecast_data <- forecast(model, h = forecast.length)
      
      #I used the following code to visually investigate how large the jump is 
      # between the last historical value and the first forecasted value
      #y_value <- c(as.numeric(kt_hist)[(length(kt_hist)-3):length((kt_hist))], as.numeric(kt_forecast)[1:4])
      #x_value <- 1:length(y_value)
      #plot(x = x_value, y = y_value, type = "s")
      
    }
    
    # Now I have all the components to forecast my mortality
    
    mortality.forecast <- exp(ax + bx %*% t(kt_forecast))
    
    {# Though as it is now, the mortality rate can exceed 1.
      # The mortality improvement factor only strengthens (or weakens)
      # he overall trend, meaning if one of the random simulations result
      # in worsening mortality (which one of them do), the improvement factor
      # can cause the mortality to go higher than 1.
      
      # Luckily we don't need to make allowance for mortality going below
      # 0, since it is a natural boundary of the exponential function
    }
    
    # Now to make sure that mortality doesn't go greater than 1
    mortality.forecast <- exp(ax + bx %*% t(kt_forecast))
    for(MORTALITY in 1:length(mortality.forecast)){
      mortality.forecast[MORTALITY] <- min(mortality.forecast[MORTALITY],1)
    }
    
    #Let's tidy it up:
    
    mortality.forecast <- cbind(ages,mortality.forecast)
    colnames(mortality.forecast) <- c("Ages",2024:(2023+ncol(mortality.forecast)-1))
    
    
    
    # To check the data, let's see the mortality rates of different ages, with the historical
    # and forecasted values combined
    
    {
      #plot((as.numeric(c(Female.Mort.data[(16-15),-1],mortality.forecast[(16-15),-1]))), type = "n",
      #    ylim = c(min(mortality.forecast[,-1]),0.0007))
      #lines((as.numeric(c(Female.Mort.data[(16-15),-1],mortality.forecast[(16-15),-1]))), col = "blue")
      #lines((as.numeric(c(Female.Mort.data[(30-15),-1],mortality.forecast[(30-15),-1]))), col = "red")
      
    }
    
    # Here I plot a 3D graph to check the data
    {
      #combined <- cbind(Female.Mort.data[-105,-1],mortality.forecast[-105,-1])
      #mat <- as.matrix(-log(combined))
      
      
      # Load necessary library
      #library(plotly)
      
      # Create a matrix with values > 1
      
      # Create grid for x and y axes
      #x <- 2014:(2024+ forecast.length)
      #y <- 16:120
      
      
      
      # Plot using plotly
      #plot_ly(
      #  x = ~x, y = ~y, z = ~mat,
      #  type = "surface"
      #) %>%
      #  layout(
      #   title = "3D Surface Plot of Matrix",
      #   scene = list(
      #     xaxis = list(title = "Columns"),
      #     yaxis = list(title = "Rows"),
      #     zaxis = list(title = "Values")
      #  )
      # )
      
    }
    
    # Now we complete our dummy mortality data by combining the dummy rates
    # and the forecasted rates
    
    Final.Female.Mort.table <- as.data.frame(cbind(Female.Mort.data, mortality.forecast[,-1]))
    
    #write.csv(Final.Mort.table, "LC Mortality Data.csv", row.names = FALSE)
  }
  
  
}
#Tempxaxis <- as.numeric(colnames(Final.Female.Mort.table[,-1]))
#Tempyaxis <- c(as.numeric(Final.Female.Mort.table[40,-1]))
#  plot(x = Tempxaxis, y = Tempyaxis, type = "l")
# The dummy mortality rates are called Final.Mort.table
# To link the model above to the model below, we rename Final.Mort.table to 
# Mortality.Table
Male.Mortality.Table <- Final.Male.Mort.table
Female.Mortality.Table <- Final.Female.Mort.table
# Run this line below to remove all but Mortality.Table from your environment
 rm(list = setdiff(ls(), c("inital.members","simulations","Bond.Proportion",
                           "interest", "fixed_increase_rate","reference.population.age","EPV.mort.risk.margin","coupon.rate", "Feature","Original.Fund", "Fund",
                           "improv.factor","control","Male.Mortality.Table",
                           "Female.Mortality.Table","rate_for_discounting", "loss","sim", "tests", "BOND","FUND", "ruin","results","results2","results3","Sensitivity_tests_loop_counter")))










#///////////////////////////////////////////////////////////////////////////////
  #Setting number of members at time 0
  num.members <- inital.members
  # This is where we mainly control the size of this script, hence I moved it here
#//////////////////////////////////////////////////////////////////////////////


 
 
 
 
 
 



# Third, we add our member.base model


{
  #library(readxl)
  #Extracting mortality table
  #Mortality.Table <- read.csv("LC Mortality Data.csv")
  # Not needed since this is where we linked the previous model to this model
  
  
  #Getting the lowest and highest age in the table for later use
  
  lowest.age <- as.numeric(  min((Male.Mortality.Table[,1])  ))
  highest.age <- as.numeric(  max(Female.Mortality.Table[,1])  )
  
  #Setting seed for consistency (Removing seed)
  #set.seed(780)
  
  
  # Here is where we used to set the number of members in the fund
  
  #Setting number of years the portfolio is expected to be active (max is 70 years
  #since terminal age is 110 for now, and the min age is 40 for now)
  num.years <- highest.age - lowest.age

  #colnames(Mortality.Table) <- c("Age", 2014:(2014+num.years))
  # The above line was to clean the data, but that is not needed in this script
  
  
  #Creating the index for our members
  members <- c(1:num.members)
  
  #Now to differentiate between Males and Females.
  # This is going to be assigned 50:50, as we expect the true mix
  # to be closer to 50:50
  # I'm having trouble with the char class, so 1 will be Male, 2 will be Female
  
  Gender <- sample(c(1,2), size = num.members, replace = TRUE)
  
  

    # The following text explains the approach to selecting the members
    # of our fund
    {#So the distribution of ages in a pension fund isn't necessarily uniform
    #Thus, I will be sampling ages from a very skewed distribution
    # such that I get a higher amount of younger ages than older
    # ages, which is what is expected.
    # Though, I can't just set the parameters and make it work.
    # Thus I will be doing something that is similar to a lag later on.
    # Meaning that my ages will be skewed so low, that the simulation
    # will run a couple of years, and as members die off, the distribution of ages 
    # will be automatically set.
    
    #For example, I'll generate a skewed sample of ages between 20-90,
    # with a higher probability of sampling ages between 20-30 than ages 70-90
    # then, I will first simulate 40 years, and then whoever is still alive 
    # after 40 years, will remain in the scheme for our choice of members
    # on which we will simulate the annuities.
    
    #The next upgrade to this code will be to include gender,
    # for now we'll focus on male lives just to complete this step of the code
    }
    
    {
      #Setting a temporary seed to generate ages (should be removed in final version)
      #set.seed(780)
      
      # Now to generate a distribution for our ages, the beta distribution is 
      # suitable since it has 2 easy parameters that I can control
      {
      # To manipulate the shape, I set a targeted mean age, and then 
      # control the Beta (shape2) to increase or decrease the thickness 
      # of the upper tail
      
      target_mean_age <- 50 # +30 to get final member base's mean age
      target_mean_percent <- (target_mean_age- lowest.age)/num.years
      shape2 <- 25
      shape1 <- (target_mean_percent* shape2)/(1- target_mean_percent)
      
      age_dist <- rbeta(num.members, shape1 = shape1, shape2 = shape2)
    }
      # Now to convert the distribution to our ages
      ages <- round(lowest.age + age_dist*num.years, 0)
      
      # Quickly plotting the age distribution to visually verify the shape
      {
      #plot(density(ages), main = "Density of sampled Ages", xlab = "Ages", xlim = c(16,100))
      #I'm very happy with this distribution, I targeted a mean age, allowing
      # the tail to go down steadily over the ages.
      
      
      #So it turns out that the age distribution is highly sensitive to my
      # initial age distribution, thus I'll have to look up what is the average 
      # age in a pension fund, and its variance, and then replicate it to 
      # 30 years younger.
      
      #So according to an article by SANLAM, the average retirement age is around 80
      # and from what I can gather from other articles, there seems to be more
      # people 80 or younger than 80 and older, but roughly equal parts,
      # so that is the shape I am aiming for with this simulation.
      
      #For now I'm happy with the shape, I'll have to run a sensitivity test on 
      # the mean age and spread to investigate its significance, but that comes later
      }
    } # Members and their ages are now decided
    

  
  
  
  # Alright, now to start the simulation.
  
  {
  # The simulation will start at the current set of ages, but afterwards
  # I will cut the first cut_off amount years out, as at that point I expect the distribution
  # of the ages to have taken the shape one would see in a pension scheme.
  
  #Creating an empty space for member base to occupy later
    #Columns must equal the years + 3 (1 for index, 1 for ages, 1 for gender, rest for years)
    member.base <- matrix(0, nrow = num.members, ncol = num.years+3)
  
  {
    #Creating a count variable to help set rows in member base
    count <- 1
    
    #For loop runs for every member to simulate their state of life through the years
    for(i in members){
      
      #assuming everyone in the portfolio is alive at time 0
      prev_state <- 1
      
      #creating an empty row for the member info to occupy
      member_row <- c(matrix(0,nrow = 1, ncol = num.years+3))
      
      #Added 3 columns, the first the member index, the second is the gender,
      # the third the member's age
      # Though Gender and Age will have to switch around later for the rest of
      # the merged model to work. They are in this order since this
      # particular loop relies on the age being the last of the non state columns
      member_row[1] <- i
      member_row[3] <- ages[i]
      member_row[2] <- Gender[i]
      
      #Simulating state of life for member i from column 4 onwards
      #(hence time 1 is column 4)
      for(j in 4:num.years){
        
        #Thus j-3 is the time point of the state being simulated
        
        #Unif q probability
        value <- runif(1,0,1)
        
        #Getting the current age of the member, as at point j
        #Including column 2 in here, since that is the original age
        current_age <-  sum(member_row[3:length(member_row)])
        
        #Ensuring that the member's age does not exceed 110
        
        y <- ifelse(current_age > highest.age, highest.age, current_age)
        
        
        #getting the index of the member's age in the mortality table
        index_age <- y-(lowest.age-1)
        
        #Getting qx according to the member's age and gender
        gender <- member_row[2]
        if (gender == 2) {
          qx <- as.numeric(Female.Mortality.Table[index_age,j-2])}else{
            qx <- as.numeric(Male.Mortality.Table[index_age,j-2])}
        
        #State of life determined by generated q prob and the qx in mortality table
        state <- prev_state*qbinom(value,size = 1, prob = 1 - qx)
        
        #resetting previous state value, such that if the member dies, 
        #the 'state' variable is 0
        prev_state <- member_row[j] <- state
        
        #here you can end the loop if state = 0, but that feature doesn't bring much
        # value to the model as of yet.
        
      } # End of j loop
      
      #assigning the members life state in the appropriate row of the member base
      member.base[i,] <- member_row
      
      #updating the count for next loop
      count <- count + 1
    }
    
    #Adding a lifetime column for analysis and error checking
    
    Lifetime <- apply(X = member.base[,4:ncol(member.base)],FUN = sum, MARGIN = 1)
    member.base <- cbind(member.base[,c(1,3,2)], Lifetime, member.base[,4:ncol(member.base)] )
    
    #Giving column headings
    colnames(member.base) <- c("Member","Age","Gender", "Lifetime",1:(num.years))
    
    #Error checking for member with maximum lifetimes
    member.base[which.max(member.base[,4]),1]
    
    #Everything seems to be in order
  }
  
  # Now for the final member base where we cut the first n number of years
  
  cut_off <- 30
  
  final.member.base <- member.base[member.base[,cut_off]==1,c(1:4,cut_off:ncol(member.base))]
  colnames(final.member.base) <- c("Member","Age", "Gender", "Lifetime",1:((ncol(final.member.base)-4  ) )   )       
  
  # of course, everyone is cut_off amount years older by this point, so we will need to adjust the ages too
  
  final.member.base[,2] <- final.member.base[,2] + cut_off
  #Adjusting the lifetime, I have to minus 5, that's just how it ended up needing
  # to be corrected
  final.member.base[,4] <- final.member.base[,4] - (cut_off-5)
  }
  
  
  #There we have it. Now I just increase the number of members, and we should have a nice
  # member base
  
  #For curiosity sake, let's look at how the distribution of ages and gender ended up to be:
  {
  #plot(density(as.numeric(final.member.base[,2] )), main = "Density of Final Member Base Ages", xlab = "Ages", xlim = c(60,100))
  
  #Now let's take a look at the distribution of age of death
  #plot(density(as.numeric(final.member.base[,2] )+as.numeric(final.member.base[,3] )), main = "Density of Final Member Base Death Ages", xlab = "Ages")
  
  # Now, I will export this final.member.base as a cvs file
  # This piece of code should only run once, otherwise it will create duplicates
  
  #write.csv(final.member.base, "Generated State of Life Member Base.csv", row.names = FALSE)
  
  # Note that I think the column names are actually incorrect. I cut off the first n number
  # of years, meaning that the time stamp is n years later than 2014.
  # Though I'll ponder on this thought for a while before I make the change
  
  
  # I pondered, and agreed, the simulation is at a later point in time, saying
  # the year is 2014 is misleading. I have edited the script to just start at year 1
  
  # With small numbers, it looks weird, but when we have larger numbers, the graphs made sense
  }
  
}

  
  
# Just to reiterate here, in the data sets the Genders are labelled:
  
# Male = 1
# Female = 2

  
# The result of the model above is called final.member.base
# To link the member base model to the next model, we change the name of final.member.base
# to member.base
 member.base <- as.data.frame(final.member.base)
 
 #Also, the member base is excessively long, let's trim it down
 member.base <- member.base[,1:(max(member.base[,4])+4)]
 
 #cut_off needs to be retained for the next model
 
# Run this line below to remove all but Mortality.Table and member.base from your environment
 rm(list = setdiff(ls(), c("inital.members","simulations","Bond.Proportion","interest", "fixed_increase_rate","reference.population.age","EPV.mort.risk.margin","coupon.rate", "Feature","Original.Fund", "Fund","improv.factor","control", "cut_off","Male.Mortality.Table","Female.Mortality.Table","rate_for_discounting",
                            "member.base","loss","sim", "tests", "BOND","FUND", "ruin","results","results2","results3","Sensitivity_tests_loop_counter")))
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 # Fourth, we add the benefit base model
 
 
 {
   
   # So the purpose of this script is to set up the sim for the actual
   # fund that will be used to pay the benefits of our members
   
   # Explanation and justification behind benefit structure
   {# To do that, we will first need the actual benefit amounts to be paid
   # Now normally the consequence of longevity can be worsened by those
   # with higher benefit levels living longer than those with lower
   # benefit levels
   
   # But this variation is more due to sampling risk rather than longevity risk,
   # and quite honestly opens up a whole new can of worms regarding the actual
   # mortality rates that we should use.
   # But our mortality rates are artificial anyways. They are based on the real thing
   # but in the end it is us who decide how bad the longevity ends up being.
   
   # Thus to remove sampling and some parameter risk, the assumption will be that
   # everyone in our member base just happen to receive the same benefit level.
   
   #We can include an upgrade to assign varying levels of benefits later, but for this
   # script, it is more important to get something to build on in the first place.
   
   # We also assume that the benefits increase at a fixed rate to provide some protection
   # against inflation, since making them inflation-linked will bring in inflation risk
   # , tracking risk, parameter risk, model risk and random fluctuation risk, all of which 
   # can obscure the longevity risk that we are trying to target here, hence the assumption.
   
   }
   
   # Pulling the member base model. Not needed since now the model is linked
   {#library(readxl)
   #member.base <- read.csv("Generated State of Life Member Base.csv")
   # I don't like the X's in front of the years, so I'm removing them like this:
   #colnames(member.base) <- c(colnames(member.base[,1:3]), 2014:(2014 + length(colnames(member.base))-4))
   # Of course, check whether the first year is actually 2014 first
   
   #Also, the member base is excessively long, let's trim it down
   #member.base <- member.base[,1:(max(member.base[,3])+4)]
   }
   
   # Now I justify the inflation increase
   {# Running the member base sim with 10 000 members multiple times (seed removed),
   # we see that the average lifespan for our members are:
   # 12.11028
   # 12.36846
   # 12.16028
   # 12.21278
   # 12.29969
   # (That took a long time to get!), thus the average life expectancy is around:
   mean(  12.11028
          , 12.36846
          , 12.16028
          , 12.21278
          , 12.29969)
   #12.11028
   
   # Now I do not have a government index linked bond with a 12 year duration, but 
   # we do have I2033 and I2038, and if we compare the yield of these 2 with
   # conventional government bonds of almost the same term, we have that the
   # market's future expectation of inflation lies around 3.4%
   
   # So as to make a guess, let's set the increase at 4%
   # Given the relevance of this increase, I do not think it is necessary to
   # set a more accurate representation of future inflation, though I do
   # think have an increase is relevant enough to the point of including it
   # in the model.
   }
   # This is to confirm the multiplication I need to apply later
   {temp1 <- c(1.1,1.2,1.3)
   temp2 <- matrix(c(1,2,3,1,2,3,1,2,3), nrow = 3, byrow = TRUE) 
   t(temp1 * t(temp2))
   
   # what I'm looking for was
   # 1.1  2.4  3.9
   # 1.1  2.4  3.9
   # 1.1  2.4  3.9
   # And that's what I got
   
   }
   
   # Now, let's include increases....
   {
   #Now to generate the vector of benefit increases
   #Set the increase as a value between 0 and 100
   increase <- fixed_increase_rate
   increase_vec <- c(1)
   for(i in 1:(length(member.base)-5)) {
     increase_vec <- c(increase_vec, (increase_vec[i])*(1+increase/100))
   }
   }
   
   # Now to generate the benefit values:
   {
   level_of_benefit <- 12000
   
   benefit.base <- t(increase_vec * t(level_of_benefit*member.base[,-c(1:4)]))
   }
   # Now we have the benefits, let's tidy it up
   
   benefit.base <- as.data.frame(cbind(member.base[,1:4], benefit.base))
   
   #Calculating the EPV and Fund
   {
   # Discount rate 
   
   v <- (1+increase/100) / (1 + rate_for_discounting/100)
   
   # Identify the columns with the yearly cashflows
   year.cols <- grep("^\\d{4}$", names(benefit.base))
   
   # Calculate Expected Present Value (EPV) for each member
   benefit.base$EPV <- apply(benefit.base[, year.cols], 1, function(cashflows) {
     
     n <- sum(cashflows > 0)
     
     sum(cashflows[1:n] * v^(0:(n-1)))
   })
   
   # View results
   benefit.base[, c("Member", "Age", "Lifetime", "EPV")]
#/////////////////////////////////////////////////////////////////////////////   
   #Edit:
   #Avoiding scientific notation
   options(scipen = 999)
   #Pulling relevant benefits
   benefit.base <- benefit.base[,- ncol(benefit.base)]
   
   #Also adding a risk margin for lower than expected mortality
   risk.margin <- EPV.mort.risk.margin
   
   #Pulling and adjusting relevant  Male mortality table
   {
     mort.table <- Male.Mortality.Table[,c(1,cut_off+1)]
     mort.table <- cbind(mort.table[,1], mort.table[,2]*(1- risk.margin/100))
     mort.table[nrow(mort.table),2] <- 1
     colnames(mort.table) <- c("Ages", "qx")
     #Pulling ages for table construction
     ages <- mort.table[,1]
     #male.px column
     column.male.px <- 1- mort.table[,2]
     #Creating a male.px table
     male.px.table <- cbind(ages,column.male.px)
     #Creating male.tpx column
     
     column.male.tpx <- c(1)
     for (i in 1:length(column.male.px)){
       
       p <- column.male.px[i]
       t <- column.male.tpx[i]
       tp <- p*t
       column.male.tpx <- c(column.male.tpx,tp)
     }
     
   }
   #Pulling and adjusting relevant  Female mortality table
   {
     mort.table <- Female.Mortality.Table[,c(1,cut_off+1)]
     mort.table <- cbind(mort.table[,1], mort.table[,2]*(1- risk.margin/100))
     colnames(mort.table) <- c("Ages", "qx")
     mort.table[nrow(mort.table),2] <- 1
     #Pulling ages for table construction
     ages <- mort.table[,1]
     #female.px column
     column.female.px <- 1- mort.table[,2]
     #Creating a female.px table
     female.px.table <- cbind(ages,column.female.px)
     #Creating female.tpx column
     
     column.female.tpx <- c(1)
     for (i in 1:length(column.female.px)){
       
       p <- column.female.px[i]
       t <- column.female.tpx[i]
       tp <- p*t
       column.female.tpx <- c(column.female.tpx,tp)
     }
     
   }
   
   # Removing mort.table to trigger errors if gender adjusted tables aren't used below
   remove("mort.table")
   #Discounting columns
   v.column <- v^c(1:(length(member.base[,-c(1:4)])))
   
   #index.converter
   index.conv <- Male.Mortality.Table[1,1] - 1
   # I used male here, but the converter is the same for Male and Female
   
   EPV <- c()
   
   for (i in 1:nrow(benefit.base)){
     member.age <- benefit.base[i,2]
     member.gender <- benefit.base[i,3]
     
     if(member.gender==2){
       px.table <- female.px.table
       column.tpx <- column.female.tpx
       column.px <- column.female.px
     }else{
       px.table <- male.px.table
       column.tpx <- column.male.tpx
       column.px <- column.male.px
     }
     
     member.px.part1 <- c(   px.table[-c(1:(member.age - index.conv-1),nrow(px.table)),2] )
     fill.in.years <- (ncol(member.base) - 4 - length(member.px.part1))
     member.px.part2 <- rep(0,max(fill.in.years+1,1))
     member.px <- c(member.px.part1,member.px.part2)
     
     
     member.tpx.part1 <- c((column.tpx[-c(1:(member.age - index.conv-1))]))/column.tpx[(member.age - index.conv)]
     member.tpx.part2 <- rep(0,max(fill.in.years+1,1))
     member.tpx <- c(member.tpx.part1[-length(member.tpx.part1)],member.tpx.part2)
     member.v <- v^c(1:length(member.px))
     
     member.epv <- sum(member.v * member.px * member.tpx[-length(member.tpx)] * level_of_benefit)
     
     EPV <- c(EPV, member.epv)
   }
   
   # Let's make a complete EPV data frame for each member
   EPV <- as.data.frame(cbind(benefit.base[,c(1:4)], EPV))
   

   
   
   
   }
 }
 
 # The results of this model is
 # benefit.base
 # EPV
 # Fund
 
 # I have to stress that the mortality rates in the first column is not our
 # member base's current mortality rate, but rather the 30th column, the cut_off
 # point in the member base model.
 
 # Now to keep only the main items in the environment
 rm(list = setdiff(ls(), c("inital.members","simulations","Bond.Proportion","interest", "fixed_increase_rate","reference.population.age","EPV.mort.risk.margin","coupon.rate", "Feature","Original.Fund", "Fund","improv.factor","control","cut_off","Male.Mortality.Table",
                           "Female.Mortality.Table","rate_for_discounting","member.base", 
                           "benefit.base", 
                           "EPV", 
                           "interest","loss","sim", "tests", "BOND","FUND", "ruin","results","results2","results3","Sensitivity_tests_loop_counter")))

 
 
 
 
 
 
 
 
 
 
 
 
 # Next, let's create the proportions needed for calculating the coupons
 # paid at each time point.
 
 {
 
 # We'll need to decide on a reference population, both age and gender. First, 
 # the assumption on the reference population's age:
 
 reference.population.age <- reference.population.age
 
 # Explaining reference population gender assumption:
 {
 # For the coupons, you'll need the realized tpx's of Male and Female lives
 # But, the bond's coupon dynamics does not allow for a distinction between
 # Male and Female populations, but rather a mix.
 
 # Thus we can make the assumption that the proportion is calculated
 # on the same ratio of Male to Female lives as in the 1st year of
 # our member base, as that will be the closest match we can make the bond 
 # to the liabilities.
 }
 
 # So we get the ratio's:
 {
 male.ratio <- mean(member.base$Gender == 1)
 female.ratio <- 1 - male.ratio 
 }
 
 # Now we get the tpx's of our reference population
 
 # Index converter to get the indexed age correct
 index.conv <- Male.Mortality.Table[1,1] - 1
 
 # Row of the reference population's age
 row.reference.pop.age <- reference.population.age - index.conv
 
 
 
 # Now we need to get the step wise qx's from our row.reference.pop.age down
 # to the terminal age, for each the males and females
 
 
 # Keep in mind, our current population's mortality rates are on the 31st column
 # (30th column of the rates, and the 1st column is age, hence 31st column)
 # of the mortality table, given cut_off is still equal to 30. Though, I made
 # it dynamic so it still works with a different cut_off value
 {
 male.qx <- c() # Creating an empty qx for the for loop below
 female.qx <- c()
 
 # Determining the amount of Rows Below the Reference Age
 rbra <- nrow(Male.Mortality.Table) - (row.reference.pop.age)
 
 row.age.forloop <- row.reference.pop.age # Creating a new row index age to use in loop
 
 for (i in (cut_off+1):(cut_off+rbra+1)) {
   
   male.qx <- c(male.qx,Male.Mortality.Table[row.age.forloop,i])
   female.qx <- c(female.qx,Female.Mortality.Table[row.age.forloop,i])
   
   row.age.forloop <- row.age.forloop + 1
   
 }
 
 
 }
 
 # Now to get the relevant px's and finally the tpx's of both genders
 {
 male.px <- 1- male.qx
 female.px <- 1- female.qx
 
 male.tpx <- c(1)
 female.tpx <- c(1)
 
 # male tpx loop
 for (i in 1:length(male.px)){
   
   p <- male.px[i]
   t <- male.tpx[i]
   tp <- p*t
   male.tpx <- c(male.tpx,tp)
 }
 
 # female tpx loop
 for (i in 1:length(female.px)){
   
   p <- female.px[i]
   t <- female.tpx[i]
   tp <- p*t
   female.tpx <- c(female.tpx,tp)
 }
 }
 
 # Thus the proportions used for the coupon calculation is:
 coupon.prop <- male.ratio*male.tpx + female.ratio*female.tpx
 
 # Now to ensure that the length of the coupon cashflows are equal to the
 # length of the simulated years. It looks funny since it is being adjusted exactly to the
 # required length down below
 
 coupon.prop <- if (length(coupon.prop) < (ncol(benefit.base)-4)) c(coupon.prop, rep(0,
 (ncol(benefit.base)-4) - length(coupon.prop))) else coupon.prop[1:(ncol(benefit.base)-4)]
 
 }
 # The important outputs of the model above are:
 #coupon.prop
 
 # Now to keep only the main relevant items in the environment
 rm(list = setdiff(ls(), c("inital.members","simulations","Bond.Proportion","interest", "fixed_increase_rate","reference.population.age","EPV.mort.risk.margin","coupon.rate", "Feature","Original.Fund", "Fund","improv.factor","control","cut_off","Male.Mortality.Table",
                           "Female.Mortality.Table","rate_for_discounting","member.base", 
                           "benefit.base", 
                           "EPV",
                           "coupon.prop", "interest", "loss","sim", "tests", "BOND","FUND", "ruin","results","results2","results3","Sensitivity_tests_loop_counter")))
 


# Now to simulate the Fund
{
Fund <- sum(EPV[,5])
Ooriginal.Fund <- Fund
Original.Fund <- c(Original.Fund, Fund)
#I assumed "coupon.prop" is the rate that we times by the fixed percentage and is equivalent to lx/l(65)

# Here I added the feature of the coupon rate being set as the prop of the first
# year's liabilities to the total fund
# fixed_percentage <- ifelse(Feature == 1, sum(benefit.base[,4])/Fund, coupon.rate/100) #please change to number appropriate
# Recording the coupon rate for later analysis
fixed_percentage <- coupon.rate/100

Bond.Prop <- Bond.Proportion/100 # The proportion of the Fund used to purchase the longevity bond
notional <- Fund * Bond.Prop # Bond Value
Fund <- Fund - notional
longevity_bonds_cashflows <- fixed_percentage*notional*coupon.prop

for(i in 2:(ncol(benefit.base)-3)){
  
  Fund[i] <- (Fund[i-1])  * (1+(interest * ifelse(Fund[i-1] > 0, 1, 0)/100)) - sum(benefit.base[,i+3]) + longevity_bonds_cashflows[i-1]
  
  
}

discount_rate <- rate_for_discounting/100 #just put something in to work for now 
years <- seq_along(longevity_bonds_cashflows)
discount_factors <- 1 / ((1 + discount_rate)^years)
PV_longevity_bond <- sum(longevity_bonds_cashflows * discount_factors)
}





#/////////////////////////////////////////////////////////////////////////////


# Finally over here we add the second half of the VAR and Prob of ruin code



# Recording ruin and loss for the l th simulation
loss <- c(loss, Ooriginal.Fund - min(Fund))
ruin <- c(ruin, ifelse(sum(Fund[-1] < 0) > 0,1,0))
Fund.adj <- c(Fund, rep(0,ncol(FUND)-length(Fund)))
FUND <- rbind(FUND,Fund.adj)

if(Bond.Prop > 0) {
Bond.adj <- c(longevity_bonds_cashflows, rep(0,ncol(BOND)-length(longevity_bonds_cashflows)))
BOND <- rbind(BOND,Bond.adj)
}


# Creating a plot to visualize the script's current position
x_simulation_vec <- c(0:sim)
y_tests_vec <- rep(Sensitivity_tests_loop_counter,length(x_simulation_vec))
plot(x = x_simulation_vec, y = y_tests_vec, xlim = c(1,simulations), ylim = c(Sensitivity_tests_loop_counter - 1, Sensitivity_tests_loop_counter + 1), main = "Model progress", xlab = paste("Simulations completed out of ", simulations), ylab = paste("Test ", Sensitivity_tests_loop_counter, " out of ",tests," in progress"), type = "l", lwd = 5, col = "#0E4B75", yaxt = "n")
}

# And now for the VAR@95% conf and Prob of Ruin calculation

VAR <- as.numeric(quantile(loss, prob = 0.95))
Prob.of.ruin <- mean(ruin)

# Final outputs of the model are:

rm(list = setdiff(ls(), c("inital.members","simulations","Bond.Proportion","interest", "fixed_increase_rate","reference.population.age","EPV.mort.risk.margin","coupon.rate", "Feature","Original.Fund", "Fund","improv.factor","control","cut_off","Male.Mortality.Table",
                          "Female.Mortality.Table","rate_for_discounting","member.base", 
                          "benefit.base", 
                          "EPV",
                          "coupon.prop", "interest", "loss","sim", "tests", "BOND","FUND", "ruin","results","results2","results3","Sensitivity_tests_loop_counter",
                          "Bond.Prop",
                          "fixed_percentage",
                          "Fund",
                          "longevity_bonds_cashflows",
                          "notional",
                          "Original.Fund", "Fund",
                          "Prob.of.ruin",
                          "PV_longevity_bond",
                          "VAR")))
