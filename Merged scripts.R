# This is the merged model of the seperate individual models.

# Why do we do this?
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
# each other. This means no creating csv files, no reading column names, 
# no cleaning of the data, no re-determining the dimensions, etc.

# Luckily for me, my style of coding has set the places where the links should 
# happen close to the start or end of the script, with very few exceptions here
# and there.

# Let's go!

# First, let's set a script wide seed for consistent results. As it is now, 
# the seeds are giving us only one possible result for each piece of the model.

set.seed(780)

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
  
  #########################################################################################
  Mort.2014 <- read_xlsx("CMI Tables Published 2014.xlsx", sheet = 4)
  #Mort.2014 <- read_excel("Research project code/CMI Tables Published 2014.xlsx", sheet = 4)
  
  #Mort.2014 <- read_excel("Research project code/CMI Tables Published 2014.xlsx", sheet = 4, skip =  4) 
  # ABOVE IS EASY WAY TO CLEAN
  ##########################################################################################
  
  #Ok, that looks nasty, let's clean that up...
  
  # I'm creating a function designed to clean the CMI table
  
  Clean.CMI.Table <- function(CMI_Table, row_of_col_names){
    # To avoid confusion, let's rename the table to be cleaned.
    Table.to.clean <- CMI_Table
    # Now to extract the column names to be preserved
    col_names <- colnames(Table.to.clean) <- c(Table.to.clean[row_of_col_names,])
    # Now to remove all the rows above the row with the column names,
    # including the column names themselves, so that we are left only with
    # the data.
    Table.to.clean <- as.data.frame(Table.to.clean[-c(1:row_of_col_names),])
    
    # Great, now, the data is a char for some reason, so as to ensure everything
    # is numeric, we extract each column into a vector, transform the vector,
    # and snap it one by one into a data.frame
    
    # This table here is created just to have the columns snap on to something
    Clean.table <- as.data.frame(matrix(0, nrow = nrow(Table.to.clean), ncol = 1))
    
    # Looping through each column, transforming them into numerics
    for(i in 1:ncol(Table.to.clean)) {
      
      Extracted_column <- Table.to.clean[,i]
      Numeric_column <- as.numeric(Extracted_column)
      Clean.table <- cbind(Clean.table,Numeric_column)
      
      
    }
    
    # Let's remove that first row of 0's
    Clean.table <- Clean.table[,-c(1)]
    
    # And finally attach the original column names
    colnames(Clean.table) <- col_names
    
    return(Clean.table)
    
  } # End of Table Cleaning function
  
  # The function is ready, and should easily apply to all the CMI mortality
  # tables we may use.
  
  # Let's clean our 2014 CMI Mortality data
  Mort.2014 <- Clean.CMI.Table(Mort.2014,row_of_col_names = 4)
  
  #Ok, now that our CMI table is clean, let's split it up to get the respective 
  # mortality tables
  
  Male.Mort.14 <- Mort.2014[,c(1,6)]
  Female.Mort.14 <- Mort.2014[,c(1,2)]
  
  
  
  #Since I do not have all the data I need to build it, I'll make dummy data
  # in the mean time and just get the model up and running, leaving space
  # for the correct data to be inserted.
  
  
  
  
  # Here I create the dummy data, which will be called Male.Mort.data
  {
    
    Male.Mort.data <- as.data.frame(matrix(0, nrow = nrow(Male.Mort.14), ncol = 1))
    cons.mort <- Male.Mort.14[,2]
    #Removing seed now
    #set.seed(780)
    

    # Here our dummy data is decreasing at a steady but random rate
    # I don't really care what the dummy data looks like, since we will
    # be replacing it with the real thing in due time. However, this is similar
    # with the assumption in the Lee-Carter model's approach, which I explain below
    
    for(i in c(1:11)){
      
      rand.improv <- runif(nrow(Male.Mort.data),min = 0, max = 10)/10000000
      mort.improv <- cons.mort - rand.improv
      Male.Mort.data <- cbind(Male.Mort.data, mort.improv)
      cons.mort <- mort.improv
      
    }
    
    #
    Male.Mort.data <- cbind(Male.Mort.14[,1], Male.Mort.data[,-1])
    colnames(Male.Mort.data) <- c("Age", c(2014:2024))
    Male.Mort.data[nrow(Male.Mort.data),] <- c(120, rep(1,11))
  }
  
  # With this, I can start constructing the Lee carter model 
  
  
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
  
  # This is whare we determined the parameters of the Lee-Carter Model
  
  {
  #First transform the data to only contain the rates:
  mort.rates.data <- as.matrix(Male.Mort.data[,-1])
  
  # Of course, we'll have to decide the forecast length:
  
  forecast.length <- nrow(Male.Mort.data) - (ncol(Male.Mort.data)-1)
  
  #Now we have to create a model data set that demography can use.
  # The transformation is done with the demodata function.
  
  
  demo <- demogdata(
    data =  mort.rates.data,
    pop = matrix(1000000, ncol = ncol(mort.rates.data), nrow = nrow(mort.rates.data))
    #Just made up a large population base so that the model can do its thing
    ,ages = Mort.2014[,1],
    years = 2014:2024,
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
  
  # I can either do this manually, or by clever use of manual overriding.
  # Thing is, I can just make a kt model, adjust the drift parameter, and continue
  # using the forecasting function with the adjusted model settings.
  
  # Reason is that to allow the adjustment to be simple enough to control, while
  # not too simple as to make it too unrealistic.
  
  # I will have a variable, called improv.factor (improvement factor),
  # which will be a value between -100 and 100
  # a value below 0 causes a worsening mortality rate, and a value above 0 causes a
  # better mortality rate over time.
  
  improv.factor <- 10
  
  #Now let's forecast the kt's
  {
    model <- auto.arima(as.ts(kt_hist),d = 1)
    drift <- model$coef * (1 + improv.factor/100)
    sigma2<- model$sigma2
    set.seed(780)
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
  
  #Let's tidy it up:
  
  mortality.forecast <- cbind(Mort.2014[,1],mortality.forecast)
  colnames(mortality.forecast) <- c("Ages",2025:(2024+forecast.length))
    
    
  
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
    combined <- cbind(Male.Mort.data[-105,-1],mortality.forecast[-105,-1])
    mat <- as.matrix(-log(combined))
    
    
    # Load necessary library
    library(plotly)
    
    # Create a matrix with values > 1
    
    # Create grid for x and y axes
    x <- 2014:(2024+ forecast.length)
    y <- 16:120
    
    
    
    # Plot using plotly
    plot_ly(
      x = ~x, y = ~y, z = ~mat,
      type = "surface"
    ) %>%
      layout(
        title = "3D Surface Plot of Matrix",
        scene = list(
          xaxis = list(title = "Columns"),
          yaxis = list(title = "Rows"),
          zaxis = list(title = "Values")
        )
      )
    
  }
  
  # Now we complete our dummy mortality data by combining the dummy rates
  # and the forecasted rates
  
  Final.Mort.table <- as.data.frame(cbind(Male.Mort.data, mortality.forecast[,-1]))
  
  #write.csv(Final.Mort.table, "LC Mortality Data.csv", row.names = FALSE)
  
}

# The dummy mortality rates are called Final.Mort.table
# To link the model above to the model below, we rename Final.Mort.table to 
# Mortality.Table
Mortality.Table <- Final.Mort.table
# Run this line below to remove all but Mortality.Table from your environment
 rm(list = setdiff(ls(), "Mortality.Table"))










###################################################################################
  #Setting number of members at time 0
  num.members <- 10
  # This is where we mainly control the size of this script, hence I moved it here
####################################################################################


 
 
 
 
 
 



# Third, we add our member.base model


{
  #library(readxl)
  #Extracting mortality table
  #Mortality.Table <- read.csv("LC Mortality Data.csv")
  # Not needed since this is where we linked the previous model to this model
  
  
  #Getting the lowest and highest age in the table for later use
  lowest.age <- as.numeric(  min(Mortality.Table[,1])  )
  highest.age <- as.numeric(  max(Mortality.Table[,1])  )
  
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
      
      target_mean_age <- 50
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
    #Columns must equal the years + 2 (1 for index, 1 for ages, rest for years)
    member.base <- matrix(0, nrow = num.members, ncol = num.years+2)
  
  {
    #Creating a count variable to help set rows in member base
    count <- 1
    
    #For loop runs for every member to simulate their state of life through the years
    for(i in members){
      
      #assuming everyone in the portfolio is alive at time 0
      prev_state <- 1
      
      #creating an empty row for the member info to occupy
      member_row <- c(matrix(0,nrow = 1, ncol = num.years+2))
      
      #Added 2 columns, the first the member index, the second the member's age
      member_row[1] <- i
      member_row[2] <- ages[i]
      
      #Simulating state of life for member i from column 3 onwards
      #(hence time 1 is column 3)
      for(j in 3:num.years){
        
        #Thus j-2 is the time point of the state being simulated
        
        #Unif q probability
        value <- runif(1,0,1)
        
        #Getting the current age of the member, as at point j
        #Including column 2 in here, since that is the original age
        current_age <-  sum(member_row[2:length(member_row)])
        
        #Ensuring that the member's age does not exceed 110
        
        y <- ifelse(current_age > highest.age, highest.age, current_age)
        
        
        #getting the index of the member's age in the mortality table
        index_age <- y-(lowest.age-1)
        
        #Getting qx for the member's age
        qx <- as.numeric(Mortality.Table[index_age,j-1])
        
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
    
    Lifetime <- apply(X = member.base[,3:ncol(member.base)],FUN = sum, MARGIN = 1)
    member.base <- cbind(member.base[,1:2], Lifetime, member.base[,3:ncol(member.base)] )
    
    #Giving column headings
    colnames(member.base) <- c("Member","Age", "Lifetime",1:(num.years))
    
    #Error checking for member with maximum lifetimes
    member.base[which.max(member.base[,3]),1]
    
    #Everything seems to be in order
  }
  
  # Now for the final member base where we cut the first n number of years
  
  cut_off <- 30
  
  final.member.base <- member.base[member.base[,cut_off]==1,c(1,2,3,cut_off:ncol(member.base))]
  colnames(final.member.base) <- c("Member","Age", "Lifetime",1:((ncol(final.member.base)-3  ) )   )       
  
  # of course, everyone is cut_off amount years older by this point, so we will need to adjust the ages too
  
  final.member.base[,2] <- final.member.base[,2] + cut_off
  final.member.base[,3] <- final.member.base[,3] - (cut_off-4)
  }
  
  
  #There we have it. Now I just increase the number of members, and we should have a nice
  # member base
  
  #For curiosity sake, let's look at how the distribution of ages ended up to be:
  {
  #plot(density(as.numeric(final.member.base[,2] )), main = "Density of Final Member Base Ages", xlab = "Ages")
  
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


# The result of the model above is called final.member.base
# To link the member base model to the next model, we change the name of final.member.base
# to member.base
 member.base <- as.data.frame(final.member.base)
 
 #Also, the member base is excessively long, let's trim it down
 member.base <- member.base[,1:(max(member.base[,3])+4)]
 
# Run this line below to remove all but Mortality.Table and member.base from your environment
 rm(list = setdiff(ls(), c("Mortality.Table","member.base")))
