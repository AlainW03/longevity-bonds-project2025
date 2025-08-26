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
  
  
  Mort.2014 <- read_xlsx("CMI Tables Published 2014.xlsx", sheet = 4)
  #Mort.2014 <- read_excel("Research project code/CMI Tables Published 2014.xlsx", sheet = 4)
  
  #Mort.2014 <- read_excel("Research project code/CMI Tables Published 2014.xlsx", sheet = 4, skip =  4) 
  # ABOVE IS EASY WAY TO CLEAN
  
  
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

# Run this line below if you want to remove all but Final.Mort.table from your environment
# rm(list = setdiff(ls(), "Final.Mort.table"))



