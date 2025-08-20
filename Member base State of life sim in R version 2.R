library(readxl)

#Extracting mortality table

 Mortality.Table <- read.csv("LC Mortality Data.csv")
 


#Getting the lowest and highest age in the table for later use
lowest.age <- as.numeric(  min(Mortality.Table[,1])  )
highest.age <- as.numeric(  max(Mortality.Table[,1])  )

#Setting seed for consistency
set.seed(780)
  #Setting number of members at time 0
  num.members <- 1000
  
  #Setting number of years the portfolio is expected to be active (max is 70 years
  #since terminal age is 110 for now, and the min age is 40 for now)
  num.years <- highest.age - lowest.age
  colnames(Mortality.Table) <- c("Age", 2014:(2014+num.years))
  #creating the index for our members
  members <- c(1:num.members)
{
  #So the distribution of ages in a pension fund isn't necessarily uniform
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
  

  {
    #Setting a temporary seed to generate ages (should remove in final version)
    #set.seed(780)
    
    #Now to generate a distribution for our ages, the beta distribution is 
    # suitable since it has 2 easy parameters that I can control
    
    #To manipulate the shape, I set a targeted mean age, and then 
    # control the Beta (shape2) to increase or decrease the thickness 
    # of the upper tail
    
    target_mean_age <- 50
    target_mean_percent <- (target_mean_age- lowest.age)/num.years
    shape2 <- 25
    shape1 <- (target_mean_percent* shape2)/(1- target_mean_percent)
    
    age_dist <- rbeta(num.members, shape1 = shape1, shape2 = shape2)
    
    # Now to convert the distribution to our ages
    ages <- round(lowest.age + age_dist*num.years, 0)
    
    # Quickly plotting the age distribution to visually verify the shape
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
  #Creating an empty space for member base to occupy later
  #Columns must equal the years + 2 (1 for index, 1 for ages, rest for years)
  member.base <- matrix(0, nrow = num.members, ncol = num.years+2)

}


#Alright, now to start the simulation.
# The simulation will start at the current set of ages, but afterwards
# I will cut the first cut_off amount years out, as at that point I expect the distribution
# of the ages to have taken the shape one would see in a pension scheme.



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
colnames(final.member.base) <- c("Member","Age", "Lifetime",2014:(2013+ (ncol(final.member.base)-3  ) )   )       

# of course, everyone is cut_off amount years older by this point, so we will need to adjust the ages too

final.member.base[,2] <- final.member.base[,2] + cut_off
final.member.base[,3] <- final.member.base[,3] - (cut_off-4)

#There we have it. Now I just increase the number of members, and we should have a nice
# member base

#For curiosity sake, let's look at how the distribution of ages ended up to be:
plot(density(as.numeric(final.member.base[,2] )), main = "Density of Member Base Ages", xlab = "Ages")

#Now let's take a look at the distribution of age of death
plot(density(as.numeric(final.member.base[,2] )+as.numeric(final.member.base[,3] )), main = "Density of Member Base Ages", xlab = "Ages")

# Now, I will export this final.member.base as a cvs file
# This piece of code should only run once, otherwise it will create duplicates

#write.csv(final.member.base, "Generated State of Life Member Base.csv", row.names = FALSE)

# Note that I think the column names are actually incorrect. I cut off the first n number
# of years, meaning that the time stamp is n years later than 2014.
# THough I'll ponder on this thought for a while before I make the change