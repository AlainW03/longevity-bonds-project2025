# So the purpose of this script is to set up the sim for the actual
# fund that will be used to pay the benefits of our members

# To do that, we will first need the actual benefit amounts to be paid
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


# Pulling the member base model

library(readxl)
member.base <- read.csv("Generated State of Life Member Base.csv")
# I don't like the X's in front of the years, so I'm removing them like this:
colnames(member.base) <- c(colnames(member.base[,1:3]), 2014:(2014 + length(colnames(member.base))-4))
# Of course, check whether the first year is actually 2014 first

#Also, the member base is excessively long, let's trim it down
 member.base <- member.base[,1:(max(member.base[,3])+4)]

# Now, let's include increases....
# Now I have to justify the inflation increase!

# Running the member base sim with 10 000 members multiple times (seed removed),
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
 
# This is to confirm the multiplication I need to apply later
temp1 <- c(1.1,1.2,1.3)
temp2 <- matrix(c(1,2,3,1,2,3,1,2,3), nrow = 3, byrow = TRUE) 
t(temp1 * t(temp2))

# what I'm looking for was
# 1.1  2.4  3.9
# 1.1  2.4  3.9
# 1.1  2.4  3.9
# And that's what I got



#Now to generate the vector of benefit increases
#Set the increase as a value between 0 and 100
increase <- 4
increase_vec <- c(1)
for(i in 1:(length(member.base)-4)) {
  increase_vec <- c(increase_vec, (increase_vec[i])*(1+increase/100))
}

# Now to generate the benefit values:

level_of_benefit <- 1000

benefit.base <- t(increase_vec * t(level_of_benefit*member.base[,-c(1:3)]))

# Now we have the benefits, let's tidy it up

benefit.base <- as.data.frame(cbind(member.base[,1:3], benefit.base))


#-------------------------------------------------------------------------------
#Calculating the EPV
#-------------------------------------------------------------------------------
#head(benefit.base)

# Discount rate 
i <- 0.05
v <- 1 / (1 + i)

# Identify the columns with the yearly cashflows
year.cols <- grep("^\\d{4}$", names(benefit.base))

# Calculate Expected Present Value (EPV) for each member
benefit.base$EPV <- apply(benefit.base[, year.cols], 1, function(cashflows) {

  n <- sum(cashflows > 0)
  
  sum(cashflows[1:n] * v^(0:(n-1)))
})

# View results
benefit.base[, c("Member", "Age", "Lifetime", "EPV")]
