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
colnames(member.base) <- c(colnames(member.base[,1:3]), 2014:(2014 + ncol(member.base)-4))
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
Mortality.Table <- read.csv("LC Mortality Data.csv")
#NEED TO RUN ALL THE CODE FROM MEMBER BASE TO GET FINAL MEMBER BASE 
final.member.base <- as.data.frame(final.member.base)

library(dplyr)
library(purrr)
library(tidyr)

# =========================
# Inputs you control
# =========================
i <- 0.07          # annual effective discount rate
j <- 0.04          # annual escalation rate
monthly_payment <- 1000
terminal_age <- 120 # This age is not necessarily guaranteed, please be careful

# =========================
# 1) Prepare mortality table: keep only Age and 2014, rename to qx, build px
# =========================
{
#mortality2014 <- Mortality.Table %>%
#  select(Age, `X2043`) %>% # Wrong column, and column name. Changed "2014" to "X2043"
#  rename(qx = `X2043`) %>% # Wrong column, and column name. Changed "2014" to "X2043"
# arrange(Age) %>%
# filter(Age <= terminal_age)

# Here is a much shorter code:
mortality2014 <- Mortality.Table[,c(1,31)]
colnames(mortality2014) <- c("Age","qx")
} #From what I can tell, this pulls the qx's of our current members


{
# force qx = 1 at terminal age, so the process ends
if (!any(mortality2014$Age == terminal_age)) {
  mortality2014 <- bind_rows(mortality2014, tibble(Age = terminal_age, qx = 1))
} else {
  mortality2014$qx[mortality2014$Age == terminal_age] <- 1
}

mortality2014 <- mortality2014 %>%
  mutate(px = pmax(0, 1 - qx)) %>%
  arrange(Age)

# sanity: ages must be consecutive integers
stopifnot(all(diff(mortality2014$Age) == 1))

} # This code is unnecessary, the mortality table is by design made to
# give the the exact output this piece of code is trying to enforce




# =========================
# 2) Prepare member base: select the id and the age only
#    Adjust the column names here if yours differ
# =========================
{
members <- final.member.base %>%
  transmute(Member = Member, StartAge = Age) %>%   # change if your id column has a different name
  mutate(StartAge = as.integer(StartAge))
}

# This code does the exact same thing ??
members <- final.member.base[,c(1,2)]
colnames(members) <- c("Member", "StartAge")


# =========================
# 3) Survival paths by starting age used in the book
#    We compute once per StartAge to avoid unnecessary rows
# =========================
unique_start_ages <- sort(unique(members$StartAge))  # Why?

survival_by_age <- map_dfr(unique_start_ages, function(x){
  mt <- mortality2014 %>% filter(Age >= x)
  tibble(
    StartAge    = x,
    AttainedAge = mt$Age,
    qx          = mt$qx,
    px          = mt$px,
    surv        = cumprod(mt$px)   # {}_t p_x for t = 1.. (alive to start of each year t)
  )
})

# If you want survival by member, join (this will be many rows, which is normal)
survival_by_member <- members %>%
  left_join(survival_by_age, by = "StartAge")

# =========================
# 4) Annuity factors by starting age using recursion at the net rate
#    Growth j can be priced by using i_net = (1+i)/(1+j) - 1 for a level stream
#    We compute immediate timing (end of year payments)
# =========================
i_net <- (1 + i) / (1 + j) - 1
v_net <- 1 / (1 + i_net)

ages <- mortality2014$Age
px_vec <- mortality2014$px
names(px_vec) <- as.character(ages)

# a_immediate[x] satisfies: a_x = v_net * p_x * (1 + a_{x+1})
a_immediate <- numeric(length(px_vec))
names(a_immediate) <- names(px_vec)
a_immediate[as.character(terminal_age)] <- 0

for (age in rev(ages[-length(ages)])) {
  p <- px_vec[as.character(age)]
  a_next <- a_immediate[as.character(age + 1)]
  a_immediate[as.character(age)] <- v_net * p * (1 + a_next)
}

annuity_factors <- tibble(
  StartAge     = as.integer(names(a_immediate)),
  a_immediate  = as.numeric(a_immediate)
) %>%
  filter(StartAge %in% unique_start_ages)

# Redundant code above, though may be useful at a later stage

# =========================
# 5) EPV per member for monthly 1 000 with annual escalation j
#    We price as 12 000 per year at net rate, then scale by the survival factor a_x
# =========================
epv_per_member <- members %>%
  left_join(annuity_factors, by = "StartAge") %>%
  mutate(EPV = 12 * monthly_payment * a_immediate) %>%
  select(Member, StartAge, EPV)

# Optional checks
stopifnot(nrow(epv_per_member) == nrow(members))
# View head(epv_per_member); head(survival_by_member)
