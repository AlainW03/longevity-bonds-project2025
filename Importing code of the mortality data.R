library(readxl)


# So I first worked in Excel, constructing the mortality data that we need
# For the years prior to 2016, I made sure to use the last available
# deaths and exposure data for each of those years and adding them as their own
# columns in the left most side.

#Some of those latest available data have age ranges even shorter than that of
# the 2013 to 2020 data. In such cases, I used the latest possible data
# available for the ages that are the difference between the 55 to 100 age
# range and the shorter age range.

# For example, for men's mortality 2010, the latest available data, in the
# 2010 to 2017 CMI Mortality experience dataset, only included
# for age ranges from age 57 to age 100. So the death and exposure (D&E) data for ages
# 57 to 100 are based on that latest available CMI data, but for ages 55 and 56
# in 2010, the latest available data is located in the 2009 to 2016. 
# The same procedures were followed for female mortality data sets with
# shorter age ranges. So far, 2010 is the only one I've found to 
# where the latest 2010 D&E data for both males and females datasets had shorter
# age ranges than latest 2013 D&E data.

# Furthermore, I only included the data from 2008 to 2023. This is to keep
# the data's range long enough for the results to be significant, but also
# short enough so that the results are more relevant. It was also around 2008 
# when the CMI team ramped up to more modern techniques and methods, hence 
# why the data is more relevant to all the way back in 2008.

#Importing the deaths and exposure data of the annuitants in payment 
# in self administered pension funds population
Male_exposure_Ann <- read_xlsx("CMI Mort Data.xlsx", sheet = "M L Exposure")
Male_deaths_Ann <- read_xlsx("CMI Mort Data.xlsx", sheet = "M L Deaths")
Female_exposure_Ann <- read_xlsx("CMI Mort Data.xlsx", sheet = "F L Exposure")
Female_deaths_Ann <- read_xlsx("CMI Mort Data.xlsx", sheet = "F L Deaths")

# Correcting the column names
colnames(Male_exposure_Ann) <- c("Age", 2013:2020)
colnames(Male_deaths_Ann) <- c("Age", 2013:2020)
colnames(Female_exposure_Ann) <- c("Age", 2013:2020)
colnames(Female_deaths_Ann) <- c("Age", 2013:2020)

# Importing the calculated qx base mortality values
# Calculated using CMI's dx and lx values over the time periods
Base_Male_Mort_qx <- read_xlsx("CMI Mort Data.xlsx", sheet = "M Base qx")
Base_Female_Mort_qx <- read_xlsx("CMI Mort Data.xlsx", sheet = "F Base qx")

#The annuitant data only includes year 2013 to 2020, but the base mortality
# extends from 1981 to 2023, so I'll be limiting the data to that range

#First extracting the age column
age_male_col <- Base_Male_Mort_qx[,1]
age_female_col <- Base_Female_Mort_qx[,1]

#Temporarily removing the age colum
Base_Male_Mort_qx <- Base_Male_Mort_qx[,-c(1)]
Base_Female_Mort_qx <- Base_Female_Mort_qx[,-c(1)]

#Now limiting the year range while putting the age column back in:
Base_Male_Mort_qx <- cbind(age_male_col, Base_Male_Mort_qx[,-c((1981:2012)-1980, (2021:2023)-1980)])
Base_Female_Mort_qx <- cbind(age_female_col,Base_Female_Mort_qx[,-c((1981:2012)-1980, (2021:2023)-1980)])

#Correcting the base mortality's column names
colnames(Base_Male_Mort_qx) <- c("Age", 2013:2020)
colnames(Base_Female_Mort_qx) <- c("Age", 2013:2020)


# Now to construct the mortality tables


  # Explaining the structure of the stitched mortality table
  {# So the Mortality tables are going to be a bit of a Frankenstein's monster
    
    # For the male mortality data, age 16 - 54 is going to be base mortality data,
    # and for the female mortality data it will be ages 16 - 58
    
    # The reason is that the mortality data for the annuitants in those age ranges
    # are all lumped in together, but our lag period depends on those younger ages
    
    # So this mortality table and the model is designed such that, even though
    # the mortality at the linkage is discontinuous, only the younger range
    # will be used for the lag period, while the older range will be focused 
    # on the actual simulated cashflows.
    
    # The older range used in the lag period is deemed to be acceptable
    # since the purpose of the lag period is to set the shape of the age distribution
    # of the member base, which is ultimately dominated by the older age range.
    
    # The 2013 data of the 2013 to 2020 mortality experience
    # shows a significant spike in mortality rates for ages 93 to 96. 
    # This outlier was too significant to leave alone, so we replace those
    # age's D&E data with that in the 2013 data of the 2012 to 2019 mortality 
    # experience. Although the spike can still be detected after the replacement,
    # this smoothed the data over those ages are now within acceptable limits.
    
  }
  
  # Extracting the mortality data for the younger male mortality
  Base_younger_male_mort_qx <- Base_Male_Mort_qx[c((16:54)-15),]
  # Extracting the mortality data for the younger female mortality
  Base_younger_female_mort_qx <- Base_Female_Mort_qx[c((16:58)-15),]
  
  # Now creating the Annuitant Mortality tables
  
  Hist_male_mort_ann <- 1 - exp(-Male_deaths_Ann[,-c(1)] /Male_exposure_Ann[,-c(1)])
  Hist_male_mort_ann <- cbind(c(55:100), Hist_male_mort_ann)
  colnames(Hist_male_mort_ann) <- c("Age", 2013:2020)
  
  Hist_female_mort_ann <- 1 - exp(-Female_deaths_Ann[,-c(1)] /Female_exposure_Ann[,-c(1)])
  Hist_female_mort_ann <- cbind(c(59:100), Hist_female_mort_ann)
  colnames(Hist_female_mort_ann) <- c("Age", 2013:2020)
  
  
  # Now to make our mixed mortality rates
  
  Male.Mort.data <- rbind(Base_younger_male_mort_qx,Hist_male_mort_ann)
  Female.Mort.data <- rbind(Base_younger_female_mort_qx, Hist_female_mort_ann)
  
  
  #write.csv(Male.Mort.data, file = "Male_Mort_data.csv", row.names = FALSE)
  #write.csv(Female.Mort.data, file = "Female_Mort_data.csv", row.names = FALSE)
