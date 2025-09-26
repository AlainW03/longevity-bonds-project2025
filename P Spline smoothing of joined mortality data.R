
# Step 1: Install the pspline package if not already installed

library(pspline)
library(readxl)

# Before smoothing, I made appropriate adjustments to the mortality data
# and also did a lot of the calculations inside the excel sheet, where
# it would have been unnecessary to perform them in R.

# All adjustments and assumptions made in both the R script and the Excel
# file are declared in comments inside this script at various locations.

# Lastly, a simple P-slpine function is used as one last simple smoothing to 
# the mortality data such that it is appropriate for further use.


# This code replaces the original importing code, so I will start with c&p the
# important comments from the original importing code:

{
  
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
  
  # Explaining the structure of the stitched mortality table
  {# So the Mortality tables are going to be a bit of a Frankenstein's monster
    
    # For the male mortality data, age 16 - 54 is going to be base mortality data,
    # and for the female mortality data it will be ages 16 - 58
    
    # The reason is that the mortality data for the annuitants population,
    # which is most applicable to this research project, in those age ranges
    # are all lumped in together, but our lag period depends on those younger ages
    
    # So this mortality table and the model is designed such that, even though
    # the mortality at the linkage is discontinuous, only the younger range
    # will be used for the lag period, while the older range will be focused 
    # on the actual simulated cashflows.
    
    # The older range used in the lag period is deemed to be acceptable
    # since the purpose of the lag period is to set the shape of the age distribution
    # of the member base, which is ultimately dominated by the older age range.
    # Smoothing is done to make the jumps less prominent and the data more
    # appropriate.
    
    # The 2013 data of the 2013 to 2020 mortality experience
    # shows a significant spike in mortality rates for ages 93 to 96. 
    # This outlier was too significant to leave alone, so we replace those
    # age's D&E data with that in the 2013 data of the 2012 to 2019 mortality 
    # experience. Although the spike can still be detected after the replacement,
    # this smoothed the data over those ages are now within acceptable limits.
    
    
    # For age ranges 101 - 120, the S3 and S4 mortality tables were joined 
    # on the right side, and smoothened to keep the mortality data appropriate.
    
    # The reason for using the S3 and S4 CMI mortality data for those ages
    # are because the base mortality data significantly deviate from the
    # annuitant mortality data at high ages. The S3 and S4 data, specifically the
    # Normal type lives data, is the closest proxy of the population
    # concerned in this research project. The downside is that the S3 and S4
    # data has one set of mortality rates each that applies to a range of years.
    # S3 applies to 2009 to 2016, and S4 2014 to 2019, but our specific population
    # ranges years 2008 to 2023. Thus, S3 was joined at years 2008 to 2013, while 
    # S4 was joined at years 2014 to 2023. It is a bit of a stretch, but this is
    # the closest set that we can find on the CMI website. It would be great if S5
    # gets released soon to more accurately link 2020 - 2023. 
    
    # The specific ages at which the S series mortality was linked is different for
    # the male and female ages. After visually inspecting all of the linkages,
    # it was determined that the best linkage for the male mortality data was at
    # age 96, and for the female mortality data at age 99. This was determined
    # by determining for each the average age close to age 100 at which the 
    # age linkage was such that our population's mortality data was the closest
    # to the S series mortality data at that age.
    
  }
  
  
  
}



# Smoothing the Male Mortality Data
{
  
# For the Male data, investigations show that the linkage
  # from our annuitants mortality to the S3 / S4 mortality is
  # most optimal at age 96
  
# Extracting the relevant unsmoothed mortality data

Unsmoothed_data  <- read_xlsx("CMI Mort Data.xlsx", sheet = "M Joined Mort Data")

age <- Unsmoothed_data[,1]
age <- as.numeric(age$Age)

crude_mu <- -log(1 - Unsmoothed_data[,-1])


crude_mu_2008 <- as.numeric(crude_mu$`2008`)
crude_mu_2009 <- as.numeric(crude_mu$`2009`)
crude_mu_2010 <- as.numeric(crude_mu$`2010`)
crude_mu_2011 <- as.numeric(crude_mu$`2011`)
crude_mu_2012 <- as.numeric(crude_mu$`2012`)
crude_mu_2013 <- as.numeric(crude_mu$`2013`)
crude_mu_2014 <- as.numeric(crude_mu$`2014`)
crude_mu_2015 <- as.numeric(crude_mu$`2015`)
crude_mu_2016 <- as.numeric(crude_mu$`2016`)
crude_mu_2017 <- as.numeric(crude_mu$`2017`)
crude_mu_2018 <- as.numeric(crude_mu$`2018`)
crude_mu_2019 <- as.numeric(crude_mu$`2019`)
crude_mu_2020 <- as.numeric(crude_mu$`2020`)
crude_mu_2021 <- as.numeric(crude_mu$`2021`)
crude_mu_2022 <- as.numeric(crude_mu$`2022`)
crude_mu_2023 <- as.numeric(crude_mu$`2023`)



# Step 3: Apply P-spline smoothing

fit_2008 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2008)
fit_2009 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2009)
fit_2010 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2010)
fit_2011 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2011)
fit_2012 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2012)
fit_2013 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2013)
plot(x = age, y = 1 - exp(-fit_2013$ysmth), type = "p")
lines(x = age, y = 1 - exp(-crude_mu_2013))
fit_2014 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2014)
fit_2015 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2015)
fit_2016 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2016)
fit_2017 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2017)
fit_2018 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2018)
fit_2019 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2019)
fit_2020 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2020)
fit_2021 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2021)
fit_2022 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2022)
fit_2023 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2023)


smooth_qx_2008 <- 1 - exp(-fit_2008$ysmth)
smooth_qx_2009 <- 1 - exp(-fit_2009$ysmth)
smooth_qx_2010 <- 1 - exp(-fit_2010$ysmth)
smooth_qx_2011 <- 1 - exp(-fit_2011$ysmth)
smooth_qx_2012 <- 1 - exp(-fit_2012$ysmth)
smooth_qx_2013 <- 1 - exp(-fit_2013$ysmth)
smooth_qx_2014 <- 1 - exp(-fit_2014$ysmth)
smooth_qx_2015 <- 1 - exp(-fit_2015$ysmth)
smooth_qx_2016 <- 1 - exp(-fit_2016$ysmth)
smooth_qx_2017 <- 1 - exp(-fit_2017$ysmth)
smooth_qx_2018 <- 1 - exp(-fit_2018$ysmth)
smooth_qx_2019 <- 1 - exp(-fit_2019$ysmth)
smooth_qx_2020 <- 1 - exp(-fit_2020$ysmth)
smooth_qx_2021 <- 1 - exp(-fit_2021$ysmth)
smooth_qx_2022 <- 1 - exp(-fit_2022$ysmth)
smooth_qx_2023 <- 1 - exp(-fit_2023$ysmth)


Smoothed_mortality <- cbind(age,
                            smooth_qx_2008,
                            smooth_qx_2009,
                            smooth_qx_2010,
                            smooth_qx_2011,
                            smooth_qx_2012,
                            smooth_qx_2013,
                            smooth_qx_2014,
                            smooth_qx_2015,
                            smooth_qx_2016,
                            smooth_qx_2017,
                            smooth_qx_2018,
                            smooth_qx_2019,
                            smooth_qx_2020,
                            smooth_qx_2021,
                            smooth_qx_2022,
                            smooth_qx_2023)






Male_smoothed_data <- Smoothed_mortality

}

# Smoothing the female mortality data


{
  # Extracting the relevant unsmoothed mortality data
  
  # For the Male data, investigations show that the linkage
  # from our annuitants mortality to the S3 / S4 mortality is
  # most optimal at age 99
  
  # Note for the ladies data, I Smoothed the younger ages by reference of the
  # S4 mortality data. At the ages 40 - 60, we can see that the Base mortality
  # is consistently 100% above the S4 series, thus an adjustment was made
  # to only attach the base * 50% to the calculated rates, which
  # is being linked at ages 58-59.
  
  Unsmoothed_data  <- read_xlsx("CMI Mort Data.xlsx", sheet = "F Joined Mort Data")
  
  age <- Unsmoothed_data[,1]
  age <- as.numeric(age$Age)
  
  crude_mu <- -log(1 - Unsmoothed_data[,-1])
  
  
  crude_mu_2008 <- as.numeric(crude_mu$`2008`)
  crude_mu_2009 <- as.numeric(crude_mu$`2009`)
  crude_mu_2010 <- as.numeric(crude_mu$`2010`)
  crude_mu_2011 <- as.numeric(crude_mu$`2011`)
  crude_mu_2012 <- as.numeric(crude_mu$`2012`)
  crude_mu_2013 <- as.numeric(crude_mu$`2013`)
  crude_mu_2014 <- as.numeric(crude_mu$`2014`)
  crude_mu_2015 <- as.numeric(crude_mu$`2015`)
  crude_mu_2016 <- as.numeric(crude_mu$`2016`)
  crude_mu_2017 <- as.numeric(crude_mu$`2017`)
  crude_mu_2018 <- as.numeric(crude_mu$`2018`)
  crude_mu_2019 <- as.numeric(crude_mu$`2019`)
  crude_mu_2020 <- as.numeric(crude_mu$`2020`)
  crude_mu_2021 <- as.numeric(crude_mu$`2021`)
  crude_mu_2022 <- as.numeric(crude_mu$`2022`)
  crude_mu_2023 <- as.numeric(crude_mu$`2023`)
  
  
  
  # Step 3: Apply P-spline smoothing
  
  fit_2008 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2008)
  fit_2009 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2009)
  fit_2010 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2010)
  fit_2011 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2011)
  fit_2012 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2012)
  fit_2013 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2013)
  plot(x = age, y = 1 - exp(-fit_2013$ysmth), type = "p")
  lines(x = age, y = 1 - exp(-crude_mu_2013))
  fit_2014 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2014)
  fit_2015 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2015)
  fit_2016 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2016)
  fit_2017 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2017)
  fit_2018 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2018)
  fit_2019 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2019)
  fit_2020 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2020)
  fit_2021 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2021)
  fit_2022 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2022)
  fit_2023 <- smooth.Pspline(x = age, norder = 8, y = crude_mu_2023)
  
  
  smooth_qx_2008 <- 1 - exp(-fit_2008$ysmth)
  smooth_qx_2009 <- 1 - exp(-fit_2009$ysmth)
  smooth_qx_2010 <- 1 - exp(-fit_2010$ysmth)
  smooth_qx_2011 <- 1 - exp(-fit_2011$ysmth)
  smooth_qx_2012 <- 1 - exp(-fit_2012$ysmth)
  smooth_qx_2013 <- 1 - exp(-fit_2013$ysmth)
  smooth_qx_2014 <- 1 - exp(-fit_2014$ysmth)
  smooth_qx_2015 <- 1 - exp(-fit_2015$ysmth)
  smooth_qx_2016 <- 1 - exp(-fit_2016$ysmth)
  smooth_qx_2017 <- 1 - exp(-fit_2017$ysmth)
  smooth_qx_2018 <- 1 - exp(-fit_2018$ysmth)
  smooth_qx_2019 <- 1 - exp(-fit_2019$ysmth)
  smooth_qx_2020 <- 1 - exp(-fit_2020$ysmth)
  smooth_qx_2021 <- 1 - exp(-fit_2021$ysmth)
  smooth_qx_2022 <- 1 - exp(-fit_2022$ysmth)
  smooth_qx_2023 <- 1 - exp(-fit_2023$ysmth)
  
  
  Smoothed_mortality <- cbind(age,
                              smooth_qx_2008,
                              smooth_qx_2009,
                              smooth_qx_2010,
                              smooth_qx_2011,
                              smooth_qx_2012,
                              smooth_qx_2013,
                              smooth_qx_2014,
                              smooth_qx_2015,
                              smooth_qx_2016,
                              smooth_qx_2017,
                              smooth_qx_2018,
                              smooth_qx_2019,
                              smooth_qx_2020,
                              smooth_qx_2021,
                              smooth_qx_2022,
                              smooth_qx_2023)
  
  
  
  
 
  
  Female_smoothed_data <- Smoothed_mortality
  
}




#write.csv(Male_smoothed_data, file = "Male_Mort_data.csv", row.names = FALSE)
#write.csv(Female_smoothed_data, file = "Female_Mort_data.csv", row.names = FALSE)
