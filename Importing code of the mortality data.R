library(readxl)

#Importing the deaths and exposure data of the annuitants in payment 
# in self administered pension funds population
Male_exposure_Ann <- read_xlsx("CMI Mort Data.xlsx", sheet = "M L Exposure")
Male_deaths_Ann <- read_xlsx("CMI Mort Data.xlsx", sheet = "M L Deaths")
Female_exposure_Ann <- read_xlsx("CMI Mort Data.xlsx", sheet = "M L Exposure")
Female_deaths_Ann <- read_xlsx("CMI Mort Data.xlsx", sheet = "M L Deaths")

# Correcting the column names
colnames(Male_exposure_Ann) <- c("Age", 2013:2020)
colnames(Male_deaths_Ann) <- c("Age", 2013:2020)
colnames(Female_exposure_Ann) <- c("Age", 2013:2020)
colnames(Female_deaths_Ann) <- c("Age", 2013:2020)

# Importing the calculated qx base mortality values
# Calculated using CMI's dx and lx values over the time periods
Base_Male_Mort_qx <- read_xlsx("CMI Mort Data.xlsx", sheet = "M Base qx")
Base_Female_Mort_qx <- read_xlsx("CMI Mort Data.xlsx", sheet = "F Base qx")

#The annuitant data only extends to 2020, so I'll be limiting the data to 2020 i/o 2023
Base_Male_Mort_qx <- Base_Male_Mort_qx[,1:(ncol(Base_Male_Mort_qx)-3)]
Base_Female_Mort_qx <- Base_Female_Mort_qx[,1:(ncol(Base_Female_Mort_qx)-3)]

#Correcting the base mortality's column names
colnames(Base_Male_Mort_qx) <- c("Age", 1981:2020)
colnames(Base_Female_Mort_qx) <- c("Age", 1981:2020)
