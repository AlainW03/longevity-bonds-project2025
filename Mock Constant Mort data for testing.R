mock_data <- CMI_Mort_Data <- read_excel("CMI Mort Data.xlsx", 
                                         sheet = "S4 Lives qx 2014 to 2019")
ages <- mock_data[,1]
ages <- as.numeric(ages$Age)

mortality <- mock_data[,2]
mortality <- as.numeric(mortality$S4PML)

Male_Mort <- cbind(ages, mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality
)
colnames(Male_Mort) <- c("Age", 2008:2023)

mortality <- mock_data[,3]
mortality <- as.numeric(mortality$S4PFL)

Female_Mort <- cbind(ages, mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality,
                   mortality
)
colnames(Female_Mort) <- c("Age", 2008:2023)
colnames(Male_Mort) <- c("Age", 2008:2023)
write.csv(Male_Mort, file = "Male_Mort_data.csv", row.names = FALSE)
write.csv(Female_Mort, file = "Female_Mort_data.csv", row.names = FALSE)
