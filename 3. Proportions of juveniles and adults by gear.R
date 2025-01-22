################################################################################
###### Script to read the outputs of the different stock assessments      ######
###### (SS3 model) and calculate from the outputs, how many juevniles     ######
######  and adults each fleet is fishing                                  ######
###### Species: BET, YFT and SKJ                                          ######
###### Author: Roger Amate (AZTI)                                         ######
###### year: 2023                                                         ######
################################################################################
#install.packages("zoo")
#install.packages("r4ss") # 
#remotes::install_github("r4ss/r4ss")
library(r4ss)
library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(hrbrthemes)
library(scales)

### Paste the directory where the outputs of each species are stored
dir.BET=".../8. Lectura de fichers/latest_SA_files/BET/io_h80_Gbase_MHamel17_sD/"         # It's a example
dir.YFT=".../8. Lectura de fichers/latest_SA_files/YFT/io_h80_q1_Gbase_Mbase_tlambda01/"  # It's a example
dir.SKJ=".../8. Lectura de fichers/latest_SA_files/SKJ/io_h80_Ua_q0_L70/"                 # It's a example

#######################################################    BIGEYE   ####################################################
SA_BET=SS_output(dir.BET, covar=FALSE)  # This reads the outputs of the assessment from "Report.soo".

# To diferentiate juveniles from adults, we will use the 50% maturity. First we obtain the size at 50% maturity from
#SA_BET_parameters, and then we check to which age that size corresponds in the endgrowth:
x=which(SA_BET$parameters$Label=="Mat50%_Fem_GP_1")   
size50Mat=SA_BET$parameters[x,"Value"]                 # This is the size at 50% maturity. Now we pass it to age class.
age_length=SA_BET$endgrowth[,c("Age_Beg", "Len_Mid")]  # Here we can see that the fish of sizes =>110 are fish older than 16 quarters (4 years).
Catch_at_age <- SA_BET$catage # This are the cathces of bigeye by fleet and age (from 0 to 40)

# Save the dataset as a .csv
write.csv(Catch_at_age, 'BET_catch_at_age.csv')
SA_BET$definitions # This allows us to check the gear that each fleet corresponds. In this case the flag is not important, 
                   # as it is only the gear used what describes the interaction of the fishery with each size class
SS_plots(SA_BET)   # This function automatically makes almost all figures of the outputs in an internet explorer page. 

#######################################################    YELLOWFIN   ##################################################

SA_YFT=SS_output(dir.YFT, covar=FALSE)  # This reads the outputs of the assessment from "Report.soo".

# To diferentiate juveniles from adults, we will use the 50% maturity. First we obtain the size at 50% maturity from
# SA_YFT_parameters, and then we check to which age that size corresponds in the endgrowth:
x=which(SA_YFT$parameters$Label=="Mat50%_Fem_GP_1")
size50Mat=SA_YFT$parameters[x,"Value"]                 # This is the size at 50% maturity. Now we pass it to age class.
age_length=SA_YFT$endgrowth[,c("Age_Beg", "Len_Mid")]  # Here we can see that the fish of sizes =>75 are fish older than 9 quarters (2.25 years)
Catch_at_age <- SA_YFT$catage # This are the cathces of yellowfin by fleet and age (from 0 to 28)

# Save the dataset as a .csv
write.csv(Catch_at_age, 'YFT_catch_at_age.csv')
SA_YFT$definitions # This allows us to check the gear that each fleet corresponds. In this case the flag is not important, 
                   # as it is only the gear used what describes the interaction of the fishery with each size class
SS_plots(SA_YFT)   # This function automatically makes almost all figures of the outputs in an internet explorer page.


#######################################################    SKIPJACK   ####################################################

SA_SKJ=SS_output(dir.SKJ, covar=FALSE)  # This reads the outputs of the assessment from "Report.soo".

# To diferentiate juveniles from adults, we will use the 50% maturity. First we obtain the size at 50% maturity from
# SA_SKJ_parameters, and then we check to which age that size corresponds in the endgrowth:
x=which(SA_SKJ$parameters$Label=="Mat50%_Fem_GP_1")
size50Mat=SA_SKJ$parameters[x,"Value"]                 # This is the size at 50% maturity. Now we pass it to age class.
age_length=SA_SKJ$endgrowth[,c("Age_Beg", "Len_Mid")]  # Here we can see that the fish of sizes =>38 are fish older than 3 quarters (0.75 years)
Catch_at_age <- SA_SKJ$catage # This are the cathces of skipjack by fleet and age (from 0 to 8)

# Save the dataset as a .csv
write.csv(Catch_at_age, 'SKJ_catch_at_age.csv')
SA_SKJ$definitions # This allows us to check the gear that each fleet corresponds. In this case the flag is not important, 
                   # as it is only the gear used what describes the interaction of the fishery with each size class
SS_plots(SA_SKJ)   # This function automatically makes almost all figures of the outputs in an internet explorer page.

####################################################### 

# Set the working directory
setwd("...")
########################### Data preparation ######
# read the data
catage <- read.csv('BET_catch_at_age.csv', sep=',')
catage[1] <- NULL # dalate the first column which is only an index
head(catage)
str(catage)

# check for NA values
any(is.na(catage)) # With this line we check if there are empty values (NA) in the database

# Subset the dataframe with only the columns that contain information that interests us
# which are: all the ones with the sizes (X0, X1, ...), Yr (191=1975 & 379=2022), Beg.Mid

catage1 <- catage %>% select(-c('XX', 'Sex', 'XX.1', 'Type', 'Morph', 'Seas', 'XX.2', 'Era', 'Area'))

# From the column Beg.Mid, we have to select one value, B or M. B is the value at the beginning, and M is 
# the value at the middle. I selected M because the values of biomass are smaller, and therefore the future measures that 
# that could be taken from the results are more conservative. 

catage2 <- catage1 %>% subset(Yr>=193 & Yr<=384) #Cambiar el valor segun la especie BET:193-384; YFT:13-304
unique(catage2$Yr)
unique(catage2$Fleet)
class(catage2$Fleet)

# Change Fleet numbers by the gear of the fleet
catage2$Fleet <- ifelse(catage2$Fleet==1, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==2, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==3, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==4, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==5, 'PSFS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==6, 'PSFS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==7, 'Others', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==8, 'Others', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==9, 'PSLS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==10, 'PSLS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==11, 'BB', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==12, 'LI', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==13, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==14, 'PSFS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==15, 'PSLS', catage2$Fleet)

# Save the dataset as a .csv
write.csv(catage2, 'BET_catch_at_age_by_gear.csv')
#catage2 <- read.csv('BET_catch_at_age_by_gear.csv', sep=',')

# convert Yr column to a character column
class(catage2$Yr)
catage2$Yr <- as.numeric(catage2$Yr)
################################

################################   BAITBOAT  ######
catage2BB <- catage2 %>% subset(Fleet=='BB')

# Aggregate by the quarter (Yr) 
catage3BB <- aggregate(. ~ Fleet + Yr, data=catage2BB, sum)
Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3BB <- cbind(Year, catage2BB)
catage3BB[,c(2,3)] <- NULL
BET_BB_catage <- aggregate(. ~ Year, data=catage3BB, mean)

# Save the dataset as .csv
write.csv(BET_BB_catage, 'BET_BB_catage.csv')

# Juvenile and adults total catch by year calculation
BET_BB_catage_Juv_Adu <- BET_BB_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Change the values according to the species: BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)
BET_BB_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(BET_BB_catage_Juv_Adu, "BET_BB_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
BET_BB_catage_JA_perc <- data.frame(
  Year = BET_BB_catage_Juv_Adu$Year,
  Juv_Catch = (BET_BB_catage_Juv_Adu$Juv_Catch/ rowSums(BET_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_BB_catage_Juv_Adu$Adu_Catch / rowSums(BET_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Save the dataset as .csv
write.csv(BET_BB_catage_JA_perc, "BET_BB_catage_JA_perc.csv", row.names = FALSE)

################################   LONGLINE  ######




