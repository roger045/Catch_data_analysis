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
Weight_at_age <- SA_BET$watage # This are the weights of bigeye by age (from 0 to 40)

# Save the datasets as a .csv
write.csv(Catch_at_age, 'BET_catch_at_age.csv') # The units of this matrix are the nº of individuals (x1000) by age
write.csv(Weight_at_age, 'BET_watage.csv') # The units of this matrix are the mean weight at each age and each year in kg
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
Weight_at_age <- SA_YFT$watage # This are the weights of yellowfin by age (from 0 to 28)

# Save the dataset as a .csv
write.csv(Catch_at_age, 'YFT_catch_at_age.csv') # The units of this matrix are the nº of individuals (x1000) by age
write.csv(Weight_at_age, 'YFT_watage.csv') # The units of this matrix are the mean weight at each age and each year in kg
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
Weight_at_age <- SA_SKJ$watage # This are the weights of yellowfin by age (from 0 to 8)

# Save the dataset as a .csv
write.csv(Catch_at_age, 'SKJ_catch_at_age.csv') # The units of this matrix are the nº of individuals (x1000) by age
write.csv(Weight_at_age, 'SKJ_watage.csv') # The units of this matrix are the mean weight at each age and each year in kg
SA_SKJ$definitions # This allows us to check the gear that each fleet corresponds. In this case the flag is not important, 
                   # as it is only the gear used what describes the interaction of the fishery with each size class
SS_plots(SA_SKJ)   # This function automatically makes almost all figures of the outputs in an internet explorer page.

####################################################### 

#######################################################    BIGEYE   ####################################################
# Set the working directory where we have saved the BET_catch_at_age.csv
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

catage2 <- catage1 %>% subset(Yr>=193 & Yr<=384) #Change the value according to the species BET:193-384; YFT:13-304
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

# convert Yr column to a character column
class(catage2$Yr)
catage2$Yr <- as.numeric(catage2$Yr)

# Read the weight at age matrix (units: kg)
watage <- read.csv('BET_watage.csv', sep=',')

# As We are interested in only the sizes, we want to transform all the first row of sizes into a column 
head(watage)
str(watage)
summary(watage)
watage[c(1,2,3,4,5,6,7)] <- NULL # dalate the first 7 columns which doesn't interest us

# Delate all the rows unless the first one and then divide by 1000 to convert from kg to tons
watage <- watage[1, , drop = FALSE]
watage <- watage/1000

# Transpose the matrix to obtain the matrix in columns
watage_col <- t(watage)
colnames(watage_col)[1] <- "watage"

# Open the file previously created of the catches by age and gear
catage2 <- read.csv('BET_catch_at_age_by_gear.csv', sep=',')

# Multiply the values x1000 to obtain the number of individualss
catage3 <- catage2[,c(4:44)]*1000

# Multiply each value of the watage matrix by each column corresponding to the value of catage 2
catage2 <- cbind(catage2[,c(1:3)], catage3)

# Convert the matrix to a data.frame to be able to do a loop
watage_col <- as.data.frame(watage_col)

# Iterate over the value of the column 'watage'
for (i in 1:41) {
  # Multiply each value of 'watage' by the corresponding column of 'catage2'
  catage2[, i + 3] <- catage2[, i + 3] * watage_col$watage[i]
}

# Save the dataset as .csv
write.csv(catage2, 'BET_catches_in_tonnes_x_gear_and_age.csv')
################################

################################   BAITBOAT  ######
catage2BB <- catage2 %>% subset(Fleet=='BB')

# Aggregate by the quarter (Yr) 
catage3BB <- aggregate(. ~ Fleet + Yr, data=catage2BB, sum)
Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3BB <- cbind(Year, catage2BB)
catage3BB[,c(2,3,4)] <- NULL
BET_BB_catage <- aggregate(. ~ Year, data=catage3BB, mean)

# Save the dataset as .csv
write.csv(BET_BB_catage, 'BET_BB_catage.csv')

# Juvenile and adults total catch by year calculation
BET_BB_catage_Juv_Adu <- BET_BB_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)
BET_BB_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(BET_BB_catage_Juv_Adu, "BET_BB_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
BET_BB_catage_JA_perc <- data.frame(
  Year = BET_BB_catage_Juv_Adu$Year,
  Juv_Catch = (BET_BB_catage_Juv_Adu$Juv_Catch/ rowSums(BET_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_BB_catage_Juv_Adu$Adu_Catch / rowSums(BET_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(BET_BB_catage_JA_perc, "BET_BB_catage_JA_perc.csv", row.names = FALSE)

################################   LONGLINE  ######
catage2LL <- catage2 %>% subset(Fleet=='LL')

# Aggregate by the quarter (Yr) 
catage3LL <- aggregate(. ~ Fleet + Yr, data=catage2LL, sum)
Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3LL <- cbind(Year, catage3LL)
catage3LL[,c(2,3,4)] <- NULL
BET_LL_catage <- aggregate(. ~ Year, data=catage3LL, mean)

# Save the dataset as .csv
write.csv(BET_LL_catage, 'BET_LL_catage.csv')

# Juvenile and adults total catch by year calculation
BET_LL_catage_Juv_Adu <- BET_LL_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)
BET_LL_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(BET_LL_catage_Juv_Adu, "BET_LL_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
BET_LL_catage_JA_perc <- data.frame(
  Year = BET_LL_catage_Juv_Adu$Year,
  Juv_Catch = (BET_LL_catage_Juv_Adu$Juv_Catch/ rowSums(BET_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_LL_catage_Juv_Adu$Adu_Catch / rowSums(BET_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# # Save the dataset as .csv
write.csv(BET_LL_catage_JA_perc, "BET_BB_catage_JA_perc_MA.csv", row.names = FALSE)

################################   PSFS   ######
catage2PSFS <- catage2 %>% subset(Fleet=='PSFS')

# Aggregate by the quarter (Yr) 
catage3PSFS <- aggregate(. ~ Fleet + Yr, data=catage2PSFS, sum)
Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3PSFS <- cbind(Year, catage3PSFS)
catage3PSFS[,c(2,3,4)] <- NULL
BET_PSFS_catage <- aggregate(. ~ Year, data=catage3PSFS, mean)

# Save the dataset as .csv
write.csv(BET_PSFS_catage, 'BET_PSFS_catage.csv')

# Juvenile and adults total catch by year calculation
BET_PSFS_catage_Juv_Adu <- BET_PSFS_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

BET_PSFS_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(BET_PSFS_catage_Juv_Adu, "BET_PSFS_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
BET_PSFS_catage_JA_perc <- data.frame(
  Year = BET_PSFS_catage_Juv_Adu$Year,
  Juv_Catch = (BET_PSFS_catage_Juv_Adu$Juv_Catch/ rowSums(BET_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_PSFS_catage_Juv_Adu$Adu_Catch / rowSums(BET_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(BET_PSFS_catage_JA_perc, "BET_PSFS_catage_JA_perc.csv", row.names = FALSE)


################################   Others   #####
catage2oth <- catage2 %>% subset(Fleet=='Others')

# Aggregate by the quarter (Yr) 
catage3oth <- aggregate(. ~ Fleet + Yr, data=catage2oth, sum)
Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3oth <- cbind(Year, catage3oth)
catage3oth[,c(2,3,4)] <- NULL
BET_oth_catage <- aggregate(. ~ Year, data=catage3oth, mean)

# Save the dataset as .csv
write.csv(BET_oth_catage, 'BET_oth_catage.csv')

# Juvenile and adults total catch by year calculation
BET_oth_catage_Juv_Adu <- BET_oth_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),     # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

BET_oth_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(BET_oth_catage_Juv_Adu, "BET_oth_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
BET_oth_catage_JA_perc <- data.frame(
  Year = BET_oth_catage_Juv_Adu$Year,
  Juv_Catch = (BET_oth_catage_Juv_Adu$Juv_Catch/ rowSums(BET_oth_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_oth_catage_Juv_Adu$Adu_Catch / rowSums(BET_oth_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(BET_oth_catage_JA_perc, "BET_oth_catage_JA_perc.csv", row.names = FALSE)


################################   PSLS   #####
catage2PSLS <- catage2 %>% subset(Fleet=='PSLS')

# Aggregate by the quarter (Yr) 
catage3PSLS <- aggregate(. ~ Fleet + Yr, data=catage2PSLS, sum)
Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3PSLS <- cbind(Year, catage3PSLS)
catage3PSLS[,c(2,3,4)] <- NULL
BET_PSLS_catage <- aggregate(. ~ Year, data=catage3PSLS, mean)

# Save the dataset as .csv
write.csv(BET_PSLS_catage, 'BET_PSLS_catage.csv')

# Juvenile and adults total catch by year calculation
BET_PSLS_catage_Juv_Adu <- BET_PSLS_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),     # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

BET_PSLS_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(BET_PSLS_catage_Juv_Adu, "BET_PSLS_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
BET_PSLS_catage_JA_perc <- data.frame(
  Year = BET_PSLS_catage_Juv_Adu$Year,
  Juv_Catch = (BET_PSLS_catage_Juv_Adu$Juv_Catch/ rowSums(BET_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_PSLS_catage_Juv_Adu$Adu_Catch / rowSums(BET_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(BET_PSLS_catage_JA_perc, "BET_PSLS_catage_JA_perc.csv", row.names = FALSE)


################################   Line   #####
catage2LI <- catage2 %>% subset(Fleet=='LI')

# Aggregate by the quarter (Yr) 
catage3LI <- aggregate(. ~ Fleet + Yr, data=catage2LI, sum)
Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3LI <- cbind(Year, catage3LI)
catage3LI[,c(2,3,4)] <- NULL
BET_LI_catage <- aggregate(. ~ Year, data=catage3LI, mean)

# Save the dataset as .csv
write.csv(BET_LI_catage, 'BET_LI_catage.csv')

# Juvenile and adults total catch by year calculation
BET_LI_catage_Juv_Adu <- BET_LI_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

BET_LI_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(BET_LI_catage_Juv_Adu, "BET_LI_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
BET_LI_catage_JA_perc <- data.frame(
  Year = BET_LI_catage_Juv_Adu$Year,
  Juv_Catch = (BET_LI_catage_Juv_Adu$Juv_Catch/ rowSums(BET_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_LI_catage_Juv_Adu$Adu_Catch / rowSums(BET_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(BET_LI_catage_JA_perc, "BET_LI_catage_JA_perc.csv", row.names = FALSE)

###############################################################  Plots  ###############################################################

############# Catch total
catage5 <- BET_LL_catage_Juv_Adu ##### Change the gear (LL, BB, GN,...) to obtain the plots

#--------------------------------Data representation---------------------------#
catage6 <- pivot_longer(catage5, c('Juv_Catch', 'Adu_Catch'))
names(catage6)[2] <- "Stanza"
names(catage6)[3] <- "Catch"
# Here we order the species x catch otherwise they'll appear alphabetically in the plot
class(catage6$Stanza)
catage6$Stanza <- factor(catage6$Stanza, levels=c('Adu_Catch', 'Juv_Catch'))
sp_colors<- c('Juv_Catch'='grey62', 'Adu_Catch'='grey27')

#Stacked barchart - With our relevant species
ggplot(catage6, aes(fill=Stanza, y=Catch, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  theme(text = element_text(size=11))+
  scale_fill_manual(values = sp_colors,name='Stanza')+xlab('Year')+ylab('Catch (t)')+
  ggtitle('Longline juvenile and adult BET catch in the model area [1975-2022]')+ # Change the gear name in the title
  scale_y_continuous(labels = scales::label_number(scale = 1)) # Converts scientific notation to numerical

############# Catch Percentage (absolut value)
catage5 <- BET_LL_catage_JA_perc    ##### Change the gear (LL, BB, GN,...) to obtain the plots

#--------------------------------Data representation---------------------------#
catage6 <- pivot_longer(catage5, c('Juv_Catch', 'Adu_Catch'))
names(catage6)[2] <- "Stanza"
names(catage6)[3] <- "Catch"
# Here we order the species x catch otherwise they'll appear alphabetically in the plot
class(catage6$Stanza)
catage6$Stanza <- factor(catage6$Stanza, levels=c('Adu_Catch', 'Juv_Catch'))
sp_colors<- c('Juv_Catch'='grey62', 'Adu_Catch'='grey27')

#Stacked barchart - With our relevant species
ggplot(catage6, aes(fill=Stanza, y=Catch, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  theme(text = element_text(size=11))+
  scale_fill_manual(values = sp_colors,name='Stanza')+xlab('Year')+ylab('Catch (%)')+
  ggtitle('Longline juvenile and adult BET catch proportion in the model area [1975-2022]')+ # Change the gear name in the title
  scale_y_continuous(labels = scales::label_number(scale = 1)) # Converts scientific notation to numerical
###############################################################################################################################

#######################################################    YELLOWFIN   ##################################################
# Set the working directory where we have saved the YFT_catch_at_age.csv
setwd("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/7. Stock_assessment/1. Datasets with data on the model area as a unit (correct)/YFT")

################## Data preparation 
# read the data
catage <- read.csv('YFT_catch_at_age.csv', sep=',')
catage[1] <- NULL # dalate the first column which is only an index
head(catage)
str(catage)

# Check for NA values
any(is.na(catage)) # With this line we check if there are empty values (NA) in the database

# Subset the dataframe with only the columns that contain information that interests us
# which are: all the ones with the sizes (X0, X1, ...), Yr (191=1975 & 379=2022), Beg.Mid

catage1 <- catage %>% select(-c('XX', 'Sex', 'XX.1', 'Type', 'Morph', 'Seas', 'XX.2', 'Era', 'Area'))

# From the column Beg.Mid, we have to select one value, B or M. B is the value at the beginning, and M is 
# the value at the middle. I selected M because the values of biomass are smaller, and therefore the future measures that 
# that could be taken from the results are more conservative. 

catage2 <- catage1 %>% subset(Yr>=13 & Yr<=304) # Change the value according to the species BET:192-379; YFT:12-300
unique(catage2$Yr)
unique(catage2$Fleet)
class(catage2$Fleet)

# Change Fleet numbers by the gear of the fleet
catage2$Fleet <- ifelse(catage2$Fleet==1, 'GN', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==2, 'LI', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==3, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==4, 'Others', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==5, 'BB', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==6, 'PSFS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==7, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==8, 'PSLS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==9, 'LI', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==10, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==11, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==12, 'GN', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==13, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==14, 'Others', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==15, 'LI', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==16, 'PSFS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==17, 'PSLS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==18, 'LI', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==19, 'PSFS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==20, 'PSLS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==21, 'LL', catage2$Fleet)

unique(catage2$Fleet)

# Save the dataset as .csv
write.csv(catage2, 'YFT_catch_at_age_by_gear.csv')

# Convert Yr column to a character column
class(catage2$Yr)
catage2$Yr <- as.numeric(catage2$Yr)

# Read the weight at age matrix (units: kg)
watage <- read.csv('YFT_watage.csv', sep=',')

# As We are interested in only the sizes, we want to transform all the first row of sizes into a column 
head(watage)
str(watage)
summary(watage)
watage[c(1,2,3,4,5,6,7)] <- NULL # Delete the first 7 columns which don't interest us

# Delete all the rows unless the first one and then divide by 1000 to convert from kg to tons
watage <- watage[1, , drop = FALSE]
watage <- watage/1000

# Transpose the matrix to obtain the matrix in columns 
watage_col <- t(watage)
colnames(watage_col)[1] <- "watage"

# Open the previously created file of the catches by age and gear
catage2 <- read.csv('YFT_catch_at_age_by_gear.csv', sep=',')

# Multiply the values x1000 to obtain the number of individuals
catage3 <- catage2[,c(4:32)]*1000
# Multiply each value of the watage matrix by each column corresponding to the value of catage 2
catage2 <- cbind(catage2[,c(1:3)], catage3)

# Convert the matrix to a data.frame to be able to do a loop
watage_col <- as.data.frame(watage_col)

# Iterate over the value of the column 'watage'
for (i in 1:29) {
  # Multiply each value of 'watage' by the corresponding column of 'catage2'
  catage2[, i + 3] <- catage2[, i + 3] * watage_col$watage[i]
}

# Save the dataset as .csv
write.csv(catage2, 'YFT_catches_in_tonnes_x_gear_and_age.csv')
################################

################################   GILLNET  #####
catage2GN <- catage2 %>% subset(Fleet=='GN')

# Aggregate by the quarter (Yr) 
catage2GN <- aggregate(. ~ Fleet + Yr, data=catage2GN, sum)
Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3GN <- cbind(Year, catage2GN)
catage3GN[,c(2,3,4)] <- NULL
YFT_GN_catage <- aggregate(. ~ Year, data=catage3GN, mean)

# Save the dataset as .csv
write.csv(YFT_GN_catage, 'YFT_GN_catage.csv')

 Juvenile and adults total catch by year calculation
YFT_GN_catage_Juv_Adu <- YFT_GN_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

YFT_GN_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(YFT_GN_catage_Juv_Adu, "YFT_GN_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
YFT_GN_catage_JA_perc <- data.frame(
  Year = YFT_GN_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_GN_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_GN_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_GN_catage_Juv_Adu$Adu_Catch / rowSums(YFT_GN_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(YFT_GN_catage_JA_perc, "YFT_GN_catage_JA_perc.csv", row.names = FALSE)

################################   LINE  #####
catage2LI <- catage2 %>% subset(Fleet=='LI')

# Aggregate by the quarter (Yr) 
catage2LI <- aggregate(. ~ Fleet + Yr, data=catage2LI, sum)
Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3LI <- cbind(Year, catage2LI)
catage3LI[,c(2,3,4)] <- NULL
YFT_LI_catage <- aggregate(. ~ Year, data=catage3LI, mean)

# Save the dataset as .csv
write.csv(YFT_LI_catage, 'YFT_LI_catage.csv')

# Juvenile and adults total catch by year calculation
YFT_LI_catage_Juv_Adu <- YFT_LI_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

YFT_LI_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(YFT_LI_catage_Juv_Adu, "YFT_LI_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
YFT_LI_catage_JA_perc <- data.frame(
  Year = YFT_LI_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_LI_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_LI_catage_Juv_Adu$Adu_Catch / rowSums(YFT_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(YFT_LI_catage_JA_perc, "YFT_LI_catage_JA_perc.csv", row.names = FALSE)


################################  LONGLINE  #####
catage2LL <- catage2 %>% subset(Fleet=='LL')

# Aggregate by the quarter (Yr) 
catage2LL <- aggregate(. ~ Fleet + Yr, data=catage2LL, sum)
Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3LL <- cbind(Year, catage2LL)
catage3LL[,c(2,3,4)] <- NULL
YFT_LL_catage <- aggregate(. ~ Year, data=catage3LL, mean)

# Save the dataset as .csv
write.csv(YFT_LL_catage, 'YFT_LL_catage.csv')

# Juvenile and adults total catch by year calculation
YFT_LL_catage_Juv_Adu <- YFT_LL_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

YFT_LL_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(YFT_LL_catage_Juv_Adu, "YFT_LL_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
YFT_LL_catage_JA_perc <- data.frame(
  Year = YFT_LL_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_LL_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_LL_catage_Juv_Adu$Adu_Catch / rowSums(YFT_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(YFT_LL_catage_JA_perc, "YFT_LL_catage_JA_perc.csv", row.names = FALSE)

################################   OTHERS  #####
catage2oth <- catage2 %>% subset(Fleet=='Others')

# Aggregate by the quarter (Yr) 
catage2oth <- aggregate(. ~ Fleet + Yr, data=catage2oth, sum)
Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3oth <- cbind(Year, catage2oth)
catage3oth[,c(2,3,4)] <- NULL
YFT_oth_catage <- aggregate(. ~ Year, data=catage3oth, mean)

# Save the dataset as .csv
write.csv(YFT_oth_catage, 'YFT_oth_catage.csv')

# Juvenile and adults total catch by year calculation
YFT_oth_catage_Juv_Adu <- YFT_oth_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

YFT_oth_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(YFT_oth_catage_Juv_Adu, "YFT_oth_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
YFT_oth_catage_JA_perc <- data.frame(
  Year = YFT_oth_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_oth_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_oth_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_oth_catage_Juv_Adu$Adu_Catch / rowSums(YFT_oth_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(YFT_oth_catage_JA_perc, "YFT_oth_catage_JA_perc.csv", row.names = FALSE)

################################   BAITBOAT  #####
catage2BB <- catage2 %>% subset(Fleet=='BB')

# Aggregate by the quarter (Yr) 
catage2BB <- aggregate(. ~ Fleet + Yr, data=catage2BB, sum)
Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3BB <- cbind(Year, catage2BB)
catage3BB[,c(2,3,4)] <- NULL
YFT_BB_catage <- aggregate(. ~ Year, data=catage3BB, mean)

# Save the dataset as .csv
write.csv(YFT_BB_catage, 'YFT_BB_catage.csv')

# Juvenile and adults total catch by year  calculation
YFT_BB_catage_Juv_Adu <- YFT_BB_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

YFT_BB_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(YFT_BB_catage_Juv_Adu, "YFT_BB_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
YFT_BB_catage_JA_perc <- data.frame(
  Year = YFT_BB_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_BB_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_BB_catage_Juv_Adu$Adu_Catch / rowSums(YFT_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(YFT_BB_catage_JA_perc, "YFT_BB_catage_JA_perc.csv", row.names = FALSE)

################################  PSFS  #####
catage2PSFS <- catage2 %>% subset(Fleet=='PSFS')

# Aggregate by the quarter (Yr) 
catage2PSFS <- aggregate(. ~ Fleet + Yr, data=catage2PSFS, sum)
Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3PSFS <- cbind(Year, catage2PSFS)
catage3PSFS[,c(2,3,4)] <- NULL
YFT_PSFS_catage <- aggregate(. ~ Year, data=catage3PSFS, mean)

# Save the dataset as .csv
write.csv(YFT_PSFS_catage, 'YFT_PSFS_catage.csv')

# Juvenile and adults total catch by year calculation
YFT_PSFS_catage_Juv_Adu <- YFT_PSFS_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

YFT_PSFS_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(YFT_PSFS_catage_Juv_Adu, "YFT_PSFS_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
YFT_PSFS_catage_JA_perc <- data.frame(
  Year = YFT_PSFS_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_PSFS_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_PSFS_catage_Juv_Adu$Adu_Catch / rowSums(YFT_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(YFT_PSFS_catage_JA_perc, "YFT_PSFS_catage_JA_perc.csv", row.names = FALSE)

################################   PSLS  #####
catage2PSLS <- catage2 %>% subset(Fleet=='PSLS')

# Aggregate by the quarter (Yr) 
catage2PSLS <- aggregate(. ~ Fleet + Yr, data=catage2PSLS, sum)
Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)
Year[,2] <- NULL
catage3PSLS <- cbind(Year, catage2PSLS)
catage3PSLS[,c(2,3,4)] <- NULL
YFT_PSLS_catage <- aggregate(. ~ Year, data=catage3PSLS, mean)

# Save the dataset as .csv
write.csv(YFT_PSLS_catage, 'YFT_PSLS_catage.csv')

# Juvenile and adults total catch by year calculation
YFT_PSLS_catage_Juv_Adu <- YFT_PSLS_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

YFT_PSLS_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(YFT_PSLS_catage_Juv_Adu, "YFT_PSLS_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
YFT_PSLS_catage_JA_perc <- data.frame(
  Year = YFT_PSLS_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_PSLS_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_PSLS_catage_Juv_Adu$Adu_Catch / rowSums(YFT_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(YFT_PSLS_catage_JA_perc, "YFT_PSLS_catage_JA_perc.csv", row.names = FALSE)

###############################################################  Plots  ###############################################################

############# Catch total
catage5 <- YFT_GN_catage_Juv_Adu ##### Change the gear (LL, BB, GN,...) to obtain the plots

#--------------------------------Data representation---------------------------#
catage6 <- pivot_longer(catage5, c('Juv_Catch', 'Adu_Catch'))
names(catage6)[2] <- "Stanza"
names(catage6)[3] <- "Catch"
# Here we order the species x catch otherwise they'll appear alphabetically in the plot
class(catage6$Stanza)
catage6$Stanza <- factor(catage6$Stanza, levels=c('Adu_Catch', 'Juv_Catch'))
sp_colors<- c('Juv_Catch'='grey62', 'Adu_Catch'='grey27')

#Stacked barchart - With our relevant species
ggplot(catage6, aes(fill=Stanza, y=Catch, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  theme(text = element_text(size=11))+
  scale_fill_manual(values = sp_colors,name='Stanza')+xlab('Year')+ylab('Catch (t)')+
  ggtitle('Gillnet juvenile and adult YFT catch in the model area [1950-2022]')+ # Change the gear name in the title
  scale_y_continuous(labels = scales::label_number(scale = 1)) # Converts scientific notation to numerical

############# Catch Percentage
catage5 <- YFT_GN_catage_JA_perc ##### Change the gear (LL, BB, GN,...) to obtain the plots
#--------------------------------Data representation---------------------------# 
catage6 <- pivot_longer(catage5, c('Juv_Catch', 'Adu_Catch'))
names(catage6)[2] <- "Stanza"
names(catage6)[3] <- "Catch"
# Here we order the species x catch otherwise they'll appear alphabetically in the plot
class(catage6$Stanza)
catage6$Stanza <- factor(catage6$Stanza, levels=c('Adu_Catch', 'Juv_Catch'))
sp_colors<- c('Juv_Catch'='grey62', 'Adu_Catch'='grey27')

#Stacked barchart - With our relevant species
ggplot(catage6, aes(fill=Stanza, y=Catch, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  theme(text = element_text(size=11))+
  scale_fill_manual(values = sp_colors,name='Stanza')+xlab('Year')+ylab('Catch (%)')+
  ggtitle('Gillnet juvenile and adult YFT catch proportion in the model area [1950-2022]')+ # Change the gear name in the title
  scale_y_continuous(labels = scales::label_number(scale = 1)) # Converts scientific notation to numerical
###################################################################################################################################

#######################################################    SKIPJACK   ##################################################
# Set the working directory where we have saved the SKJ_catch_at_age.csv
setwd("...")

################## Data preparation 
# read the data
catage <- read.csv('SKJ_catch_at_age.csv', sep=',')
catage[1] <- NULL # dalate the first column which is only an index
head(catage)
str(catage)

# check for NA values
any(is.na(catage)) # With this line we check if there are empty values (NA) in the database

# Subset the dataframe with only the columns that contain information that interests us
# which are: all the ones with the sizes (X0, X1, ...), Yr (191=1975 & 379=2022), Beg.Mid
catage1 <- catage %>% select(-c('Area', 'XX', 'Sex', 'XX.1', 'Type', 'Morph', 'Seas', 'XX.2', 'Era', 'Area'))

# From the column Beg.Mid, we have to select one value, B or M. B is the value at the beginning, and M is 
# the value at the middle. I selected M because the values of biomass are smaller, and therefore the future measures that 
# that could be taken from the results are more conservative. 
catage2 <- catage1 %>% subset(Yr>=1950 & Yr<=2022) 
unique(catage2$Yr)
unique(catage2$Fleet)
class(catage2$Fleet)

# Change Fleet numbers by the gear of the fleet
catage2$Fleet <- ifelse(catage2$Fleet==1, 'BB', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==2, 'PSLS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==3, 'PSFS', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==4, 'GN', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==5, 'LI', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==6, 'LL', catage2$Fleet)
catage2$Fleet <- ifelse(catage2$Fleet==7, 'Others', catage2$Fleet)

unique(catage2$Fleet)

# Save the dataset as .csv
write.csv(catage2, 'SKJ_catch_at_age_by_gear.csv')

# convert Yr column to a character column
class(catage2$Yr)
catage2$Yr <- as.numeric(catage2$Yr)

# Read the weight at age matrix (units: kg)
watage <- read.csv('SKJ_watage.csv', sep=',')

# As We are interested in only the sizes, we want to transform all the first row of sizes into a column 
head(watage)
str(watage)
summary(watage)
watage[c(1,2,3,4,5,6,7)] <- NULL # Delete the first 7 columns which don't interest us

# Delete all the rows unless the first one and then divide by 1000 to convert from kg to tons
watage <- watage[1, , drop = FALSE]
watage <- watage/1000

# Transpose the matrix to obtain the matrix in columns
watage_col <- t(watage)
colnames(watage_col)[1] <- "watage"

# Open the previously created file of the catches by age and gear
catage2 <- read.csv('SKJ_catch_at_age_by_gear.csv', sep=',')

# Multiply the values x1000 to obtain the number of individuals
catage3 <- catage2[,c(4:12)]*1000

# Multiply each value of the watage matrix by each column corresponding to the value of catage 2
catage2 <- cbind(catage2[,c(1:3)], catage3)

# Convert the matrix to a data.frame to be able to do a loop
watage_col <- as.data.frame(watage_col)

# Iterate over the value of the column 'watage'
for (i in 1:9) {
  # Multiply each value of 'watage' by the corresponding column of 'catage2'
  catage2[, i + 3] <- catage2[, i + 3] * watage_col$watage[i]
}

# Save the dataset as .csv
write.csv(catage2, 'SKJ_catches_in_tonnes_x_gear_and_age.csv')
################################

################################   LONGLINE  #####
catage2LL <- catage2 %>% subset(Fleet=='LL')

# Aggregate by the quarter (Yr) 
catage2LL <- aggregate(. ~ Fleet + Yr, data=catage2LL, sum)
catage2LL[,1] <- NULL

# Juvenile and adults total catch by year calculation
catage2LL[,2] <- NULL
SKJ_LL_catage_Juv_Adu <- catage2LL %>%
  mutate(Juv_Catch = catage2LL[,2],      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

SKJ_LL_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(SKJ_LL_catage_Juv_Adu, "SKJ_LL_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
SKJ_LL_catage_JA_perc <- data.frame(
  Yr = SKJ_LL_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_LL_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_LL_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(SKJ_LL_catage_JA_perc, "SKJ_LL_catage_JA_perc.csv", row.names = FALSE)

################################   LINE  #####
catage2LI <- catage2 %>% subset(Fleet=='LI')

# Aggregate by the quarter (Yr) 
catage2LI <- aggregate(. ~ Fleet + Yr, data=catage2LI, sum)
catage2LI[,1] <- NULL

# Juvenile and adults total catch by year calculation
catage2LI[,2] <- NULL
SKJ_LI_catage_Juv_Adu <- catage2LI %>%
  mutate(Juv_Catch = catage2LI[,2],      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

SKJ_LI_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(SKJ_LI_catage_Juv_Adu, "SKJ_LI_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
SKJ_LI_catage_JA_perc <- data.frame(
  Yr = SKJ_LI_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_LI_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_LI_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(SKJ_LI_catage_JA_perc, "SKJ_LI_catage_JA_perc.csv", row.names = FALSE)

################################   GILLNET  #####
catage2GN <- catage2 %>% subset(Fleet=='GN')

# Aggregate by the quarter (Yr) 
catage2GN <- aggregate(. ~ Fleet + Yr, data=catage2GN, sum)
catage2GN[,1] <- NULL

# Juvenile and adults total catch by year calculation
catage2GN[,2] <- NULL
SKJ_GN_catage_Juv_Adu <- catage2GN %>%
  mutate(Juv_Catch = catage2GN[,2],      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

SKJ_GN_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(SKJ_GN_catage_Juv_Adu, "SKJ_GN_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
SKJ_GN_catage_JA_perc <- data.frame(
  Yr = SKJ_GN_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_GN_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_GN_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_GN_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_GN_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(SKJ_GN_catage_JA_perc, "SKJ_GN_catage_JA_perc.csv", row.names = FALSE)

################################   PSFS  #####
catage2PSFS <- catage2 %>% subset(Fleet=='PSFS')

# Aggregate by the quarter (Yr) 
catage2PSFS <- aggregate(. ~ Fleet + Yr, data=catage2PSFS, sum)
catage2PSFS[,1] <- NULL

# Juvenile and adults total catch by year calculation
catage2PSFS[,2] <- NULL
SKJ_PSFS_catage_Juv_Adu <- catage2PSFS %>%
  mutate(Juv_Catch = catage2PSFS[,2],      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

SKJ_PSFS_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(SKJ_PSFS_catage_Juv_Adu, "SKJ_PSFS_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
SKJ_PSFS_catage_JA_perc <- data.frame(
  Yr = SKJ_PSFS_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_PSFS_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_PSFS_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(SKJ_PSFS_catage_JA_perc, "SKJ_PSFS_catage_JA_perc.csv", row.names = FALSE)

################################   PSLS  #####
catage2PSLS <- catage2 %>% subset(Fleet=='PSLS')

# Aggregate by the quarter (Yr) 
catage2PSLS <- aggregate(. ~ Fleet + Yr, data=catage2PSLS, sum)
catage2PSLS[,1] <- NULL

# Juvenile and adults total catch by year calculation
catage2PSLS[,2] <- NULL
SKJ_PSLS_catage_Juv_Adu <- catage2PSLS %>%
  mutate(Juv_Catch = catage2PSLS[,2],      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

SKJ_PSLS_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(SKJ_PSLS_catage_Juv_Adu, "SKJ_PSLS_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
SKJ_PSLS_catage_JA_perc <- data.frame(
  Yr = SKJ_PSLS_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_PSLS_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_PSLS_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(SKJ_PSLS_catage_JA_perc, "SKJ_PSLS_catage_JA_perc.csv", row.names = FALSE)

################################   Others  #####
catage2o <- catage2 %>% subset(Fleet=='Others')

# Aggregate by the quarter (Yr) 
catage2o <- aggregate(. ~ Fleet + Yr, data=catage2o, sum)
catage2o[,1] <- NULL

# Juvenile and adults total catch by year calculation
catage2o[,2] <- NULL
SKJ_Others_catage_Juv_Adu <- catage2o %>%
  mutate(Juv_Catch = catage2o[,2],      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

SKJ_Others_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(SKJ_Others_catage_Juv_Adu, "SKJ_Others_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
SKJ_Others_catage_JA_perc <- data.frame(
  Yr = SKJ_Others_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_Others_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_Others_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_Others_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_Others_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(SKJ_Others_catage_JA_perc, "SKJ_Others_catage_JA_perc.csv", row.names = FALSE)

################################   Baitboat  #####
catage2o <- catage2 %>% subset(Fleet=='BB')

# Aggregate by the quarter (Yr) 
catage2o <- aggregate(. ~ Fleet + Yr, data=catage2o, sum)
catage2o[,1] <- NULL

# Juvenile and adults total catch by year calculation
catage2o[,2] <- NULL
SKJ_BB_catage_Juv_Adu <- catage2o %>%
  mutate(Juv_Catch = catage2o[,2],      # Change the values according to the species. BET:2:18-19:42; YFT:2:11-12:28; SKJ:2-3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

SKJ_BB_catage_Juv_Adu # See the dataframe

# Save the dataset as .csv
write.csv(SKJ_BB_catage_Juv_Adu, "SKJ_BB_catage_Juv_Adu.csv", row.names = FALSE)

# Juvenile and adults total catch by year PERCENTAGE calculation
SKJ_BB_catage_JA_perc <- data.frame(
  Yr = SKJ_BB_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_BB_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_BB_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100)

# Save the dataset as .csv
write.csv(SKJ_BB_catage_JA_perc, "SKJ_BB_catage_JA_perc.csv", row.names = FALSE)

###############################################################  Plots  ###############################################################

############# Catch total
catage5 <- SKJ_LL_catage_Juv_Adu ##### Change the gear (LL, BB, GN,...) to obtain the plots

#--------------------------------Data representation---------------------------#
catage6 <- pivot_longer(catage5, c('Juv_Catch', 'Adu_Catch'))
names(catage6)[2] <- "Stanza"
names(catage6)[3] <- "Catch"

# Here we order the species x catch otherwise they'll appear alphabetically in the plot
class(catage6$Stanza)
catage6$Stanza <- factor(catage6$Stanza, levels=c('Adu_Catch', 'Juv_Catch'))
sp_colors<- c('Juv_Catch'='dodgerblue', 'Adu_Catch'='dodgerblue4')

#Stacked barchart - With our relevant species
ggplot(catage6, aes(fill=Stanza, y=Catch, x=Yr)) + 
  geom_bar(position="stack", stat="identity")+
  theme(text = element_text(size=11))+
  scale_fill_manual(values = sp_colors,name='Stanza')+xlab('Year')+ylab('Catch (t)')+
  ggtitle('Longline fleets juvenile and adult SKJ catch in the Indian Ocean [1950-2022]')+ #Change the gear name in the title
  scale_y_continuous(labels = scales::label_number(scale = 1)) # Converts scientific notation to numerical

############# Catch Percentage
catage5 <- SKJ_LL_catage_JA_perc ##### Change the gear (LL, BB, GN,...) to obtain the plots

#--------------------------------Data representation---------------------------#
catage6 <- pivot_longer(catage5, c('Juv_Catch', 'Adu_Catch'))
names(catage6)[2] <- "Stanza"
names(catage6)[3] <- "Catch"
# Here we order the species x catch otherwise they'll appear alphabetically in the plot
class(catage6$Stanza)

catage6$Stanza <- factor(catage6$Stanza, levels=c('Adu_Catch', 'Juv_Catch'))

sp_colors<- c('Juv_Catch'='dodgerblue', 'Adu_Catch'='dodgerblue4')

#Stacked barchart - With our relevant species
ggplot(catage6, aes(fill=Stanza, y=Catch, x=Yr)) + 
  geom_bar(position="stack", stat="identity")+
  theme(text = element_text(size=11))+
  scale_fill_manual(values = sp_colors,name='Stanza')+xlab('Year')+ylab('Catch (%)')+
  ggtitle('Longline fleets juvenile and adult SKJ catch proportion in the Indian Ocean [1950-2022]')+ #Change the gear name in the title
  scale_y_continuous(labels = scales::label_number(scale = 1)) # Converts scientific notation to numerical

###################################################################################################################################

