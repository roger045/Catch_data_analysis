################################################################################
###### Script to calculate which fleets are fishing inside the model area ###### 
###### and how many catches of the are made inside/outside our model area ######
###### Tropical tunas: ALB, YFT, BET, SKJ, SWO                            ######
###### Author: Roger Amate (AZTI)                                         ######
###### year: 2023                                                         ######
################################################################################

#install.packages('tidyr')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('readr')
#install.packages('readxl')
#install.packages('hrbrthemes')

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(hrbrthemes)

# Set the working directory
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/4. Rised catch data/2022_Data/csv")


#--------------------------------Data preparation------------------------------#

# read the data
catch <- read.csv('Rised_catch_5sp_catches_ecoregion_with_model_reasinged.csv', sep=',')
catch[1] <- NULL
head(catch)
str(catch)

# check for NA values
any(is.na(catch)) # With this line we check if there are empty values (NA) in the database

catch$Fleet <- trimws(catch$Fleet) # Dealte the white spaces that some fleets have at the end of their acronym.

# Add a column classifying groups depending if the species is Tropical, temperate or subtropical billfishes
# Create an empty column named: SPECIES_GROUP
catch$SPECIES_GROUP <- NA

# Albacore = Temperate 
catch$SPECIES_GROUP <- ifelse(catch$Species=='ALB', 'Temperate tunas', catch$SPECIES_GROUP)

# Bigeye = Tropical
catch$SPECIES_GROUP <- ifelse(catch$Species=='BET', 'Tropical tunas', catch$SPECIES_GROUP)

# Skipjack = Tropical
catch$SPECIES_GROUP <- ifelse(catch$Species=='SKJ', 'Tropical tunas', catch$SPECIES_GROUP)

# Yellowfin = Tropical
catch$SPECIES_GROUP <- ifelse(catch$Species=='YFT', 'Tropical tunas', catch$SPECIES_GROUP)

# Swordfish = Subtropical Billfishes
catch$SPECIES_GROUP <- ifelse(catch$Species=='SWO', 'Subtropical billfishes', catch$SPECIES_GROUP)


catch_21y <- catch%>%subset(Year >= '2000') # Subset the data for the years that we want

Fleets <- aggregate(MT ~  Species + Ecoregion_name + Gear + SchoolType + Fleet + Year, data = catch_21y, sum) 

# Create a new column that is going to be filled with the general gear code of the gear ex. LL, PS, BB,...
Fleets$Gear_code <-NA

# Check which Gear we have
unique(Fleets$Gear)

### Fill the column###

# Longline (LL)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('ELL', 'FLL', 'LL', 'LLEX', 'LG', 'SLL'), 'LL', Fleets$Gear_code)

# Purse seine (PS)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('PS', 'PSS', 'RIN', 'RNOF'), 'PS', Fleets$Gear_code)

# Bait Boat (BB)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('BB', 'BBOF'), 'BB', Fleets$Gear_code)

# Gill net (GN)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('GILL', 'GIOF', 'GL'), 'GN', Fleets$Gear_code)

# Line (L)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('HAND', 'TROL', 'LLCO', 'SPOR', 'HLOF', 'TROLM'), 'LI', Fleets$Gear_code)

# Others (Others)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('DSEI', 'LIFT', 'BS', 'TRAW', 'TRAP', 'HARP', 'RR', 'FN', 'CN'), 'OT', Fleets$Gear_code)

# Check that the column Gear_code has been completely filled
any(is.na(Fleets$Gear_code)) # We should se FALSE here. If true it means that we missed a Gear on one of the lines

catch2 <- Fleets
##### Create a new column that is going to be filled with the corresponding EwE model fleet
catch2$EwE_fleet <-NA

unique(catch2$Gear_code)

# All BB fleets are NonEU_BB
catch2$EwE_fleet <- ifelse(catch2$Gear_code=='BB', 'NonEU_BB', catch2$EwE_fleet)

# All Others fleets are NonEU_Others
catch2$EwE_fleet <- ifelse(catch2$Gear_code=='OT', 'NonEU_Others', catch2$EwE_fleet)

# All LI fleets are NonEU_LI
catch2$EwE_fleet <- ifelse(catch2$Gear_code=='LI', 'NonEU_LI', catch2$EwE_fleet)

# All GN fleets are NonEU_GN
catch2$EwE_fleet <- ifelse(catch2$Gear_code=='GN', 'NonEU_GN', catch2$EwE_fleet)

# All PS fleets are NonEU_PS
catch2$EwE_fleet <- ifelse(catch2$Gear_code=='PS', 'NonEU_PS', catch2$EwE_fleet)

# Two longline fleets are EU, the rest are NonEU_LL
catch2$EwE_fleet <- ifelse(catch2$Gear_code=='LL', 'NonEU_LL', catch2$EwE_fleet)

# We correct PS for the Spanish PS fleet
catch2$EwE_fleet <- ifelse(catch2$Fleet=='EUESP' & catch2$Gear_code=='PS', 'EUESP_PS', catch2$EwE_fleet)

# We correct PS for the French PS fleet
catch2$EwE_fleet <- ifelse((catch2$Fleet=='EUFRA' | catch2$Fleet=='EUITA' | catch2$Fleet=='EUMYT' | catch2$Fleet=='MUS') & catch2$Gear_code=='PS', 'EUFRA_PS', catch2$EwE_fleet)

# We correct PS for the Seychellean PS fleet
catch2$EwE_fleet <- ifelse(catch2$Fleet=='SYC' & catch2$Gear_code=='PS', 'SYC_PS', catch2$EwE_fleet)

# We correct for the FS and LS

##### Convert all the school_type= UNCL to FS
catch2$SchoolType <- ifelse(catch2$Gear_code=='PS' & catch2$SchoolType=='UNCL', 'FS', catch2$SchoolType)

catch2$EwE_fleet <- ifelse(catch2$Gear_code=='PS', paste(catch2$EwE_fleet, catch2$SchoolType, sep=''), catch2$EwE_fleet)

# We select only the columns that interest us
catch2[,c(3,4,5,8)] <- NULL

# Subsetting only the catches in our model area
catch2MA <- subset(catch2, Ecoregion_name=='Model_area')

# Aggregating the catches by fleet
Catch3MA <- aggregate(MT ~ Year + Species + EwE_fleet, data = catch2MA, sum) 

# Make a subseted dataframe for each species
ALB_catch <- subset(Catch3MA, Species=='ALB')
BET_catch <- subset(Catch3MA, Species=='BET')
SKJ_catch <- subset(Catch3MA, Species=='SKJ')
SWO_catch <- subset(Catch3MA, Species=='SWO')
YFT_catch <- subset(Catch3MA, Species=='YFT')

# Change saving directory
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/4. Rised catch data/2022_Data/csv")

# Change the df configuration so now the years are in columns 

# ALB
ALBcatch <- ALB_catch %>% pivot_wider(names_from = Year, values_from = MT)
ALBcatch[is.na(ALBcatch)] <- 0
write.csv(ALBcatch, 'ALB.csv')

# BET
BETcatch <- BET_catch %>% pivot_wider(names_from = Year, values_from = MT)
BETcatch[is.na(BETcatch)] <- 0
write.csv(BETcatch, 'BET.csv')

# SKJ
SKJcatch <- SKJ_catch %>% pivot_wider(names_from = Year, values_from = MT)
SKJcatch[is.na(SKJcatch)] <- 0
write.csv(SKJcatch, 'SKJ.csv')

# SWO
SWOcatch <- SWO_catch %>% pivot_wider(names_from = Year, values_from = MT)
SWOcatch[is.na(SWOcatch)] <- 0
write.csv(SWOcatch, 'SWO.csv')

# YFT
YFTcatch <- YFT_catch %>% pivot_wider(names_from = Year, values_from = MT)
YFTcatch[is.na(YFTcatch)] <- 0
write.csv(YFTcatch, 'YFT.csv')

###################################################################################################################################
###################################################################################################################################


