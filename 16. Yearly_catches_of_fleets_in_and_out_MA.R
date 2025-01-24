################################################################################
###### Script to calculate theyearly proportion of catches inside the     ######
###### model area by fleets that fish inside and outside the model area   ######
###### Multiple species                                                   ######
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
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('DSEI', 'LIFT', 'BS', 'TRAW', 'TRAP', 'HARP', 'RR', 'FN', 'CN'), 'Others', Fleets$Gear_code)

# Check that the column Gear_code has been completely filled
any(is.na(Fleets$Gear_code)) # We should se FALSE here. If true it means that we missed a Gear on one of the lines

catch2 <- Fleets
##### Create a new column that is going to be filled with the corresponding EwE model fleet
catch2$EwE_fleet <-NA

unique(catch2$Gear_code)

# All BB fleets are NonEU_BB
catch2$EwE_fleet <- ifelse(catch2$Gear_code=='BB', 'NonEU_BB', catch2$EwE_fleet)

# All Others fleets are NonEU_Others
catch2$EwE_fleet <- ifelse(catch2$Gear_code=='Others', 'NonEU_Others', catch2$EwE_fleet)

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
catch2$EwE_fleet <- ifelse(catch2$EwE_fleet=='EUESP_PS' & catch2$SchoolType=='FS', 'EUESP_PSFS', catch2$EwE_fleet)
catch2$EwE_fleet <- ifelse(catch2$EwE_fleet=='EUESP_PS' & catch2$SchoolType=='LS', 'EUESP_PSLS', catch2$EwE_fleet)
catch2$EwE_fleet <- ifelse(catch2$EwE_fleet=='EUFRA_PS' & catch2$SchoolType=='FS', 'EUFRA_PSFS', catch2$EwE_fleet)
catch2$EwE_fleet <- ifelse(catch2$EwE_fleet=='EUFRA_PS' & catch2$SchoolType=='LS', 'EUFRA_PSLS', catch2$EwE_fleet)
catch2$EwE_fleet <- ifelse(catch2$EwE_fleet=='SYC_PS' & catch2$SchoolType=='FS', 'SYC_PSFS', catch2$EwE_fleet)
catch2$EwE_fleet <- ifelse(catch2$EwE_fleet=='SYC_PS' & catch2$SchoolType=='LS', 'SYC_PSLS', catch2$EwE_fleet)
catch2$EwE_fleet <- ifelse(catch2$EwE_fleet=='NonEU_PS' & (catch2$SchoolType=='UNCL' | catch2$SchoolType=='FS'), 'NonEU_PSFS', catch2$EwE_fleet)
catch2$EwE_fleet <- ifelse(catch2$EwE_fleet=='NonEU_PS' & catch2$SchoolType=='LS', 'NonEU_PSLS', catch2$EwE_fleet)

# Aggregate the FS to the PS column when corresponding
catch2$Gear_code <- ifelse((catch2$SchoolType==('FS') | catch2$SchoolType==('UNCL')) & catch2$Gear_code==('PS'), 'PSFS', catch2$Gear_code)

# Aggregate the LS to the PS column when corresponding
catch2$Gear_code <- ifelse(catch2$SchoolType==('LS') & catch2$Gear==('PS'), 'PSLS', catch2$Gear_code)


# Unite the two columns of Gear_code and Fleet into one called 'Fleet'
catch2$Fleet <- paste(catch2$Gear_code, catch2$Fleet, sep = '_')

# We select only the columns that interest us
catch2[,c(3,4,8)] <- NULL

# Vector con los nombres de las flotas deseadas
flotas_interes = c('BB_IDN', 'GN_IDN', 'GN_MOZ', 'LI_IDN', 'LI_MDG', 'LI_MOZ', 'LL_AUS', 'LL_BLZ', 'LL_CHN', 'LL_EUESP', 'LL_EUPRT',
                   'LL_EUREU', 'LL_GIN', 'LL_IDN', 'LL_IND', 'LL_IRN', 'LL_JPN', 'LL_KOR', 'LL_MOZ', 'LL_MUS', 'LL_MYS', 'LL_NEICE',
                   'LL_NEIFR', 'LL_PHL', 'LL_SEN', 'LL_SYC', 'LL_THA', 'LL_TWN', 'LL_TZA', 'LL_VUT', 'Others_IDN', 'Others_MOZ', 'PSFS_IDN',
                   'PSFS_MOZ', 'PSLS_IDN')

# Filtrar el data.frame
catch3 <- catch2[catch2$Fleet %in% flotas_interes, ]
unique(catch3$Fleet)

##### Create a new column that is going to be filled with the corresponding EwE model fleet
catch3$in_out <-NA
catch3$in_out <- ifelse(catch3$Ecoregion_name==('Model_area'), 'in', 'out')


##### Catches inside MA ########
catchMA <- catch3%>%subset(in_out=='in')

catchMA2 <- catchMA[,c(3,4,5)]

# Aggregating the catches by fleet
CatchMA3 <- aggregate(MT ~ Year + Fleet, data = catchMA2, sum) 

# Crear una matriz con Year como columnas, Fleet como filas, y MT como datos
catchMA4 <- reshape(CatchMA3, idvar = "Fleet", timevar = "Year", direction = "wide")

# Reemplazar NA por 0
catchMA4[is.na(catchMA4)] <- 0

write.csv(catchMA4, 'Yearly_catches_of_in&out_Fleets_inside_MA.csv')

##### Catches outside Ma#########
catch_OutMA <- catch3%>%subset(in_out=='out')

catch_OutMA2 <- catch_OutMA[,c(3,4,5)]

# Aggregating the catches by fleet
Catch_OutMA3 <- aggregate(MT ~ Year + Fleet, data = catch_OutMA2, sum) 

# Crear una matriz con Year como columnas, Fleet como filas, y MT como datos
catch_OutMA4 <- reshape(Catch_OutMA3, idvar = "Fleet", timevar = "Year", direction = "wide")

# Reemplazar NA por 0
catch_OutMA4[is.na(catch_OutMA4)] <- 0

write.csv(catch_OutMA4, 'Yearly_catches_of_in&out_Fleets_outside_MA.csv')
















