########################################################################################
###### Script to calculate how much PS catch has been made inside and outside     ######
###### of the model area by each fleet and how much of it was at PSFS and at PSLS ######
###### Species: SKJ, YFT, BET, ALB, SWO                                           ######
###### Author: Roger Amate (AZTI)                                                 ######
###### year: 2024                                                                 ######
########################################################################################

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
setwd("~/.../4. Rised catch data/2022_Data/csv")

#--------------------------------Data preparation------------------------------#

# Read the data
catch <- read.csv('Rised_catch_5sp_catches_ecoregion_with_model_reasinged.csv', sep=',')
catch[1] <- NULL
head(catch)
str(catch)

# Check for NA values
any(is.na(catch)) # With this line we check if there are empty values (NA) in the database

catch$Fleet <- trimws(catch$Fleet) # Delete the white spaces that some fleets have at the end of their acronym.

catch_21y <- catch%>%subset(Year >= '2000') # Subset the data for the years that we want

Fleets <- aggregate(MT ~  Ecoregion_name + Gear + SchoolType + Fleet + Year, data = catch_21y, sum) 

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

# Agreegate the Gear, Fleet and SchoolType into one column so we can calculate

# Aggregate the FS to the PS column when corresponding
Fleets$Gear_code <- ifelse((Fleets$SchoolType==('FS') | Fleets$SchoolType==('UNCL')) & Fleets$Gear==('PS'), 'PSFS', Fleets$Gear_code)

# Aggregate the LS to the PS column when corresponding
Fleets$Gear_code <- ifelse(Fleets$SchoolType==('LS') & Fleets$Gear==('PS'), 'PSLS', Fleets$Gear_code)

# Unite the two columns of Gear_code and Fleet into one called 'Fleet'
Fleets$Fleet <- paste(Fleets$Gear_code, Fleets$Fleet, sep = '_')

# Delete the columns that are no longer needed
Fleets[,c(2,3,7)] <-NULL 

# Vector with the names of the selected fleets 
flotas_interes = c('PS_IND', 'PS_JOR', 'PS_KEN', 'PS_LKA', 'PS_MYS', 'PS_THA', 'PSFS_BLZ', 'PSFS_IRN', 'PSFS_JPN', 'PSFS_KOR', 'PSFS_MYS',
                   'PSFS_NEIPS', 'PSFS_NEISU', 'PSFS_THA', 'PSFS_TZA', 'PSLS_BLZ', 'PSLS_IRN', 'PSLS_JPN', 'PSLS_KOR', 'PSLS_LKA', 'PSLS_MYS',
                   'PSLS_NEIPS', 'PSLS_NEISU', 'PSLS_PHL', 'PSLS_THA', 'PSLS_TZA', 'PS_IDN', 'PS_MOZ', 'PSLS_IDN', 'PSFS_EUESP', 'PSFS_SYC',
                   'PSFS_EUFRA', 'PSFS_EUITA', 'PSFS_EUMYT', 'PSFS_MUS', 'PSLS_EUESP', 'PSLS_SYC',
                   'PSLS_EUFRA', 'PSLS_EUITA', 'PSLS_EUMYT', 'PSLS_MUS')

# Filter the data.frame
catch4 <- Fleets[Fleets$Fleet %in% flotas_interes, ]

# Now we want to aggregate between inside and outside the model area
# Convert the ecoregion column with new names: Model_area, Outside_MA
catch4$Ecoregion_name <- ifelse(catch4$Ecoregion_name %in% c('Aghullas Current', 'Indian Ocean Gyre', 'Southern Ocean', 'Leeuwin Current'), 
  'Outside_MA', catch4$Ecoregion_name)
catch5 <- aggregate(MT ~  Ecoregion_name +  Fleet + Year, data = catch4, sum) 

# Unite the two columns of Ecoregion and Fleet into one called 'Fleet'
catch5$Fleet_area <- paste(catch5$Fleet, catch5$Ecoregion_name, sep = '_')
catch6 <- catch5

# Delate the columns that we don't need anymore
catch6[,c(1,2)] <-NULL 

# Now we want to put the years as columns and the fleets as rows
# Change the df configuration so now the SPECIES_GROUP are the columns
catch7 <- catch6 %>% pivot_wider(names_from = Year, values_from = MT)

# Replace NA with 0
catch7  <- replace(catch7 , is.na(catch7), 0)

# Change the working directory to the desired folder where to save the data
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/4. Rised catch data/2022_Data/csv")

# Save the dataframe as a .csv
write.csv(catch7, 'FS-LS_fleets_catch_separation_in_and_out_MA.csv')

########## NOTE: Once we obtain the resultant data frame ("FS-LS_fleets_catch_separation_in_and_out_MA.csv") using excel
########## we will calculate, using the catches of each fleet inside and outside the MA, the proportion of catches at FAD 
########## and FSC for each fleet and each year inside the MA. (Reference: README)

################################################################################################################################
################################################################################################################################
