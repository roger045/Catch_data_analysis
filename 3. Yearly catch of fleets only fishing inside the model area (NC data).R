################################################################################
###### Script to calculate the catch of IOTC species for the fleets       ######
###### that only fish inside the model area, for which we can use         ######
###### the nominal catch database                                         ######
###### Species: all species contained in the IOTC NC database             ######
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
setwd("~/.../2. Workind data/8. Nominal_catch")


#--------------------------------Data preparation------------------------------#

# read the data
catch <- read.csv('IOTC_NC_RAW_[2000-2022].csv', sep=',')
unique(catch$SPECIES_CODE)
catch2 <- catch[,c(1,3,5,11,12,16)]

##### Create a new column that is going to be filled with the corresponding EwE model fleet
catch2$EwE_fleet <-NA
unique(catch2$FISHERY_GROUP_CODE)

# All BB fleets are NonEU_BB
catch2$EwE_fleet <- ifelse(catch2$FISHERY_GROUP_CODE=='BB', 'NonEU_BB', catch2$EwE_fleet)

# All Others fleets are NonEU_Others
catch2$EwE_fleet <- ifelse(catch2$FISHERY_GROUP_CODE=='OT', 'NonEU_Others', catch2$EwE_fleet)

# All LI fleets are NonEU_LI
catch2$EwE_fleet <- ifelse(catch2$FISHERY_GROUP_CODE=='LI', 'NonEU_LI', catch2$EwE_fleet)

# All GN fleets are NonEU_GN
catch2$EwE_fleet <- ifelse(catch2$FISHERY_GROUP_CODE=='GN', 'NonEU_GN', catch2$EwE_fleet)

# All PS fleets are NonEU_PS
catch2$EwE_fleet <- ifelse(catch2$FISHERY_GROUP_CODE=='PS', 'NonEU_PS', catch2$EwE_fleet)

# Two longline fleets are EU, the rest are NonEU_LL
catch2$EwE_fleet <- ifelse(catch2$FISHERY_GROUP_CODE=='LL', 'NonEU_LL', catch2$EwE_fleet)

# We correct PS for the Spanish PS fleet
catch2$EwE_fleet <- ifelse(catch2$FLEET_CODE=='EUESP' & catch2$FISHERY_GROUP_CODE=='PS', 'EUESP_PS', catch2$EwE_fleet)

# We correct PS for the French PS fleet
catch2$EwE_fleet <- ifelse((catch2$FLEET_CODE=='EUFRA' | catch2$FLEET_CODE=='EUITA' | catch2$FLEET_CODE=='EUMYT' | catch2$FLEET_CODE=='MUS') & catch2$FISHERY_GROUP_CODE=='PS', 'EUFRA_PS', catch2$EwE_fleet)

# We correct PS for the Seychellean PS fleet
catch2$EwE_fleet <- ifelse(catch2$FLEET_CODE=='SYC' & catch2$FISHERY_GROUP_CODE=='PS', 'SYC_PS', catch2$EwE_fleet)


# Unite the two columns of Gear_code and Fleet into one called 'Fleet'
catch2$Fleet <- paste(catch2$FISHERY_GROUP_CODE, catch2$FLEET_CODE, sep = '_')

# Select only the columns that we need: Year, Species, Catch, Fleet
catch3 <- catch2[,c(1,5,6,8)]

# write a vetor with the names of the fleets that fish inside and outside or only outside the model area
flotas_no_interes = c('BB_IDN', 'GN_IDN', 'GN_MOZ', 'LI_IDN', 'LI_MDG', 'LI_MOZ', 'LL_AUS', 'LL_BLZ', 'LL_CHN', 'LL_EUESP', 'LL_EUPRT',
                   'LL_EUREU', 'LL_GIN', 'LL_IDN', 'LL_IND', 'LL_IRN', 'LL_JPN', 'LL_KOR', 'LL_MOZ', 'LL_MUS', 'LL_MYS', 'LL_NEICE',
                   'LL_NEIFR', 'LL_PHL', 'LL_SEN', 'LL_SYC', 'LL_THA', 'LL_TWN', 'LL_TZA', 'LL_VUT', 'OT_IDN', 'OT_MOZ', 'PS_IDN',
                   'PS_MOZ')

# Filter the data frame
catch4 <- catch3[!(catch3$Fleet %in% flotas_no_interes), ]

# Check how many species do we have now
unique(catch4$SPECIES_CODE)

# Obtain the unique names of the column Fleet
fleet_names <- unique(catch4$Fleet)

# Sum the catches by year, species code and fleet
catch5 <- catch4 %>% aggregate(CATCH ~ YEAR + SPECIES_CODE + Fleet, sum)

# Obtain the unique years of the column YEAR
Years <- unique(catch5$YEAR)

# Change the directory where we want the files of catches by fllet and year to be saved
setwd("~/.../10. Catch_calculation_data/1. Catches_of_NC_fleets_by_year")

# Iterate over each year and save one .csv file per year
for (yr in Years) {
  # Filter the dataframe for each year
  subset_year <- subset(catch5, YEAR == yr)

  # Create a matrix with Fleet as columns, SPECIES_CODE as rows, and CATCH as data
  pivot_data <- reshape(subset_year, idvar = "SPECIES_CODE", timevar = "Fleet", direction = "wide")
  
  # Replace NA for 0
  pivot_data[is.na(pivot_data)] <- 0
  
  # Save the dataset as a .csv
  csv_filename <- paste( yr, "_catches_by_fleet.csv", sep = "")
  write.csv(pivot_data, file = csv_filename, row.names = FALSE)
  
  print(paste("Archivo", csv_filename, "creado con éxito."))
}
###################################################################################################################################
