###################################################################################################
###### Script to calculate the yearly proportion of catches inside the model area by fleets  ######
###### that fish inside and outside the model area and multiply this proportion to the total ######
###### catch of each species in each year and by each fleet obtained from the NC database    ######    
###### Multiple species                                                                      ######
###### Author: Roger Amate (AZTI)                                                            ######
###### year: 2023                                                                            ######
###################################################################################################

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

# read the data
catch <- read.csv('Rised_catch_5sp_catches_ecoregion_with_model_reasinged.csv', sep=',')
catch[1] <- NULL
head(catch)
str(catch)

# check for NA values
any(is.na(catch)) # With this line we check if there are empty values (NA) in the database

catch$Fleet <- trimws(catch$Fleet) # Delete the white spaces that some fleets have at the end of their acronym.

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

# Subset the data for the years that we want
catch_21y <- catch%>%subset(Year >= '2000') 

# Sum the total catches (MT) aggregated by the variables that interests us
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

##### NOTE: With this two files ("Yearly_catches_of_in&out_Fleets_inside_MA.csv" and "Yearly_catches_of_in&out_Fleets_outside_MA.csv"
##### we calculate the proportion of catches that each fleet has inside the model area. We do it in Excel and save the resulting matrix 
##### as a .csv file with the proportions by fleet and year as: Proportions_yearly_catches_in&out_fleets.csv


#######################################################
###########################
# Set the working directory
setwd("~/.../2. Workind data/8. Nominal_catch")


#--------------------------------Data preparation------------------------------#

# read the data
catch <- read.csv('IOTC_NC_RAW_[2000-2022].csv', sep=',')
unique(catch$SPECIES_CODE)
# Select only the columns with useful data for this step
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
catch3 <- catch2[,c(1,5,6,7,8)]

#  Obtain the names of the Fleet column
fleet_names <- unique(catch3$Fleet)

# Vector with the names of the fleets for which we will do the calculations
flotas_interes = c('BB_IDN', 'GN_IDN', 'GN_MOZ', 'LI_IDN', 'LI_MDG', 'LI_MOZ', 'LL_AUS', 'LL_BLZ', 'LL_CHN', 'LL_EUESP', 'LL_EUPRT',
                      'LL_EUREU', 'LL_GIN', 'LL_IDN', 'LL_IND', 'LL_IRN', 'LL_JPN', 'LL_KOR', 'LL_MOZ', 'LL_MUS', 'LL_MYS', 'LL_NEICE',
                      'LL_NEIFR', 'LL_PHL', 'LL_SEN', 'LL_SYC', 'LL_THA', 'LL_TWN', 'LL_TZA', 'LL_VUT', 'OT_IDN', 'OT_MOZ', 'PS_IDN',
                      'PS_MOZ')
# Filter the data.frame
catch4 <- catch3[catch3$Fleet %in% flotas_interes, ]

# Check how many species do we have now
unique(catch4$SPECIES_CODE)

# Change the working directory to where we have saved the file: Proportions_yearly_catches_in&out_fleets.csv
setwd("~/.../4. Rised catch data/2022_Data/csv")

# Load the yearly proportions dataset
proportions <- read.csv('Proportions_yearly_catches_in&out_fleets.csv', sep=';', header=T, row.names=1)
colnames(proportions) <- gsub("^X", "", colnames(proportions)) # remove the X at the begining of the names of the columns

# Set the working first directory again
setwd("~/.../2. Workind data/8. Nominal_catch")

# Multyply the data frame of catch4 and teh data frame of catch proportions inside the MA 
# Multiply each register of a catch by a fleet and a year by the proportion of catches 
# of that same fleet in that same year inside the model area and place the result in the
# corresponidng cell of the Catch_prop column.
catch4$Catch_prop <- NA
for (row in 1:nrow(catch4)){
  year <- catch4[row,'YEAR']
  fleet <- catch4[row, 'Fleet']
  catch <- catch4[row, 'CATCH']
  
  prop <- proportions[fleet,]
  for (column in colnames(prop)){
      if (column == year) {
        pro <- prop[column]
      }
  }
  
  catch4$Catch_prop[row] <- pro * catch
}

# Undo the list to be able to use the function aggregate
catch4$Catch_prop <- unlist(catch4$Catch_prop)

# Subset the data for only the fleets for which we will assume catches from the proportion of the RC
flotas_interes2 = c('BB_IDN', 'GN_IDN', 'GN_MOZ', 'LI_IDN', 'LI_MDG', 'LI_MOZ', 'LL_AUS', 'LL_BLZ', 'LL_EUPRT',
                    'LL_EUREU', 'LL_GIN', 'LL_IDN', 'LL_IND', 'LL_IRN', 'LL_MOZ', 'LL_MUS', 'LL_MYS', 'LL_NEICE',
                    'LL_NEIFR', 'LL_PHL', 'LL_SEN', 'LL_THA', 'LL_TZA', 'LL_VUT', 'OT_IDN', 'OT_MOZ', 'PS_IDN',
                    'PS_MOZ')

catch4_sub <- subset(catch4, Fleet %in% flotas_interes2)

# Aggregate catch by EwE fleet
catch5 <- catch4_sub %>% aggregate(Catch_prop ~ YEAR + SPECIES_CODE + EwE_fleet, sum)

# Obtain the names of the Fleet column
fleet_names <- unique(catch5$EwE_fleet)

# Change the directory where we want to save the files of catches y fleet of the model and year inside the model area.
setwd("~/.../10. Catch_calculation_data/2. Catches_of_in&out_fleets_RC_prop")

# Iterate over each model fleet and save the file as a .csv
for (fleet in fleet_names) {
  # Filter the data.frame for the actual fleet
  subset_fleet <- subset(catch5, EwE_fleet == fleet)
  
  # Create a matrix with YEAR as columns, Species_CODE as rows and CATCH as data
  pivot_data <- reshape(subset_fleet, idvar = "SPECIES_CODE", timevar = "YEAR", direction = "wide")
  
  # Replace NA for 0
  pivot_data[is.na(pivot_data)] <- 0
  
  # Save the data frame as a .csv
  csv_filename <- paste( fleet, "_catches_by_year.csv", sep = "") 
  print(paste("Archivo", csv_filename, "correctly created.")) # confirmation menssage that the file has been correctly created.

#####################################################################################################################################
#####################################################################################################################################
