################################################################################
###### Script to calculate which fleets are fishing inside the model area ###### 
###### and how many catches are made inside/outside our model area        ######
###### Species included in the dataset: ALB, YFT, BET, SKJ, SWO           ######
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

# Set the working directory where the dataset is saved
setwd("~/...")


#--------------------------------Data preparation------------------------------#
# Read the data 
# The data used is the file obtained as a result of the script  "1. Assignment to ecoregions of IOTC rised catch data"
catch <- read.csv('Rised_catch_5sp_catches_ecoregion_with_model_reasinged.csv', sep=',')
catch[1] <- NULL
head(catch)
str(catch)

# Check for NA values
any(is.na(catch)) # With this line we check if there are empty values (NA) in the database

catch$Fleet <- trimws(catch$Fleet) # Dealte the white spaces that some fleets have at the end of their acronym.

# Add a column classifying groups depending if the species is tropical, temperate or subtropical billfishes
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

# Swordfish = Subtropical billfishes
catch$SPECIES_GROUP <- ifelse(catch$Species=='SWO', 'Subtropical billfishes', catch$SPECIES_GROUP)

catch_21y <- catch%>%subset(Year >= '2000') # Subset the data for the years that we want (2000-2022)
###################

# Aggregate, summing, the catch in tones (MT) data by multiple variables
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

# Gillnet (GN)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('GILL', 'GIOF', 'GL'), 'GN', Fleets$Gear_code)

# Line (LI)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('HAND', 'TROL', 'LLCO', 'SPOR', 'HLOF', 'TROLM'), 'LI', Fleets$Gear_code)

# Others (Others)
Fleets$Gear_code <- ifelse(Fleets$Gear %in% c('DSEI', 'LIFT', 'BS', 'TRAW', 'TRAP', 'HARP', 'RR', 'FN', 'CN'), 'Others', Fleets$Gear_code)

# Check that the column Gear_code has been completely filled
any(is.na(Fleets$Gear_code)) # We should see FALSE here. If true it means that we missed a Gear on one of the lines.
###################

#### To identify which fleets (flag + gear) fish inside the study area we want to create a new column with a % of how many registers 
#### of each fleet are inside the study area.
## If 100% - the fleets operates completely in the model area
## If >0% and <100% the fleet operates inside and outside our study area
## If 0% fleet operates entirely outside our study area

# Aggregate the Gear, Fleet and SchoolType into one column 
# Aggregate by Gear_code
Fleets_21y <- aggregate(MT ~ Species + Ecoregion_name + Gear_code + SchoolType + Fleet, data = Fleets, sum) 

# Aggregate the FS to the PS column when corresponding
Fleets_21y$Gear_code <- ifelse((Fleets_21y$SchoolType==('FS') | Fleets_21y$SchoolType==('UNCL')) & Fleets_21y$Gear==('PS'), 'PSFS', Fleets_21y$Gear_code)

# Aggregate the LS to the PS column when corresponding
Fleets_21y$Gear_code <- ifelse(Fleets_21y$SchoolType==('LS') & Fleets_21y$Gear==('PS'), 'PSLS', Fleets_21y$Gear_code)

# Delete the SchoolType columns as it is no longer needed
Fleets_21y[3] <-NULL 

# Unite the two columns of Gear_code and Fleet into one called 'Fleet'
Fleets_21y$Fleet <- paste(Fleets_21y$Gear_code, Fleets_21y$Fleet, sep = '_')

# Delete the Gear_code columns as it is no longer needed
Fleets_21y[2] <-NULL 

# Change the df configuration so now the ecoregions are the columns
Fleets_21y_wide <- Fleets_21y %>% pivot_wider(names_from = Ecoregion_name, values_from = MT)

Fleets_21y_wide

# Fill NA values with 0
Fleets_21y_wide <- replace(Fleets_21y_wide, is.na(Fleets_21y_wide), 0)


################################  Extract where each fleet fishes ######################################################

# Extract the list of fleets that have catches in the model area
Fleets_model_area <- Fleets_21y_wide %>% subset(Model_area != 0)
Fleets_model_area

All_fleets <- Fleets_21y_wide$Fleet # Vector of all the fleets that fish, inside, inside and outside, and outside

# Extract the fleets that don't have any catch inside the study area
Fleets_outside_model_area <- Fleets_21y_wide %>% subset(Model_area == 0)
Fleets_outside_model_area

Outside_fleets <- Fleets_outside_model_area$Fleet # Vector of all the fleets taht doesn't fish anything inside the model area

# Extract the fleets that all their catches are inside the study area
Fleets_fully_model_area <- Fleets_21y_wide %>% subset(Model_area != 0 & `Southern Ocean`==0 & `Leeuwin Current`==0 
                                                      & `Indian Ocean Gyre`==0 & `Aghullas Current`==0)
Fleets_fully_model_area

Fully_model_area_fleets <- Fleets_fully_model_area$Fleet


# Extract the fleets that part of their catches are inside the study area
Fleets_partially_model_area <- Fleets_21y_wide %>% subset(Model_area != 0 & (`Southern Ocean`!=0 | `Leeuwin Current`!=0 
                                                      | `Indian Ocean Gyre`!=0 | `Aghullas Current`!=0))
Fleets_partially_model_area

Partially_model_area_fleets <- Fleets_partially_model_area$Fleet



################################  Extract the % of catches per Ecoregion ################################################

Fleets_21y_perc <- Fleets_21y_wide

# First we calculate the total sumation of all cels
total_catches <- sum(Fleets_21y_perc[,-1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
Fleets_21y_perc[, -1] <- Fleets_21y_perc[, -1] / total_catches * 100 

# 
Fleets_21y_perc

# Ecoregion % 

catch_perc_ecoregion <-colSums(Fleets_21y_perc[,-1])

sum(catch_perc_ecoregion)

################################  Extract the % of catches per SPECIES_GROUP ################################################

Fleets <- aggregate(MT ~ Ecoregion_name + SPECIES_GROUP, data = catch_21y, sum) 

# Change the df configuration so now the SPECIES_GROUP are the columns
Fleets_wide <- Fleets %>% pivot_wider(names_from = SPECIES_GROUP, values_from = MT)

Fleets_wide

Fleets_perc <- Fleets_wide

# First we calculate the total sumation of all cels
total_catches <- sum(Fleets_perc[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
Fleets_perc[, -1] <- Fleets_perc[, -1] / total_catches * 100 

# 
Fleets_perc

# Species group % 

catch_perc_species_group <-colSums(Fleets_perc[,-1])

sum(catch_perc_species_group)

##################################  Extract the % of catches per Species ################################################

catch_per <- aggregate(MT ~ Ecoregion_name + Species, data = catch_21y, sum) 

# Change the df configuration so now the SPECIES_GROUP are the columns
catch_wide <- catch_per %>% pivot_wider(names_from = Species, values_from = MT)

catch_wide

catch_perc <- catch_wide

# First we calculate the total sumation of all cels
total_catches <- sum(catch_perc[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
catch_perc[, -1] <- catch_perc[, -1] / total_catches * 100 

# 
catch_perc

# Species group % 

catch_perc_species <-colSums(catch_perc[,-1])

sum(catch_perc_species)

##################################  Extract the % of catches per Gear ################################################

gear_per <- aggregate(MT ~ Ecoregion_name + Gear_code, data = Fleets, sum) 

# Change the df configuration so now the SPECIES_GROUP are the columns
gear_wide <- gear_per %>% pivot_wider(names_from = Gear_code, values_from = MT)

# Fill NA values with 0
gear_wide  <- replace(gear_wide , is.na(gear_wide ), 0)

gear_wide

gear_perc <- gear_wide

# First we calculate the total sumation of all cels
gear_catches <- sum(gear_perc[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
gear_perc[, -1] <- gear_perc[, -1] / gear_catches * 100 

# 
gear_perc

# Species group % 

gear_perc_er <-colSums(gear_perc[,-1])

sum(gear_perc_er)

##################################  Extract the % of catches per Gear x Species #########################################

gear_sp <- aggregate(MT ~ Species + Gear_code, data = Fleets, sum) 

# Change the df configuration so now the SPECIES_GROUP are the columns
gearsp_wide <- gear_sp %>% pivot_wider(names_from = Species, values_from = MT)

# Fill NA values with 0
gearsp_wide  <- replace(gearsp_wide , is.na(gearsp_wide ), 0)

gearsp_wide

gear_sp <- gearsp_wide

# First we calculate the total sumation of all cels
gearsp_catches <- sum(gear_sp[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
gear_sp[, -1] <- gear_sp[, -1] / gearsp_catches * 100 

# 
gear_sp

# Species group % 

gearsp <-colSums(gear_sp[,-1])

sum(gearsp)


#############################  Extract the % of catches per Gear x Species in the model area ######################################

gear_sp <- aggregate(MT ~ Species + Gear_code + Ecoregion_name, data = Fleets, sum) 

#Subset only the catches in the model area
gear_sp <- gear_sp%>%subset(Ecoregion_name=='Model_area') # Subset the data for the years that we want

#Delate the Ecoregion_name column
gear_sp[,3] <- NULL

# Change the df configuration so now the SPECIES_GROUP are the columns
gearsp_wide <- gear_sp %>% pivot_wider(names_from = Species, values_from = MT)

# Fill NA values with 0
gearsp_wide  <- replace(gearsp_wide , is.na(gearsp_wide ), 0)

gearsp_wide

gear_sp <- gearsp_wide

# First we calculate the total sumation of all cels
gearsp_catches <- sum(gear_sp[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
gear_sp[, -1] <- gear_sp[, -1] / gearsp_catches * 100 

# 
gear_sp

# Species group % 

gearsp <-colSums(gear_sp[,-1])

sum(gearsp)


#############################  Extract the % of catches of PS x SchoolType & Species in the model area ######################################

gear_sp <- aggregate(MT ~ Species + Gear_code + Ecoregion_name, data = Fleets_21y, sum) 

#Subset only the catches in the model area
gear_sp <- gear_sp%>%subset(Ecoregion_name=='Model_area') # Subset the data for the years that we want

gear_sp <- gear_sp%>%subset(Gear_code=='PSFS' | Gear_code=='PSLS')
#Delate the Ecoregion_name column
gear_sp[,3] <- NULL

# Change the df configuration so now the SPECIES_GROUP are the columns
gearsp_wide <- gear_sp %>% pivot_wider(names_from = Species, values_from = MT)

# Fill NA values with 0
gearsp_wide  <- replace(gearsp_wide , is.na(gearsp_wide ), 0)

gearsp_wide

gear_sp <- gearsp_wide

# First we calculate the total sumation of all cels
gearsp_catches <- sum(gear_sp[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
gear_sp[, -1] <- gear_sp[, -1] / gearsp_catches * 100 

# 
gear_sp

# Species group % 

gearsp <-colSums(gear_sp[,-1])

sum(gearsp)









