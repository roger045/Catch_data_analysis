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

                  ################ IN & OUT FLEETS IDENTIFICATION #################

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

# Group by Gear, Ecoregion, Fleet and SchoolType 
Fleets_21y <- Fleets %>% filter(Year >= '2000') %>% group_by(Ecoregion_name, Gear_code, SchoolType, Fleet) %>% summarise(MT = sum(MT)) %>% as.data.frame()

# Aggregate the FS to the PS column when corresponding
Fleets_21y$Gear_code <- ifelse((Fleets_21y$SchoolType==('FS') | Fleets_21y$SchoolType==('UNCL')) & Fleets_21y$Gear==('PS'), 'PSFS', Fleets_21y$Gear_code)

# Aggregate the LS to the PS column when corresponding
Fleets_21y$Gear_code <- ifelse(Fleets_21y$SchoolType==('LS') & Fleets_21y$Gear==('PS'), 'PSLS', Fleets_21y$Gear_code)

# Delete the SchoolType columns as it is no longer needed
Fleets_21y[3] <-NULL 

# Aggregate the Gear, Fleet and SchoolType into one column 
Fleets_21y <- Fleets_21y %>% group_by(Ecoregion_name, Gear_code, Fleet) %>% summarise(MT = sum(MT)) %>% as.data.frame()

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

Outside_fleets <- Fleets_outside_model_area$Fleet # Vector of all the fleets that doesn't fish anything inside the model area

# Extract the fleets for which their catches are ONLY INSIDE the study area
Fleets_fully_model_area <- Fleets_21y_wide %>% subset(Model_area != 0 & `Southern Ocean`==0 & `Leeuwin Current`==0 
                                                      & `Indian Ocean Gyre`==0 & `Aghullas Current`==0)
Fleets_fully_model_area

Fully_model_area_fleets <- Fleets_fully_model_area$Fleet # Vector of the fleets that only fish inside the model area

# Extract the fleets that part of their catches are inside the study area
Fleets_partially_model_area <- Fleets_21y_wide %>% subset(Model_area != 0 & (`Southern Ocean`!=0 | `Leeuwin Current`!=0 
                                                      | `Indian Ocean Gyre`!=0 | `Aghullas Current`!=0))
Fleets_partially_model_area

# Vector of the fleets that fish inside and outside the model area
Partially_model_area_fleets <- Fleets_partially_model_area$Fleet 

################################  Extract the % of catches per Ecoregion ################################################
Fleets_21y_perc <- Fleets_21y_wide

# First we calculate the total sumation of all cels
total_catches <- sum(Fleets_21y_perc[,-1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
Fleets_21y_perc[, -1] <- Fleets_21y_perc[, -1] / total_catches * 100 
Fleets_21y_perc

# Ecoregion % 
catch_perc_ecoregion <-colSums(Fleets_21y_perc[,-1])
sum(catch_perc_ecoregion)
                               #####################################################

                               ################ INDEX CALCULATIONS #################
#--------------------------------Data preparation------------------------------#
catch1 <- catch
# Create a new column that is going to be filled with the general gear code of the gear ex. LL, PS, BB,...
catch1$Gear_code <-NA

# Check which Gear we have
unique(catch1$Gear)

# Longline (LL)
catch1$Gear_code <- ifelse(catch1$Gear %in% c('ELL', 'FLL', 'LL', 'LLEX', 'LG', 'SLL'), 'LL', catch1$Gear_code)
# Purse seine (PS)
catch1$Gear_code <- ifelse(catch1$Gear %in% c('PS', 'PSS', 'RIN', 'RNOF'), 'PS', catch1$Gear_code)
# Bait Boat (BB)
catch1$Gear_code <- ifelse(catch1$Gear %in% c('BB', 'BBOF', 'BBM', 'BBN'), 'BB', catch1$Gear_code)
# Gillnet (GN)
catch1$Gear_code <- ifelse(catch1$Gear %in% c('GILL', 'GIOF', 'GL'), 'GN', catch1$Gear_code)
# Line (L)
catch1$Gear_code <- ifelse(catch1$Gear %in% c('HAND', 'TROL', 'LLCO', 'SPOR', 'HLOF', 'TROLM', 'TROLN'), 'LI', catch1$Gear_code)
# Others (Others)
catch1$Gear_code <- ifelse(catch1$Gear %in% c('DSEI', 'LIFT', 'BS', 'TRAW', 'TRAP', 'HARP', 'RR', 'FN', 'CN'), 'Others', catch1$Gear_code)

# Check that the column Gear_code has been completely filled
any(is.na(catch1$Gear_code)) # We should se FALSE here. If true it means that we missed a Gear on one of the lines

# Aggregate the FS to the PS column when corresponding
catch1$Gear_code <- ifelse(catch1$SchoolType==('FS') & catch1$Gear==('PS'), 'PSFS', catch1$Gear_code)

# Aggregate the LS to the PS column when corresponding
catch1$Gear_code <- ifelse(catch1$SchoolType==('LS') & catch1$Gear==('PS'), 'PSLS', catch1$Gear_code)

# Delete the SchoolType columns as it is no longer needed
catch1[10] <-NULL 
# Unite the two columns of Gear_code and Fleet into one called 'Fleet'
catch1$Fleet <- paste(catch1$Gear_code, catch1$Fleet, sep = '_')

# Delete the Gear_code columns as it is no longer needed
catch1[14] <-NULL 

# Rename the datset so we don't mess it 
dataset_all<-catch1
dataset_all_20<- dataset_all %>% filter(Year >= '2000') # filter the yers of our interest


######## Objective: calculate the SPECIFICITY ###### 
# The specificicty represents the proportion of all the catches of that fleet that have been made inside the model area for all the years
library(plyr)
step1fl<-ddply(dataset_all_20, .(Fleet,Ecoregion_name), function(x) data.frame(sum_catch=sum(x$MT,na.rm=T)))

# step 2: sum of the mean catches by fleet over all regions
step2fl<-ddply(step1fl,.(Fleet),function(x) data.frame(Ecoregion_name=x$Ecoregion_name, sum_catch=x$sum_catch, 
                                                       sum_all_catch=sum(x$sum_catch,na.rm=T)))  

# Step 3: calculate the proportion of the catch in each ecoregion
step2fl$Specificity<-step2fl$sum_catch/step2fl$sum_all_catch
step2fl$fleet2<-as.factor(step2fl$Fleet)
step2fl2 <- step2fl %>% filter (Ecoregion_name =="Model_area")

# Delate line 84 as it contain an error, as the PS_EGY fleet has registers in the model area, with Num, but 0 in the MT column. 
step2fl2 <- slice(step2fl2, -84)
unique(step2fl2$Fleet)
specificity_method='sum'
###########################################################################################################################

######## Objective: calculate the FIDELITY######
# Which is for all the cells that the Model_Area ecoregion has, in how many of this cells each fleet has fished. 
names(dataset_all_20)
TcatchEcoLongLat<-ddply(dataset_all_20, .(Ecoregion_name,Latitude,Longitude), function(x) Tcatch = sum(x$MT,na.rm=T)) # Here we calculate how much of catches have been 
summary(TcatchEcoLongLat)                                                                                             # made in each unique latitude and longitude combi

Number_of_cells_Eco<-ddply(TcatchEcoLongLat, .(Ecoregion_name),summarize, count = length(Ecoregion_name)) # Here we calculate how many points (Lat,Long) fall in each Ecor
head(Number_of_cells_Eco) # Shows how many cells each ecoregion has

# Next, we calculate for each fleet, how many catch points fall in each ecoregion (for how many cells in each ecoregion there are registers of catch) 
Number_of_cells_Eco_fleet<-ddply(dataset_all_20, .(Ecoregion_name,Fleet), function(x) count = dim(unique(x[,c('Latitude','Longitude')]))[1])
head(Number_of_cells_Eco_fleet)

# We then merge the two datasets so we know for each fleet, and ecoregion, how many points the ecoregion have, and on how many of it the fleet has catches.
fidelity_prep<-merge(Number_of_cells_Eco,Number_of_cells_Eco_fleet,by.x="Ecoregion_name",by.y="Ecoregion_name")
head(fidelity_prep)
names(fidelity_prep)<-c("Ecoregion_name","Number_of_cells_Eco","Fleet","Number_of_cells_Eco_fleet")
head(fidelity_prep)
                                 
# Here we divide the numer of cells that the fleet has catches by the number of cells that the ecoregion have.
fidelity_prep$Fidelity<-fidelity_prep$Number_of_cells_Eco_fleet/fidelity_prep$Number_of_cells_Eco
summary(fidelity_prep)
levels(as.factor(fidelity_prep$Fleet))
fidelity_prep$Fleet<-as.factor(fidelity_prep$Fleet)

# Here we subset only the registers for the Model Area
data_model_area_fidel <- fidelity_prep %>% filter(Ecoregion_name == "Model_area") 

# Delate line 84 as it contain an error, as the PS_EGY fleet has registers in the model area, with Num, but 0 in the MT column. 
data_model_area_fidel <- slice(data_model_area_fidel, -84)

# The fidelity dataframe showing for each fleet (flag + gear) how many of the cells of the model area have catches in it. 
view(data_model_area_fidel)
                            







                               ################ EXPLORATORY TABLES #################
                               

################################  Extract the % of catches per SPECIES_GROUP ################################################
Fleets <- aggregate(MT ~ Ecoregion_name + SPECIES_GROUP, data = catch_21y, sum) 

# Change the data frame configuration so now the SPECIES_GROUP are the columns
Fleets_wide <- Fleets %>% pivot_wider(names_from = SPECIES_GROUP, values_from = MT)
Fleets_wide
Fleets_perc <- Fleets_wide

# First we calculate the total sumation of all cels
total_catches <- sum(Fleets_perc[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
Fleets_perc[, -1] <- Fleets_perc[, -1] / total_catches * 100 
Fleets_perc

# Species group % 
catch_perc_species_group <-colSums(Fleets_perc[,-1])
sum(catch_perc_species_group)

##################################  Extract the % of catches per Species ################################################
catch_per <- aggregate(MT ~ Ecoregion_name + Species, data = catch_21y, sum) 
# Change the data frame configuration so now the SPECIES are the columns
catch_wide <- catch_per %>% pivot_wider(names_from = Species, values_from = MT)
catch_wide
catch_perc <- catch_wide

# First we calculate the total sumation of all cels
total_catches <- sum(catch_perc[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
catch_perc[, -1] <- catch_perc[, -1] / total_catches * 100 
catch_perc

# Species % 
catch_perc_species <-colSums(catch_perc[,-1])
sum(catch_perc_species)

##################################  Extract the % of catches per Gear ################################################
gear_per <- aggregate(MT ~ Ecoregion_name + Gear_code, data = Fleets, sum)

# Change the data frame configuration so now the GEAR_CODE are the columns
gear_wide <- gear_per %>% pivot_wider(names_from = Gear_code, values_from = MT)

# Fill NA values with 0
gear_wide  <- replace(gear_wide , is.na(gear_wide ), 0)
gear_wide
gear_perc <- gear_wide

# First we calculate the total sumation of all cels
gear_catches <- sum(gear_perc[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
gear_perc[, -1] <- gear_perc[, -1] / gear_catches * 100 
gear_perc

# Gear % 
gear_perc_er <-colSums(gear_perc[,-1])
sum(gear_perc_er)

##################################  Extract the % of catches per Gear x Species #########################################
gear_sp <- aggregate(MT ~ Species + Gear_code, data = Fleets, sum) 

# Change the data frame configuration so now the SPECIES are the columns
gearsp_wide <- gear_sp %>% pivot_wider(names_from = Species, values_from = MT)

# Fill NA values with 0
gearsp_wide  <- replace(gearsp_wide , is.na(gearsp_wide ), 0)
gearsp_wide
gear_sp <- gearsp_wide

# First we calculate the total sumation of all cels
gearsp_catches <- sum(gear_sp[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
gear_sp[, -1] <- gear_sp[, -1] / gearsp_catches * 100 
gear_sp

# Gear x Species % 
gearsp <-colSums(gear_sp[,-1])
sum(gearsp)


#############################  Extract the % of catches per Gear x Species in the model area ######################################

gear_sp <- aggregate(MT ~ Species + Gear_code + Ecoregion_name, data = Fleets, sum) 

#Subset only the catches in the model area
gear_sp <- gear_sp%>%subset(Ecoregion_name=='Model_area') # Subset the data for the years that we want

#Delate the Ecoregion_name column
gear_sp[,3] <- NULL

# Change the data frame configuration so now the SPECIES_GROUP are the columns
gearsp_wide <- gear_sp %>% pivot_wider(names_from = Species, values_from = MT)

# Fill NA values with 0
gearsp_wide  <- replace(gearsp_wide , is.na(gearsp_wide ), 0)
gearsp_wide
gear_sp <- gearsp_wide

# First we calculate the total sumation of all cels
gearsp_catches <- sum(gear_sp[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
gear_sp[, -1] <- gear_sp[, -1] / gearsp_catches * 100 
gear_sp

# Gear x Species % in the model area 
gearsp <-colSums(gear_sp[,-1])
sum(gearsp)


#############################  Extract the % of catches of PS x SchoolType & Species in the model area ######################################
gear_sp <- aggregate(MT ~ Species + Gear_code + Ecoregion_name, data = Fleets_21y, sum) 

#Subset only the catches in the model area
gear_sp <- gear_sp%>%subset(Ecoregion_name=='Model_area') # Subset the data for the years that we want
gear_sp <- gear_sp%>%subset(Gear_code=='PSFS' | Gear_code=='PSLS')

#Delate the Ecoregion_name column
gear_sp[,3] <- NULL

# Change the data frame configuration so now the SPECIES_GROUP are the columns
gearsp_wide <- gear_sp %>% pivot_wider(names_from = Species, values_from = MT)

# Fill NA values with 0
gearsp_wide  <- replace(gearsp_wide , is.na(gearsp_wide ), 0)
gearsp_wide
gear_sp <- gearsp_wide

# First we calculate the total sumation of all cels
gearsp_catches <- sum(gear_sp[, -1]) # exclude the first row and column as they contain the names 

# Then we divide each cell by the total and multiply it by 100 to obtain the % excluding first row and column
gear_sp[, -1] <- gear_sp[, -1] / gearsp_catches * 100 
gear_sp

# Species group % 
gearsp <-colSums(gear_sp[,-1])
sum(gearsp)
###############################################################################################################################################
                                              #####################################################







