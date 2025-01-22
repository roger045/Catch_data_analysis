################################################################################
###### Script to calculate from the stock assessment outputs, how much    ######
###### juevniles and adults each fleet is fishing                         ######
###### Tropical tunas: ALB, YFT, BET, SKJ, SWO                            ######
###### Author: Roger Amate (AZTI)                                         ######
###### year: 2023                                                         ######
################################################################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(hrbrthemes)
library(scales)



########## BIGEYE TUNA ############################################################################################
# Set the working directory
setwd("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/7. Stock_assessment/1. Datasets with data on the model area as a unit (correct)/BET")


################################ Data preparation ################################################################################
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

# from the column Beg.Mid, we have to select one value, B or M. B is the value at the beginning, and M is 
# the value at the middle. I selected M because the values of biomass are smaller, and therefore the future measures that 
# that could be taken from the results are more conservative. 

catage2 <- catage1 %>% subset(Yr>=193 & Yr<=384) #Cambiar el valor segun la especie BET:193-384; YFT:13-304
unique(catage2$Yr)
unique(catage2$Fleet)
class(catage2$Fleet)

# Separamos entre dos datasets segun el area
#catage2_1 <- catage2 %>% subset(Area==1)
#catage2_2 <- catage2 %>% subset(Area==2)
#catage2_3 <- catage2 %>% subset(Area==3)
#catage2_4 <- catage2 %>% subset(Area==4)

# Multiplicamos los valores de biomasa de cada edad por el valor correspondiente previamente calculado en QGIS.
#catage2_11 <- catage2_1[, 4:44]*1
#catage2_22 <- catage2_2[, 4:44]*1
#catage2_33 <- catage2_3[, 4:44]*1
#catage2_44 <- catage2_4[, 4:44]*1

#Unimos los datasets
#catage11 <- cbind(catage2_1[,1:3], catage2_11)
#catage22 <- cbind(catage2_2[,1:3], catage2_22)
#catage33 <- cbind(catage2_3[,1:3], catage2_33)
#catage44 <- cbind(catage2_4[,1:3], catage2_44)


#catage2 <- rbind(catage11, catage22, catage33, catage44)


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

write.csv(catage2, 'BET_catch_at_age_by_gear.csv')
#catage2 <- read.csv('BET_catch_at_age_by_gear.csv', sep=',')

# convert Yr column to a character column
class(catage2$Yr)
catage2$Yr <- as.numeric(catage2$Yr)
################################

###############################################################   BAITBOAT  ######################################################


catage2BB <- catage2 %>% subset(Fleet=='BB')
#catage2BB[,1] <- NULL

# Aggregate by the quarter (Yr) 
catage3BB <- aggregate(. ~ Fleet + Yr, data=catage2BB, sum)

Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)

Year[,2] <- NULL

catage3BB <- cbind(Year, catage2BB)

catage3BB[,c(2,3)] <- NULL

BET_BB_catage <- aggregate(. ~ Year, data=catage3BB, mean)

write.csv(BET_BB_catage, 'BET_BB_catage.csv')

# Calculo juveniles y adultos
BET_BB_catage_Juv_Adu <- BET_BB_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
BET_BB_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_BB_catage_Juv_Adu, "BET_BB_catage_Juv_Adu.csv", row.names = FALSE)

BET_BB_catage_JA_perc <- data.frame(
  Year = BET_BB_catage_Juv_Adu$Year,
  Juv_Catch = (BET_BB_catage_Juv_Adu$Juv_Catch/ rowSums(BET_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_BB_catage_Juv_Adu$Adu_Catch / rowSums(BET_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_BB_catage_JA_perc, "BET_BB_catage_JA_perc.csv", row.names = FALSE)


###############################################################   LONGLINE  ######################################################

# Subset by the gear
catage2LL <- catage2 %>% subset(Fleet=='LL')
#catage2LL[,1] <- NULL

# Aggregate by the quarter (Yr) 
catage3LL <- aggregate(. ~ Fleet + Yr, data=catage2LL, sum)

Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)

Year[,2] <- NULL

catage3LL <- cbind(Year, catage3LL)

catage3LL[,c(2,3)] <- NULL

BET_LL_catage <- aggregate(. ~ Year, data=catage3LL, mean)

write.csv(BET_LL_catage, 'BET_LL_catage.csv')

# Calculo juveniles y adultos
BET_LL_catage_Juv_Adu <- BET_LL_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
BET_LL_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_LL_catage_Juv_Adu, "BET_LL_catage_Juv_Adu.csv", row.names = FALSE)

BET_LL_catage_JA_perc <- data.frame(
  Year = BET_LL_catage_Juv_Adu$Year,
  Juv_Catch = (BET_LL_catage_Juv_Adu$Juv_Catch/ rowSums(BET_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_LL_catage_Juv_Adu$Adu_Catch / rowSums(BET_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_LL_catage_JA_perc, "BET_BB_catage_JA_perc_MA.csv", row.names = FALSE)


###############################################################   PSFS   ######################################################

# Subset by the gear
catage2PSFS <- catage2 %>% subset(Fleet=='PSFS')
#catage2PSFS[,1] <- NULL

# Aggregate by the quarter (Yr) 
catage3PSFS <- aggregate(. ~ Fleet + Yr, data=catage2PSFS, sum)

Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)

Year[,2] <- NULL

catage3PSFS <- cbind(Year, catage3PSFS)

catage3PSFS[,c(2,3)] <- NULL

BET_PSFS_catage <- aggregate(. ~ Year, data=catage3PSFS, mean)

write.csv(BET_PSFS_catage, 'BET_PSFS_catage.csv')

# Calculo juveniles y adultos
BET_PSFS_catage_Juv_Adu <- BET_PSFS_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
BET_PSFS_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_PSFS_catage_Juv_Adu, "BET_PSFS_catage_Juv_Adu.csv", row.names = FALSE)

BET_PSFS_catage_JA_perc <- data.frame(
  Year = BET_PSFS_catage_Juv_Adu$Year,
  Juv_Catch = (BET_PSFS_catage_Juv_Adu$Juv_Catch/ rowSums(BET_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_PSFS_catage_Juv_Adu$Adu_Catch / rowSums(BET_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_PSFS_catage_JA_perc, "BET_PSFS_catage_JA_perc.csv", row.names = FALSE)


###############################################################   Others   ######################################################

# Subset by the gear
catage2oth <- catage2 %>% subset(Fleet=='Others')
#catage2oth[,1] <- NULL

# Aggregate by the quarter (Yr) 
catage3oth <- aggregate(. ~ Fleet + Yr, data=catage2oth, sum)

Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)

Year[,2] <- NULL

catage3oth <- cbind(Year, catage3oth)

catage3oth[,c(2,3)] <- NULL

BET_oth_catage <- aggregate(. ~ Year, data=catage3oth, mean)

write.csv(BET_oth_catage, 'BET_oth_catage.csv')

# Calculo juveniles y adultos
BET_oth_catage_Juv_Adu <- BET_oth_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
BET_oth_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_oth_catage_Juv_Adu, "BET_oth_catage_Juv_Adu.csv", row.names = FALSE)

BET_oth_catage_JA_perc <- data.frame(
  Year = BET_oth_catage_Juv_Adu$Year,
  Juv_Catch = (BET_oth_catage_Juv_Adu$Juv_Catch/ rowSums(BET_oth_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_oth_catage_Juv_Adu$Adu_Catch / rowSums(BET_oth_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_oth_catage_JA_perc, "BET_oth_catage_JA_perc.csv", row.names = FALSE)


###############################################################   PSLS   ######################################################

# Subset by the gear
catage2PSLS <- catage2 %>% subset(Fleet=='PSLS')
#catage2PSLS[,1] <- NULL

# Aggregate by the quarter (Yr) 
catage3PSLS <- aggregate(. ~ Fleet + Yr, data=catage2PSLS, sum)

Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)

Year[,2] <- NULL

catage3PSLS <- cbind(Year, catage3PSLS)

catage3PSLS[,c(2,3)] <- NULL

BET_PSLS_catage <- aggregate(. ~ Year, data=catage3PSLS, mean)

write.csv(BET_PSLS_catage, 'BET_PSLS_catage.csv')

# Calculo juveniles y adultos
BET_PSLS_catage_Juv_Adu <- BET_PSLS_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
BET_PSLS_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_PSLS_catage_Juv_Adu, "BET_PSLS_catage_Juv_Adu.csv", row.names = FALSE)

BET_PSLS_catage_JA_perc <- data.frame(
  Year = BET_PSLS_catage_Juv_Adu$Year,
  Juv_Catch = (BET_PSLS_catage_Juv_Adu$Juv_Catch/ rowSums(BET_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_PSLS_catage_Juv_Adu$Adu_Catch / rowSums(BET_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_PSLS_catage_JA_perc, "BET_PSLS_catage_JA_perc.csv", row.names = FALSE)


###############################################################   Line   ######################################################

# Subset by the gear
catage2LI <- catage2 %>% subset(Fleet=='LI')
#catage2LI[,1] <- NULL

# Aggregate by the quarter (Yr) 
catage3LI <- aggregate(. ~ Fleet + Yr, data=catage2LI, sum)

Year <- expand_grid(Year = seq(1975, 2022), Repetition = 1:4)

Year[,2] <- NULL

catage3LI <- cbind(Year, catage3LI)

catage3LI[,c(2,3)] <- NULL

BET_LI_catage <- aggregate(. ~ Year, data=catage3LI, mean)

write.csv(BET_LI_catage, 'BET_LI_catage.csv')

# Calculo juveniles y adultos
BET_LI_catage_Juv_Adu <- BET_LI_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:18]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 19:42])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
BET_LI_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_LI_catage_Juv_Adu, "BET_LI_catage_Juv_Adu.csv", row.names = FALSE)

BET_LI_catage_JA_perc <- data.frame(
  Year = BET_LI_catage_Juv_Adu$Year,
  Juv_Catch = (BET_LI_catage_Juv_Adu$Juv_Catch/ rowSums(BET_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (BET_LI_catage_Juv_Adu$Adu_Catch / rowSums(BET_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(BET_LI_catage_JA_perc, "BET_LI_catage_JA_perc.csv", row.names = FALSE)


###############################################################  Plots  ###############################################################

############# Catch total
catage5 <- BET_oth_catage_Juv_Adu
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
  ggtitle('Others juvenile and adult BET catch in the model area [1975-2022]')+
  scale_y_continuous(labels = scales::label_number(scale = 1)) # convierte la notacion científica en numérica
#facet_grid(Stanza ~.) #Use if we want 5 different plots all together at once


############# Catch Percentage
catage5 <- BET_oth_catage_JA_perc
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
  ggtitle('Others juvenile and adult BET catch proportion in the model area [1975-2022]')+
  scale_y_continuous(labels = scales::label_number(scale = 1)) # convierte la notacion científica en numérica
#facet_grid(Stanza ~.) #Use if we want 5 different plots all together at once


###################################################################################################################################


########## YELLOWFIN TUNA ############################################################################################
# Set the working directory
setwd("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/7. Stock_assessment/1. Datasets with data on the model area as a unit (correct)/YFT")

################## Data preparation 
# read the data
catage <- read.csv('YFT_catch_at_age.csv', sep=',')
catage[1] <- NULL # dalate the first column which is only an index
head(catage)
str(catage)

# check for NA values
any(is.na(catage)) # With this line we check if there are empty values (NA) in the database

# Subset the dataframe with only the columns that contain information that interests us
# which are: all the ones with the sizes (X0, X1, ...), Yr (191=1975 & 379=2022), Beg.Mid

catage1 <- catage %>% select(-c('XX', 'Sex', 'XX.1', 'Type', 'Morph', 'Seas', 'XX.2', 'Era', 'Area'))

# from the column Beg.Mid, we have to select one value, B or M. B is the value at the beginning, and M is 
# the value at the middle. I selected M because the values of biomass are smaller, and therefore the future measures that 
# that could be taken from the results are more conservative. 

catage2 <- catage1 %>% subset(Yr>=13 & Yr<=304) #Cambiar el valor segun la especie BET:192-379; YFT:12-300
unique(catage2$Yr)
unique(catage2$Fleet)
class(catage2$Fleet)

# Separamos entre dos datasets segun el area
#catage2_1 <- catage2 %>% subset(Area==1)
#catage2_2 <- catage2 %>% subset(Area==2)
#catage2_3 <- catage2 %>% subset(Area==3)
#catage2_4 <- catage2 %>% subset(Area==4)

# Multiplicamos los valores de biomasa de cada edad por el valor correspondiente previamente calculado en QGIS.
#catage2_11 <- catage2_1[, 4:32]*0.998828
#catage2_22 <- catage2_2[, 4:32]*0.209938
#catage2_33 <- catage2_3[, 4:32]*0.000790
#catage2_44 <- catage2_4[, 4:32]*0.809032

#Unimos los datasets
#catage11 <- cbind(catage2_1[,1:3], catage2_11)
#catage22 <- cbind(catage2_2[,1:3], catage2_22)
#catage33 <- cbind(catage2_3[,1:3], catage2_33)
#catage44 <- cbind(catage2_4[,1:3], catage2_44)


#catage2 <- rbind(catage11, catage22, catage33, catage44)


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

write.csv(catage2, 'YFT_catch_at_age_by_gear.csv')
#catage2 <- read.csv('YFT_catch_at_age_by_gear.csv', sep=',')
# Not use when calculating for BET or YFT
#catage2 <-catage2[c(1:292),]

# convert Yr column to a character column
class(catage2$Yr)
catage2$Yr <- as.numeric(catage2$Yr)
################################

###############################################################   GILLNET  ######################################################


catage2GN <- catage2 %>% subset(Fleet=='GN')
#catage2GN[,1] <- NULL

Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)

Year[,2] <- NULL

# Aggregate by the quarter (Yr) 
catage2GN <- aggregate(. ~ Fleet + Yr, data=catage2GN, sum)

catage3GN <- cbind(Year, catage2GN)

catage3GN[,c(2,3)] <- NULL

YFT_GN_catage <- aggregate(. ~ Year, data=catage3GN, mean)

write.csv(YFT_GN_catage, 'YFT_GN_catage.csv')

# Calculo juveniles y adultos
YFT_GN_catage_Juv_Adu <- YFT_GN_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
YFT_GN_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_GN_catage_Juv_Adu, "YFT_GN_catage_Juv_Adu.csv", row.names = FALSE)

YFT_GN_catage_JA_perc <- data.frame(
  Year = YFT_GN_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_GN_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_GN_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_GN_catage_Juv_Adu$Adu_Catch / rowSums(YFT_GN_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_GN_catage_JA_perc, "YFT_GN_catage_JA_perc.csv", row.names = FALSE)



###############################################################   LINE  ######################################################


catage2LI <- catage2 %>% subset(Fleet=='LI')
#catage2LI[,1] <- NULL

Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)

Year[,2] <- NULL

# Aggregate by the quarter (Yr) 
catage2LI <- aggregate(. ~ Fleet + Yr, data=catage2LI, sum)

catage3LI <- cbind(Year, catage2LI)

catage3LI[,c(2,3)] <- NULL

YFT_LI_catage <- aggregate(. ~ Year, data=catage3LI, mean)

write.csv(YFT_LI_catage, 'YFT_LI_catage.csv')

# Calculo juveniles y adultos
YFT_LI_catage_Juv_Adu <- YFT_LI_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
YFT_LI_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_LI_catage_Juv_Adu, "YFT_LI_catage_Juv_Adu.csv", row.names = FALSE)

YFT_LI_catage_JA_perc <- data.frame(
  Year = YFT_LI_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_LI_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_LI_catage_Juv_Adu$Adu_Catch / rowSums(YFT_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_LI_catage_JA_perc, "YFT_LI_catage_JA_perc.csv", row.names = FALSE)


###############################################################   LONGLINE  ######################################################


catage2LL <- catage2 %>% subset(Fleet=='LL')
#catage2LL[,1] <- NULL

Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)

Year[,2] <- NULL

# Aggregate by the quarter (Yr) 
catage2LL <- aggregate(. ~ Fleet + Yr, data=catage2LL, sum)

catage3LL <- cbind(Year, catage2LL)

catage3LL[,c(2,3)] <- NULL

YFT_LL_catage <- aggregate(. ~ Year, data=catage3LL, mean)

write.csv(YFT_LL_catage, 'YFT_LL_catage.csv')

# Calculo juveniles y adultos
YFT_LL_catage_Juv_Adu <- YFT_LL_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
YFT_LL_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_LL_catage_Juv_Adu, "YFT_LL_catage_Juv_Adu.csv", row.names = FALSE)

YFT_LL_catage_JA_perc <- data.frame(
  Year = YFT_LL_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_LL_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_LL_catage_Juv_Adu$Adu_Catch / rowSums(YFT_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_LL_catage_JA_perc, "YFT_LL_catage_JA_perc.csv", row.names = FALSE)



###############################################################   OTHERS  ######################################################

catage2oth <- catage2 %>% subset(Fleet=='Others')
#catage2oth[,1] <- NULL

Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)

Year[,2] <- NULL

# Aggregate by the quarter (Yr) 
catage2oth <- aggregate(. ~ Fleet + Yr, data=catage2oth, sum)

catage3oth <- cbind(Year, catage2oth)

catage3oth[,c(2,3)] <- NULL

YFT_oth_catage <- aggregate(. ~ Year, data=catage3oth, mean)

write.csv(YFT_oth_catage, 'YFT_oth_catage.csv')

# Calculo juveniles y adultos
YFT_oth_catage_Juv_Adu <- YFT_oth_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
YFT_oth_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_oth_catage_Juv_Adu, "YFT_oth_catage_Juv_Adu.csv", row.names = FALSE)

YFT_oth_catage_JA_perc <- data.frame(
  Year = YFT_oth_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_oth_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_oth_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_oth_catage_Juv_Adu$Adu_Catch / rowSums(YFT_oth_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_oth_catage_JA_perc, "YFT_oth_catage_JA_perc.csv", row.names = FALSE)
###############################################################   BAITBOAT  ######################################################

catage2BB <- catage2 %>% subset(Fleet=='BB')
#catage2BB[,1] <- NULL

Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)

Year[,2] <- NULL

# Aggregate by the quarter (Yr) 
catage2BB <- aggregate(. ~ Fleet + Yr, data=catage2BB, sum)

catage3BB <- cbind(Year, catage2BB)

catage3BB[,c(2,3)] <- NULL

YFT_BB_catage <- aggregate(. ~ Year, data=catage3BB, mean)

write.csv(YFT_BB_catage, 'YFT_BB_catage.csv')

# Calculo juveniles y adultos
YFT_BB_catage_Juv_Adu <- YFT_BB_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
YFT_BB_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_BB_catage_Juv_Adu, "YFT_BB_catage_Juv_Adu.csv", row.names = FALSE)

YFT_BB_catage_JA_perc <- data.frame(
  Year = YFT_BB_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_BB_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_BB_catage_Juv_Adu$Adu_Catch / rowSums(YFT_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_BB_catage_JA_perc, "YFT_BB_catage_JA_perc.csv", row.names = FALSE)
###############################################################   PSFS  ######################################################

catage2PSFS <- catage2 %>% subset(Fleet=='PSFS')
#catage2PSFS[,1] <- NULL

Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)

Year[,2] <- NULL

# Aggregate by the quarter (Yr) 
catage2PSFS <- aggregate(. ~ Fleet + Yr, data=catage2PSFS, sum)

catage3PSFS <- cbind(Year, catage2PSFS)

catage3PSFS[,c(2,3)] <- NULL

YFT_PSFS_catage <- aggregate(. ~ Year, data=catage3PSFS, mean)

write.csv(YFT_PSFS_catage, 'YFT_PSFS_catage.csv')

# Calculo juveniles y adultos
YFT_PSFS_catage_Juv_Adu <- YFT_PSFS_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
YFT_PSFS_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_PSFS_catage_Juv_Adu, "YFT_PSFS_catage_Juv_Adu.csv", row.names = FALSE)

YFT_PSFS_catage_JA_perc <- data.frame(
  Year = YFT_PSFS_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_PSFS_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_PSFS_catage_Juv_Adu$Adu_Catch / rowSums(YFT_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_PSFS_catage_JA_perc, "YFT_PSFS_catage_JA_perc.csv", row.names = FALSE)
###############################################################   PSLS  ######################################################

catage2PSLS <- catage2 %>% subset(Fleet=='PSLS')
#catage2PSLS[,1] <- NULL

Year <- expand_grid(Year = seq(1950, 2022), Repetition = 1:4)

Year[,2] <- NULL

# Aggregate by the quarter (Yr) 
catage2PSLS <- aggregate(. ~ Fleet + Yr, data=catage2PSLS, sum)

catage3PSLS <- cbind(Year, catage2PSLS)

catage3PSLS[,c(2,3)] <- NULL

YFT_PSLS_catage <- aggregate(. ~ Year, data=catage3PSLS, mean)

write.csv(YFT_PSLS_catage, 'YFT_PSLS_catage.csv')

# Calculo juveniles y adultos
YFT_PSLS_catage_Juv_Adu <- YFT_PSLS_catage %>%
  mutate(Juv_Catch = rowSums(.[,2:11]),      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 12:28])) %>%
  select(Year, Juv_Catch, Adu_Catch)

# See the dataframe
YFT_PSLS_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_PSLS_catage_Juv_Adu, "YFT_PSLS_catage_Juv_Adu.csv", row.names = FALSE)

YFT_PSLS_catage_JA_perc <- data.frame(
  Year = YFT_PSLS_catage_Juv_Adu$Year,
  Juv_Catch = (YFT_PSLS_catage_Juv_Adu$Juv_Catch/ rowSums(YFT_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (YFT_PSLS_catage_Juv_Adu$Adu_Catch / rowSums(YFT_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(YFT_PSLS_catage_JA_perc, "YFT_PSLS_catage_JA_perc.csv", row.names = FALSE)
###############################################################  Plots  ###############################################################

############# Catch total
catage5 <- YFT_oth_catage_Juv_Adu
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
  ggtitle('Others juvenile and adult YFT catch in the model area [1950-2022]')+
  scale_y_continuous(labels = scales::label_number(scale = 1)) # convierte la notacion científica en numérica
#facet_grid(Stanza ~.) #Use if we want 5 different plots all together at once


############# Catch Percentage
catage5 <- YFT_oth_catage_JA_perc
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
  ggtitle('Others juvenile and adult YFT catch proportion in the model area [1950-2022]')+
  scale_y_continuous(labels = scales::label_number(scale = 1)) # convierte la notacion científica en numérica
#facet_grid(Stanza ~.) #Use if we want 5 different plots all together at once


###################################################################################################################################


############## SKIPJACK TUNA
# Set the working directory
setwd("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/7. Stock_assessment/1. Datasets with data on the model area as a unit (correct)/SKJ")

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

# from the column Beg.Mid, we have to select one value, B or M. B is the value at the beginning, and M is 
# the value at the middle. I selected M because the values of biomass are smaller, and therefore the future measures that 
# that could be taken from the results are more conservative. 

catage2 <- catage1 %>% subset(Yr>=1950 & Yr<=2022) #Cambiar el valor segun la especie BET:192-379; YFT:12-300
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

write.csv(catage2, 'SKJ_catch_at_age_by_gear.csv')
#catage2 <- read.csv('SKJ_catch_at_age_by_gear.csv', sep=',')
# Not use when calculating for BET or YFT
#catage2 <-catage2[c(1:292),]

# convert Yr column to a character column
class(catage2$Yr)
catage2$Yr <- as.numeric(catage2$Yr)
################################

###############################################################   LONGLINE  ######################################################

catage2LL <- catage2 %>% subset(Fleet=='LL')

# Aggregate by the quarter (Yr) 
catage2LL <- aggregate(. ~ Fleet + Yr, data=catage2LL, sum)

catage2LL[,1] <- NULL
#write.csv(catage2o, 'SKJ_Others_catage.csv')

# Calculo juveniles y adultos
catage2LL[,2] <- NULL
SKJ_LL_catage_Juv_Adu <- catage2LL %>%
  mutate(Juv_Catch = catage2LL[,2],      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

# See the dataframe
SKJ_LL_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_LL_catage_Juv_Adu, "SKJ_LL_catage_Juv_Adu.csv", row.names = FALSE)

SKJ_LL_catage_JA_perc <- data.frame(
  Yr = SKJ_LL_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_LL_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_LL_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_LL_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_LL_catage_JA_perc, "SKJ_LL_catage_JA_perc.csv", row.names = FALSE)

###############################################################   LINE  ######################################################

catage2LI <- catage2 %>% subset(Fleet=='LI')

# Aggregate by the quarter (Yr) 
catage2LI <- aggregate(. ~ Fleet + Yr, data=catage2LI, sum)

catage2LI[,1] <- NULL
#write.csv(catage2o, 'SKJ_Others_catage.csv')

# Calculo juveniles y adultos
catage2LI[,2] <- NULL
SKJ_LI_catage_Juv_Adu <- catage2LI %>%
  mutate(Juv_Catch = catage2LI[,2],      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

# See the dataframe
SKJ_LI_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_LI_catage_Juv_Adu, "SKJ_LI_catage_Juv_Adu.csv", row.names = FALSE)

SKJ_LI_catage_JA_perc <- data.frame(
  Yr = SKJ_LI_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_LI_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_LI_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_LI_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_LI_catage_JA_perc, "SKJ_LI_catage_JA_perc.csv", row.names = FALSE)

###############################################################   GILLNET  ######################################################

catage2GN <- catage2 %>% subset(Fleet=='GN')

# Aggregate by the quarter (Yr) 
catage2GN <- aggregate(. ~ Fleet + Yr, data=catage2GN, sum)

catage2GN[,1] <- NULL
#write.csv(catage2o, 'SKJ_Others_catage.csv')

# Calculo juveniles y adultos
catage2GN[,2] <- NULL
SKJ_GN_catage_Juv_Adu <- catage2GN %>%
  mutate(Juv_Catch = catage2GN[,2],      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

# See the dataframe
SKJ_GN_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_GN_catage_Juv_Adu, "SKJ_GN_catage_Juv_Adu.csv", row.names = FALSE)

SKJ_GN_catage_JA_perc <- data.frame(
  Yr = SKJ_GN_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_GN_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_GN_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_GN_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_GN_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_GN_catage_JA_perc, "SKJ_GN_catage_JA_perc.csv", row.names = FALSE)

###############################################################   PSFS  ######################################################

catage2PSFS <- catage2 %>% subset(Fleet=='PSFS')

# Aggregate by the quarter (Yr) 
catage2PSFS <- aggregate(. ~ Fleet + Yr, data=catage2PSFS, sum)

catage2PSFS[,1] <- NULL
#write.csv(catage2o, 'SKJ_Others_catage.csv')

# Calculo juveniles y adultos
catage2PSFS[,2] <- NULL
SKJ_PSFS_catage_Juv_Adu <- catage2PSFS %>%
  mutate(Juv_Catch = catage2PSFS[,2],      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

# See the dataframe
SKJ_PSFS_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_PSFS_catage_Juv_Adu, "SKJ_PSFS_catage_Juv_Adu.csv", row.names = FALSE)

SKJ_PSFS_catage_JA_perc <- data.frame(
  Yr = SKJ_PSFS_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_PSFS_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_PSFS_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_PSFS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_PSFS_catage_JA_perc, "SKJ_PSFS_catage_JA_perc.csv", row.names = FALSE)
###############################################################   PSLS  ######################################################

catage2PSLS <- catage2 %>% subset(Fleet=='PSLS')

# Aggregate by the quarter (Yr) 
catage2PSLS <- aggregate(. ~ Fleet + Yr, data=catage2PSLS, sum)

catage2PSLS[,1] <- NULL
#write.csv(catage2o, 'SKJ_Others_catage.csv')

# Calculo juveniles y adultos
catage2PSLS[,2] <- NULL
SKJ_PSLS_catage_Juv_Adu <- catage2PSLS %>%
  mutate(Juv_Catch = catage2PSLS[,2],      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

# See the dataframe
SKJ_PSLS_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_PSLS_catage_Juv_Adu, "SKJ_PSLS_catage_Juv_Adu.csv", row.names = FALSE)

SKJ_PSLS_catage_JA_perc <- data.frame(
  Yr = SKJ_PSLS_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_PSLS_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_PSLS_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_PSLS_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_PSLS_catage_JA_perc, "SKJ_PSLS_catage_JA_perc.csv", row.names = FALSE)


###############################################################   Others  ######################################################

catage2o <- catage2 %>% subset(Fleet=='Others')

# Aggregate by the quarter (Yr) 
catage2o <- aggregate(. ~ Fleet + Yr, data=catage2o, sum)

catage2o[,1] <- NULL
#write.csv(catage2o, 'SKJ_Others_catage.csv')

# Calculo juveniles y adultos
catage2o[,2] <- NULL
SKJ_Others_catage_Juv_Adu <- catage2o %>%
  mutate(Juv_Catch = catage2o[,2],      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

# See the dataframe
SKJ_Others_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_Others_catage_Juv_Adu, "SKJ_Others_catage_Juv_Adu.csv", row.names = FALSE)

SKJ_Others_catage_JA_perc <- data.frame(
  Yr = SKJ_Others_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_Others_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_Others_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_Others_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_Others_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_Others_catage_JA_perc, "SKJ_Others_catage_JA_perc.csv", row.names = FALSE)


###############################################################   Baitboat  ######################################################

catage2o <- catage2 %>% subset(Fleet=='BB')

# Aggregate by the quarter (Yr) 
catage2o <- aggregate(. ~ Fleet + Yr, data=catage2o, sum)

catage2o[,1] <- NULL
#write.csv(catage2o, 'SKJ_Others_catage.csv')

# Calculo juveniles y adultos
catage2o[,2] <- NULL
SKJ_BB_catage_Juv_Adu <- catage2o %>%
  mutate(Juv_Catch = catage2o[,2],      # Cambiar valores segun la especie. BET:2:18-19:42; YFT:2:11-12:28; SKJ: 2 - 3:9
         Adu_Catch = rowSums(.[, 3:9])) %>%
  select(Yr, Juv_Catch, Adu_Catch)

# See the dataframe
SKJ_BB_catage_Juv_Adu

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_BB_catage_Juv_Adu, "SKJ_BB_catage_Juv_Adu.csv", row.names = FALSE)

SKJ_BB_catage_JA_perc <- data.frame(
  Yr = SKJ_BB_catage_Juv_Adu$Yr,
  Juv_Catch = (SKJ_BB_catage_Juv_Adu$Juv_Catch/ rowSums(SKJ_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100,
  Adu_Catch = (SKJ_BB_catage_Juv_Adu$Adu_Catch / rowSums(SKJ_BB_catage_Juv_Adu[, c("Juv_Catch", "Adu_Catch")])) * 100
)

# Guardar el nuevo dataframe en un archivo CSV
write.csv(SKJ_BB_catage_JA_perc, "SKJ_BB_catage_JA_perc.csv", row.names = FALSE)

###############################################################  Plots  ###############################################################

############# Catch total
catage5 <- SKJ_LL_catage_Juv_Adu
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
  ggtitle('Longline fleets juvenile and adult SKJ catch in the Indian Ocean [1950-2022]')+
  scale_y_continuous(labels = scales::label_number(scale = 1)) # convierte la notacion científica en numérica
#facet_grid(Stanza ~.) #Use if we want 5 different plots all together at once


############# Catch Percentage
catage5 <- SKJ_LL_catage_JA_perc
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
  ggtitle('Longline fleets juvenile and adult SKJ catch proportion in the Indian Ocean [1950-2022]')+
  scale_y_continuous(labels = scales::label_number(scale = 1)) # convierte la notacion científica en numérica
#facet_grid(Stanza ~.) #Use if we want 5 different plots all together at once


###################################################################################################################################
