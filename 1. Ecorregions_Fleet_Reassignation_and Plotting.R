
#########################################################################################################################
###### This script opens the raw Rised catch database previously obtained from IOTC (.csv)                         ######
###### Opens the shapefile created with the ecoregions of our interest (.shp)                                      ######
###### Adds the ecoregion information into the Rised catch dataset based on the latitude and longitude parameters  ######
###### Identifys points outside the ecoregions which where assinged NAs                                            ######
###### Finds the closest ecoregion for this points and assignes it instead of NAs                                  ######
#########################################################################################################################

################################################################################
##### RESULT: Each register of a catch is assigned to an ecoregion based on the latitude and longitude #####
################################################################################

### Requiered packages
library("ggplot2")
library('dplyr')
library('tidyr')
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library('rgdal')
library("rgeos")
library("maps")
library(rgdal)
library(sf)
###

rm(list=ls())

# Set the working dierectory where the data of the rised catch is stored. The data must be in .csv format
setwd("...")

##################################################
#read the dataset
##################################################

datos<-read.csv("5sp_Rised_catch.csv", sep=",")
head(datos) # check the headers to know which variables there are
names(datos)
summary(datos) # get a summary of the data contained
class(datos)# see how it is a data.frame

##################################################
#read shapefile
##################################################

# Set the working dierectory where the shapefile of the ecoregions is stored. It must be in .shp format.
setwd("...")

ecoregions<-readOGR("IOEcorregions.shp", layer="IOEcorregions")
class(ecoregions) # see how it is a "SpatialPolygonsDataFrame"

### create a basemap of the world for future maps
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Set the working dierectory where the data of the rised catch is stored or to a new one, where the newly generated datasets will be stored.
setwd("...")

##################################################
#Transform fishing ground code into latitude and longitude
##################################################

# Split grid column into: size, quadrant, latitude and longitude
# In Grid size: 6=5ºx5º
# In hemisphere: 1=N and 2=S
datos <- datos %>% separate(col = FISHING_GROUND_CODE, sep = c(1, 2, 4), 
                            into = c("Grid_size", "hemisphere", "latitude", "longitude"))
head(datos)
str(datos) 
# We have to convert the new columns from chracter to numeric 
datos$hemisphere <- as.numeric(datos$hemisphere)
datos$latitude <- as.numeric(datos$latitude)
datos$longitude <- as.numeric(datos$longitude)
str(datos) 
summary(datos)

# Add a negative sign in front of latitudes if their hemisphere=2(SH)
datos$latitude <- ifelse(datos$hemisphere==2, -datos$latitude, datos$latitude)

# Transform the latitude and longitude so they represent the center of the 
# square and not the borders of it.
# In the northern hemisphere we have to add 2.5, and in the southern hemisphere we have to 
# add -2.5
datos$latitude <- ifelse(datos$hemisphere==2, -2.5+datos$latitude, 2.5+datos$latitude)

# 3rd, correct the value of the longitude to be in the middle of the square.
# As all our values are in the Eastern hemisphere, we only need to add 0.5
# to all the values

datos$longitude <- 2.5+datos$longitude

# Trim white spaces
datos$FLEET_CODE <- trimws(datos$FLEET_CODE)



##################################################
# Visualizing which points fall over land
##################################################

ggplot(data = world) + geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", 
                                    fill = NA) +geom_point(data=datos,aes(x=longitude, y=latitude)) 
# notice how some points are OUTSIDE the ecoregions OVER land. when we merge the shapefile with the Rised catches 
# points, this points over land will be assigned a NA. There are ways to fix this. But so far, we are not fixing it.


##################################################
# Transforming and merging rised cath and the ecorregions
##################################################

### To merge the Rised Catch and Ecoregion_they need to be the same class, and have the same Coordinates System.

proj4string(ecoregions) #"+proj=longlat +datum=WGS84 +no_defs
proj4string(datos) # this is a dataframe, and below we convert it to a "SpatialPointsDataFrame"

datos$longitude2<-datos$longitude #just making a copy
datos$latitude2<-datos$latitude #just making a copy

coordinates(datos) <- ~longitude2 + latitude2 # converts DATOS into a "SpatialPointsDataFrame"
class(datos) # see how now it is "SpatialPointsDataFrame"
proj4string(datos) #still not coordiantes systems 

proj4string(datos)<-CRS("+proj=longlat +datum=WGS84 +no_defs") #this assigs the coordinate systems
proj4string(datos) 


datos<-spTransform(datos,CRS(proj4string(ecoregions)))

identical(proj4string(datos),proj4string(ecoregions)) # We want to see here TRUE


##################################################
# Find overlaping and adding extra column to Rised catch
##################################################

# Now we find the overlap between the points and poligons of the shapefiles so we can add the extra columns with
# ecoregion information into the Rised catch database 

# Here we do the spatial overlap between points and polygons
points_with_ecoregions <- over(datos, ecoregions)

class(points_with_ecoregions)
head(points_with_ecoregions) #we still need to add the data attributes from the ecoregion shapefiles to the datapoint file. 
datos$Ecoregion_name <- points_with_ecoregions$Ecoregion #important line
datos$Ecoregion_ID <- points_with_ecoregions$Region_ID #important line


datos$Ecoregion_name<-as.factor(datos$Ecoregion_name)
datos$Ecoregion_ID<-as.factor(datos$Ecoregion_ID)
class(datos)
summary(datos)
levels(datos$Ecoregion_name)

class(datos) # still it is a "SpatialPointsDataFrame"
dim(datos)

datos_df<-as.data.frame(datos) #convert back to data.frame before saving it to csv
head(datos_df)

######
#save catdis with the ecoregion info added at the end. Once you do this once, you dont´t need to do it anymore and just use the file "CATDIS_with_ecoregions.csv" for analysis
######
write.csv(datos_df,file="Rised_catch_5sp_with_ecoregions.csv") # SAVE FILE


### Yet there is a problem
### Plot the data - # see how there are some points are still OUTSIDE the ecoregions and OVER LAND. For those points the ecoregion assiged was a NA-we need to fix this.
ggplot(data = world) + 
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", fill = NA) +geom_point(data=datos_df,aes(x=longitude, y=latitude)) 


###################################################
### Code for reasigning points outside poligons (ecoregions) to the closest poligons (ecoregions)
#########################################################
#########################################################
#########################################################

#open the Rised catch with the ecoregion information that we added in the previous step
catches_ecoregion <-read.csv("Rised_catch_5sp_with_ecoregions.csv", header=TRUE, sep=",")

head(catches_ecoregion)

summary(catches_ecoregion)

#read and plot ecoregion poligons

ecoregions_shp="IOEcorregions.shp"
ecoregion<-readOGR(dsn = ecoregions_shp, stringsAsFactors = F)

#plot shapefile and points of catches
ggplot() + geom_polygon(data = ecoregion, aes(x = long, y = lat, group = group), colour = "grey", fill = NA) + geom_point(data=catches_ecoregion, aes(longitude,latitude))

##############################
##### reassign outside cells with NA values into ecoregions shapefile
########################################

# convert to data.frame>spatialpointsdataframe
coordinates(catches_ecoregion)<- c("longitude","latitude")

# assign same projection as Ecoregion shapefile
proj4string(catches_ecoregion) <- proj4string(ecoregion)


# removes the data points outside the IOTC grid
# if(remove_points==TRUE){
# find where catch points lie within the IOTC grid
inside.grid <- !is.na(over(catches_ecoregion, as(ecoregion, "SpatialPolygons"))) # Here we make
# a vector of logical values (true or false) of which points fall inside the ecorregions

summary(inside.grid)

#points inside ecoregion
head(catches_ecoregion)
inside<-as.data.frame(catches_ecoregion[inside.grid,]) # Here we create a df called inside with all the points that
# fall inside the ecorregions 

dim(inside)

#### points outside

outside<-catches_ecoregion[!inside.grid,]
dim(outside)


catch_coords<-as.data.frame(unique(outside@coords))
head(catch_coords)
dim(catch_coords) # this tells us how many points fall outside the IOTC grid

outside<-as.data.frame(outside)

library(rgeos)

coordinates(catch_coords)<-c('longitude','latitude')
proj4string(catch_coords)<-proj4string(ecoregion)

head(outside)

#this code finds the distance of a point to all the ecoregins, and picks the smallest distance, and then assigns the name of the ecoregions to the Columns Ecoregion_name, which before had a NA

#First we do it for the column ecoregion$Ecoregion
grid<-NULL
for (i in 1:length(catch_coords)) {
  grid[i] <- ecoregion$Ecoregion[which.min(gDistance(catch_coords[i,], ecoregion, byid=TRUE))]
  
  outside[which(outside$latitude==catch_coords@coords[i,2] & outside$longitude==catch_coords@coords[i,1]),'Ecoregion_name']<-grid[i]
}

warnings()#Second we do it for the column ecoregion$Region_ID 
grid<-NULL
for (i in 1:length(catch_coords)) {
  grid[i] <- ecoregion$Region_ID[which.min(gDistance(catch_coords[i,], ecoregion, byid=TRUE))]
  
  outside[which(outside$latitude==catch_coords@coords[i,2] & outside$longitude==catch_coords@coords[i,1]),'Ecoregion_ID']<-grid[i]
}

head(grid)
head(outside)
summary(outside)

ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, aes(longitude,latitude), size=3, col='red') + 
  geom_point(data=inside, aes(longitude,latitude), size=3,col="blue") +
  ggtitle("Inside and outside points")

### making sure the asigments have been done correct!
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, size=3, aes(longitude,latitude, col=Ecoregion_name)) + 
  geom_point(data=inside, size=3, aes(longitude,latitude,col=Ecoregion_name)) +
  ggtitle("Point assignation by ecorregion")


#now that we have assiged all the "outside" points to an ecoregion, then combine both, the inside points and outside points into a database again.
Rised_catch_5sp_catches_ecoregion_reasinged<-rbind(inside,outside)

dim(Rised_catch_5sp_catches_ecoregion_reasinged)
summary(Rised_catch_5sp_catches_ecoregion_reasinged)

Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_name<-as.factor(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_name)

Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_ID<-as.factor(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_ID)

levels(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_name)
levels(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_ID)

summary(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_name)
summary(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_ID)


#ready to be save. You just need to do this once. And then just use "CATDIS_with_ecoregions_outside_pointsreasigned.csv" for analysis.

write.csv(Rised_catch_5sp_catches_ecoregion_reasinged, "Rised_catch_5sp_catches_ecoregion_reasinged.csv")

##################################################################################################################


#########################################################
#########################################################
# Ploting catch composition (piecharts) and catch values (points)
#########################################################
#########################################################






# Select a subset of the dataframe with only the column that interests us 
# changing the name of the fleet of interest each time 
catch <-read.csv('Rised_catch_5sp_catches_ecoregion_reasinged.csv')

# Add a column classifying groups depending if the species is Tropical, temperate or subtropical billfishes
# Create an empty column named: SPECIES_GROUP
catch$SPECIES_GROUP <- 3

# Albacore = Temperate 
catch$SPECIES_GROUP <- ifelse(catch$SPECIES_CODE=='ALB', 'Temperate tunas', catch$SPECIES_GROUP)

# Bigeye = Tropical
catch$SPECIES_GROUP <- ifelse(catch$SPECIES_CODE=='BET', 'Tropical tunas', catch$SPECIES_GROUP)

# Skipjack = Tropical
catch$SPECIES_GROUP <- ifelse(catch$SPECIES_CODE=='SKJ', 'Tropical tunas', catch$SPECIES_GROUP)

# Yellowfin = Tropical
catch$SPECIES_GROUP <- ifelse(catch$SPECIES_CODE=='YFT', 'Tropical tunas', catch$SPECIES_GROUP)

# Swordfish = Subtropical Billfishes
catch$SPECIES_GROUP <- ifelse(catch$SPECIES_CODE=='SWO', 'Subtropical billfishes', catch$SPECIES_GROUP)


#check the dataframe to see what are the columns that interests us
head(catch)
str(catch)
summary(catch)

# We don't need the first two columns as they are only indexes, not containing information. We remove them
catch <- select(catch,-X.1 & -X)


# Create a new dataframe called "catches" with the columns that interests us:
catches <- catch%>%
  select(c(FLEET_CODE, FISHERY_CODE, latitude, longitude, CATCH, SPECIES_CODE, SPECIES_GROUP)) # Change the variables here

# Subset the data with the metier that interests us:
 catches<- catches%>%subset(FISHERY_CODE %in% c('PSS', 'PSFS', 'PSLS, RIN'))       # Change the metier code here

# For Purse seine: 'PSS', 'PSFS', 'PSLS, RIN'
# For Longline: 'ELL', 'FLL', 'LL', 'LLEX', 'LG', 'SLL'
# For Baitboat: 'BB'
# For Gillnet: 'GILL', 'GIOF', 'GL'
# For line: 'LLCO', 'HAND', 'TROL', 'SPOR'
# For Others: 'BS', 'CN', 'DSEI', 'FN', 'LIFT', 'TRAP', 'TRAW'



# As it appears (I don't know why) that at the end of each fleet code values there are a lot of white spaces
# I will delate these spaces with:
catches$FLEET_CODE <- trimws(catches$FLEET_CODE)

catches <- catches%>%subset(FLEET_CODE %in%c('EUESP')) # Change the flag code here for all the fleets that we want to select


# Non-European fleets: 'AUS', 'BHR', 'BLZ', 'CHN', 'COM', 'DJI', 'EUGBR', 'EUITA', 'EUMYT', 'EUPRT', 'EUREU',
                     #'GBRT', 'GIN', 'JOR', 'JPN', 'KEN', 'KOR', 'MDG', 'MOZ', 'MUS', 'MYS', 'NEICE', 'NEIFR', 
                     #'NEIPS', 'NEISU', 'OMN', 'PAK', 'PHL', 'QAT', 'SEN', 'THA', 'TMP', 'TZA', 'VUT', 'ZAF'


# In case we want to save the dataset as a .csv
# write.csv(catches, "9. Iranian_YFT_catches_[2000-2016].csv")

#-------------------------Averaging by nº of registers-------------------------#

# Sum values based on matching latitude and longitude to know the amount fished in each point
# Group by latitude and longitude and calculate the sum of catch
sum_catch <- aggregate(CATCH ~ latitude + longitude + SPECIES_CODE, data = catches, sum)

# Add a column of 17 which corresponds to the time span of the dataframe
sum_catch$years <- 17
# Calculate the count of lines aggregated by latitude and longitude
#count_lines <- aggregate(. ~ latitude + longitude + SPECIES_CODE, data = catches, length)
result <- sum_catch
# Calculate the average catch
result$average_catch <- result$CATCH / result$years

# Check our new dataset "result" to select the interesting columns
head(result)

# Keep the columns of the dataset that interests us
result <- result%>%
  select(c(latitude, longitude, SPECIES_CODE, average_catch))

# Display the result
head(result)

summary(result)

# Transform the SPECIES_CODE column into new columns, one for each species
result <- reshape(result, idvar = c("longitude", "latitude"), timevar = "SPECIES_CODE", direction = "wide")
colnames(result) <- gsub("VALUE.", "", colnames(result))

# Substitute NA values with 0
result[is.na(result)] <- 0

# Rename the columns to make them clearer
# In case the fleets that we are analyzing have not fished a species an "Error in 'rename'()" will appear
# Don't worry abou it and keep running the script, but remember which is/are the species that are not fished.
result <- rename(result, 'Bigeye tuna' = 'average_catch.BET')
result <- rename(result, 'Skipjack tuna' = 'average_catch.SKJ')
result <- rename(result, 'Yellowfin tuna' = 'average_catch.YFT')
result <- rename(result, 'Swordfish' = 'average_catch.SWO')
result <- rename(result, 'Albacore' = 'average_catch.ALB')

#result <- rename(result, 'Tropical tunas' = 'average_catch.Tropical tunas')
#result <- rename(result, 'Temperate tunas' = 'average_catch.Temperate tunas')
#result <- rename(result, 'Subtropical billfishes' = 'average_catch.Subtropical billfishes')
# Calculate the sum of Albacore, Bigeye, Skipjack, and Yellowfin columns and add it as a new column
result$total <- rowSums(result[, !(colnames(result) %in% c('latitude', 'longitude'))])
#"Frigate", "Bigeye", "Yellowfin", 'Kawakawa', 'Sailfish'

# Check our final "result" dataset before ploting
head(result)
#---------------------------Graphical representation---------------------------#


#install.packages('devtools')
#devtools::install_github("MikkoVihtakari/ggOceanMapsData")
#install.packages('Rtools')
#install.packages('sf')

library(ggOceanMapsData)
library(ggOceanMaps)
library(ggspatial)

# Load the required libraries
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(scatterpie)
# Get the map data
map <- map_data("world")

#-------------------------If we wont to add a shapefile------------------------#

shapefile <- st_read("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/6.Ecor_fleet_reassing/IOEcorregions.shp")  # Reading the shapefile
# in case we want to plot the shapefile of the Ecorregions in the plot we will need to activate line 461


# In case we have only one column of data, one species, we will need to add a 
# column of 0 so the geom_scatterpie can work properly
# result <- result %>% mutate(Species = 0)


#------------------------Piecharts with all the same size----------------------#

# Create a vector with all the names of the species that appear in the dataframe, all the ones that when running the
# lines from 4004 to 408 didn't gave error
plot_with_basemap <- ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = "grey55", color = "grey55") +
  geom_scatterpie(aes(longitude, latitude), data = result, cols = c('Yellowfin tuna', 'Skipjack tuna', 'Bigeye tuna', 'Albacore'), 
                  legend_name='Species', pie_scale=2.2) +
 # assign a colour for each species: I prefer yellow for yellowfin, orange for skipjack, red for bigeye, blue for swordfish, 
# and green for albacore
   scale_fill_manual(values = c('yellow', "orange", 'red', 'green'))+
  # geom_sf(data = shapefile, fill = NA, color = "black") +  # Add the shapefile as a new layer
  theme_bw()+
  ggtitle('EUESP purse seine catch composition in the IO [2000-2016]') + # Change the fleet in the title here
  xlab('Longitude (º)') +
  ylab('Latitude (º)') +
  coord_sf(xlim = c(10, 160), ylim = c(40, -60), expand = FALSE) +  # Specify appropriate limits for lat(ylim) and long(xlim)
  theme(plot.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(vjust = 0.5, size = 12),
        axis.title.y = element_text(vjust = 0.5, size = 12))

# Print the final plot and readjust the size of the pies in line 455
print(plot_with_basemap)



#--------------------Piecharts with size depending on the catch----------------#

plot_with_basemap <- ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = "grey55", color = "grey55") +
  geom_scatterpie(aes(longitude, latitude, r=total/1200), data = result, cols = c('Yellowfin tuna', 'Skipjack tuna', 'Bigeye tuna', 'Albacore'), 
                  legend_name='Species', pie_scale = 1) +
  scale_fill_manual(values = c('yellow', "orange", 'red', 'green'))+
  geom_scatterpie_legend(result$total/1200, x=130, y=20, n=4, labeller=function(x) 1200*x)+
  #geom_sf(data = shapefile, fill = NA, color = "black") +  # Add the shapefile as a new layer
  theme_bw()+
  ggtitle('Catches (t/year) by the EUESP purse seine fleets in the IO [2000-2016]') + # Change the fleet in the title here
  xlab('Longitude (º)') +
  ylab('Latitude (º)') +
  coord_sf(xlim = c(10, 160), ylim = c(40, -60), expand = FALSE) +  # Specify appropriate limits for lat(ylim) and long(xlim)
  theme(plot.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(vjust = 0.5, size = 12),
        axis.title.y = element_text(vjust = 0.5, size = 12))

# Print the final plot and adjust the size of the piecharts modifiyng the radius in line 482(r), 485(total/...), 
# and 485 (...*X) all these 3 numbers must have the same value
print(plot_with_basemap)



#--------------------Points with size and colour depending on the catch----------------#

plot_with_basemap <- ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = "grey55", color = "grey55") +
  geom_point(aes(longitude, latitude, size=total, colour=total), data = result)+ 
  scale_color_gradient(name = "Catch (t/y)", low = "lightskyblue", high = "blue4", guide = "none") +  # Reverse the color gradient
  scale_size_continuous(name = "Catch (t/y)", range = c(0, 20)) +  # Change the name of the points legend
  ggtitle('Catch by the EUESP purse seine fleet in the IO [2000-2016]') + # Change the fleet in the title here
  xlab('Longitude (º)') +
  ylab('Latitude (º)') +
  #theme_bw()+
  coord_sf(xlim = c(10, 160), ylim = c(40, -60), expand = FALSE) +  # Specify appropriate limits for lat(ylim) and long(xlim)
  theme(plot.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(vjust = 0.5, size = 12),
        axis.title.y = element_text(vjust = 0.5, size = 12))

# Print the final plot
print(plot_with_basemap)


