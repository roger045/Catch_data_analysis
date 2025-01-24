#########################################################################################################################
##### Part 1/2:                                                                                                     #####
##### This script, opens the selected Catch and Effort dataset, opens IOTC shapefile, add the ecoregion information #####
##### into the dataset and then identifys points outside the ecoregions which where assinged NAs, then finds the    #####
##### closest ecoregion for that points and substitutes the NA by the ecoregion name, and save the reassigned data  #####
##### Author: Roger Amate (AZTI)                                                                                    #####
##### Year: 2024                                                                                                    #####
#########################################################################################################################

library("ggplot2")
library('dplyr')
library('tidyr')
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library('rgdal')
library("rgeos")
library("maps")
library("zoo")
library("readr")
library("hrbrthemes")
theme_set(theme_bw())

###################################################
# Set the working directory
setwd("~/.../1. Data_and_files_2024")

##### Longline #####
# Open the dataset
datos_LL<-read.csv(file="IOTC-2024-WPB22-DATA04-CELongline.csv",header=TRUE, sep=",")
head(datos_LL)
names(datos_LL)
summary(datos_LL)
class(datos_LL) # See how it is a data.frame

# Select only the columns that interests us
datos_LL <- datos_LL[,c(1,2,3,7,8,9,13,15,17)]

# Fill NA values of catches with 0
datos_LL[is.na(datos_LL)] <- 0

# Read shapefile
setwd("~/...IOTC_fisheries_reassignation_layers")

ecoregions<-readOGR("Final_area_for_calculations.shp", layer="Final_area_for_calculations") # Don't worry about the warnings ()
class(ecoregions) # See how it is a "SpatialPolygonsDataFrame"

### Plot ecoregions and points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#Transform fishing ground code into latitude and longitude
setwd("~/.../1. Data_and_files_2024")
# Split grid column into: size, quadrant, latitude and longitude
# In hemisphere: 1=N and 2=S
datos <- datos_LL %>% separate(col = Grid, sep = c(1, 2, 4), 
                            into = c("Grid_size", "hemisphere", "latitude", "longitude"))
head(datos)
str(datos) 
# We have to convert the new columns from chracter to numeric 
datos$hemisphere <- as.numeric(datos$hemisphere)
datos$latitude <- as.numeric(datos$latitude)
datos$longitude <- as.numeric(datos$longitude)
str(datos) 
summary(datos)

##################################################
#Correct corners of latitude and longitude into the center of the square or rectangle
# Grid_sizes:
# 5 --> 1ºx1º
# 6 --> 5ºx5º
# 1 --> 5ºx10º
# 2 --> 10ºx20º
# 3 --> 10ºx10º
# 4 --> 20ºx20º
##################################################

# Add a negative sign in front of latitudes if their hemisphere=2(SH)
datos$latitude <- ifelse(datos$hemisphere==2, -datos$latitude, datos$latitude)

# Split the dataset into two datasets depending on the grid size to correct the catch coordinates
datos5gs <- subset(datos, datos$Grid_size==5)
datos6gs <- subset(datos, datos$Grid_size==6)
datos1gs <- subset(datos, datos$Grid_size==1)
datos2gs <- subset(datos, datos$Grid_size==2)
datos3gs <- subset(datos, datos$Grid_size==3)
datos4gs <- subset(datos, datos$Grid_size==4)
# Transform the latitude and longitude so they represent the center of the square and not the corner of it
# When Grid_size is 6 in the northern hemisphere we have to add 2.5, and in the southern hemisphere we have to 
# add -2.5, and other corresponding values for other grid-sizes
datos5gs$latitude <- ifelse((datos5gs$Grid_size==5 & datos5gs$hemisphere==2), -0.5+datos5gs$latitude, 0.5+datos5gs$latitude)
datos6gs$latitude <- ifelse((datos6gs$Grid_size==6 & datos6gs$hemisphere==2), -2.5+datos6gs$latitude, 2.5+datos6gs$latitude)
datos1gs$latitude <- ifelse((datos1gs$Grid_size==1 & datos1gs$hemisphere==2), -2.5+datos1gs$latitude, 2.5+datos1gs$latitude)
datos2gs$latitude <- ifelse((datos2gs$Grid_size==2 & datos2gs$hemisphere==2), -5+datos2gs$latitude, 5+datos2gs$latitude)
datos3gs$latitude <- ifelse((datos3gs$Grid_size==3 & datos3gs$hemisphere==2), -5+datos3gs$latitude, 5+datos3gs$latitude)
datos4gs$latitude <- ifelse((datos4gs$Grid_size==4 & datos4gs$hemisphere==2), -10+datos4gs$latitude, 10+datos4gs$latitude)

# 3rd, correct the value of the longitude to be in the middle of the square.
# As all our values are in the Eastern hemisphere, we only need to add the 
# positive value of half of the longitude size to all the values
datos5gs$longitude <- ifelse(datos5gs$Grid_size==5, 0.5+datos5gs$longitude, datos5gs$longitude)
datos6gs$longitude <- ifelse(datos6gs$Grid_size==6, 2.5+datos6gs$longitude, datos6gs$longitude)
datos1gs$longitude <- ifelse(datos1gs$Grid_size==1, 5+datos1gs$longitude, datos1gs$longitude)
datos2gs$longitude <- ifelse(datos2gs$Grid_size==2, 10+datos2gs$longitude, datos2gs$longitude)
datos3gs$longitude <- ifelse(datos3gs$Grid_size==3, 5+datos3gs$longitude, datos3gs$longitude)
datos4gs$longitude <- ifelse(datos4gs$Grid_size==4, 10+datos4gs$longitude, datos4gs$longitude)

# Here we merge the datasets, so we have the coordinates corrected all together
datos <- rbind(datos5gs, datos6gs, datos1gs, datos2gs, datos3gs, datos4gs)

##################################################
# Visualizing which points fall over land
##################################################

map1 <-ggplot(data = world) + geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", 
                                           fill = NA) +geom_point(data=datos,aes(x=longitude, y=latitude)) 
print(map1)
# Notice how some points are OUTSIDE the ecoregions OVER land. when we merge the shapefile with the Catch & Effort catches 
# Points, this points over land will be assigned a NA. There are ways to fix this. But so far, we are not fixing it.

##################################################
# Transforming and merging Catch & Effort and the ecorregions
##################################################

### To merge the Catch & Effort and Ecoregion_they need to be the same class, and have the same Coordinates System.

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
# Find overlaping and adding extra column to the catch
##################################################

# Now we find the overlap between the points and poligons of the shapefiles so we can add the extra columns with
# ecoregion information into the catch database 

# Here we do the spatial overlap between points and polygons
points_with_ecoregions <- over(datos, ecoregions)

class(points_with_ecoregions)
head(points_with_ecoregions) # We still need to add the data attributes from the ecoregion shapefiles to the datapoint file. 
datos$Ecoregion_name <- points_with_ecoregions$Ecoregion # Important line
datos$Ecoregion_ID <- points_with_ecoregions$Region_ID # Important line

datos$Ecoregion_name<-as.factor(datos$Ecoregion_name)
datos$Ecoregion_ID<-as.factor(datos$Ecoregion_ID) # It doesn't matter if here gives an error like: '[[...
class(datos)
summary(datos)
levels(datos$Ecoregion_name)

class(datos) # Still it is a "SpatialPointsDataFrame"
dim(datos)

datos_df<-as.data.frame(datos) # Convert back to data.frame before saving it to .csv
head(datos_df)

######
# Save effort data with the ecoregion info added at the end. Once you do this once, you dont´t need to do it 
# anymore and just use the file "Catch_&_Effort_LL_with_ecoregions_(no-reassigned).csv" for analysis
######
write.csv(datos_df,file="Catch_&_Effort_LL_with_ecoregions_(no-reassigned).csv") # SAVE FILE

### Yet there is a problem
### Plot the data  to see how there are some points are still OUTSIDE the ecoregions and OVER LAND. For those points the ecoregion assiged was a NA-we need to fix this.
map2 <- ggplot(data = world) + 
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", fill = NA) +geom_point(data=datos_df,aes(x=longitude, y=latitude)) 
print(map2)

###################################################
### Code for reasigning points outside poligons (ecoregions) to the closest poligons (ecoregions)
#########################################################

# Open the Catch & Effort with the ecoregion information that we added in the previous step
catches_ecoregion <-read.csv("Catch_&_Effort_LL_with_ecoregions_(no-reassigned).csv", header=TRUE, sep=",")
head(catches_ecoregion)
summary(catches_ecoregion)

# Read and plot ecoregion poligons
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")
ecoregions_shp="Final_area_for_calculations.shp"
ecoregion<-readOGR(dsn = ecoregions_shp, stringsAsFactors = F) # Don't worry about the warnings ()

# Change the working directory to the original one
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")

# Plot shapefile and points of catches
ggplot() + geom_polygon(data = ecoregion, aes(x = long, y = lat, group = group), colour = "grey", fill = NA) + geom_point(data=catches_ecoregion, aes(longitude,latitude))

##############################
##### Reassign outside cells with NA values into ecoregions shapefile
########################################

# Convert to data.frame>spatialpointsdataframe
coordinates(catches_ecoregion)<- c("longitude","latitude")

# Assign same projection as the Ecoregions shapefile
proj4string(catches_ecoregion) <- proj4string(ecoregion)

# Remove the data points outside the IOTC grid
# Make a vector of logical values (true or false) of which points fall inside the ecorregions
inside.grid <- !is.na(over(catches_ecoregion, as(ecoregion, "SpatialPolygons"))) 
summary(inside.grid)

# Points inside ecoregion
head(catches_ecoregion)
# Here we create a df called inside with all the points that fall inside the ecorregions
inside<-as.data.frame(catches_ecoregion[inside.grid,])  
dim(inside)

#### Points outside
outside<-catches_ecoregion[!inside.grid,]
dim(outside)
catch_coords<-as.data.frame(unique(outside@coords))
head(catch_coords)
dim(catch_coords) # This tells us how many points fall outside the IOTC grid

outside<-as.data.frame(outside)
library(rgeos)
coordinates(catch_coords)<-c('longitude','latitude')
proj4string(catch_coords)<-proj4string(ecoregion)
head(outside)

# This chunk of code finds the distance of a point to all the ecoregins, and picks the smallest distance, 
# and then assigns the name of the closest ecoregions to the Columns Ecoregion_name, which before had a NA
#First we do it for the column ecoregion$Ecoregion
grid<-NULL
for (i in 1:length(catch_coords)) {
  grid[i] <- ecoregion$Ecoregion[which.min(gDistance(catch_coords[i,], ecoregion, byid=TRUE))]
  outside[which(outside$latitude==catch_coords@coords[i,2] & outside$longitude==catch_coords@coords[i,1]),'Ecoregion_name']<-grid[i]
}
warnings() # Don't warry about the warnings()

#Second we do it for the column ecoregion$Region_ID 
grid<-NULL
for (i in 1:length(catch_coords)) {
  grid[i] <- ecoregion$Region_ID[which.min(gDistance(catch_coords[i,], ecoregion, byid=TRUE))]
  outside[which(outside$latitude==catch_coords@coords[i,2] & outside$longitude==catch_coords@coords[i,1]),'Ecoregion_ID']<-grid[i]
} # Don't warry about the warnings() or an Error: in `*tmp*`...

head(grid)
head(outside)
summary(outside)

# Plot a map to see if the identification of points that fall on land has been done correctly
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, aes(longitude,latitude), size=3, col='red') + 
  geom_point(data=inside, aes(longitude,latitude), size=3,col="blue") +
  ggtitle("Inside and outside points")

### Making sure the assigment to the ecoregions has been done correctly!
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, size=3, aes(longitude,latitude, col=Ecoregion_name)) + 
  geom_point(data=inside, size=3, aes(longitude,latitude,col=Ecoregion_name)) +
  ggtitle("Point assignation by ecorregion")

# Now that we have assiged all the "outside" points to an ecoregion, then combine both, the inside points and outside points into a database again.
CE_LL_catches_ecoregion_reasigned<-rbind(inside,outside)

dim(CE_LL_catches_ecoregion_reasigned)
summary(CE_LL_catches_ecoregion_reasigned)

CE_LL_catches_ecoregion_reasigned$Ecoregion_name<-as.factor(CE_LL_catches_ecoregion_reasigned$Ecoregion_name)

CE_LL_catches_ecoregion_reasigned$Ecoregion_ID<-as.factor(CE_LL_catches_ecoregion_reasigned$Ecoregion_ID)

levels(CE_LL_catches_ecoregion_reasigned$Ecoregion_name)
levels(CE_LL_catches_ecoregion_reasigned$Ecoregion_ID)

summary(CE_LL_catches_ecoregion_reasigned$Ecoregion_name)
summary(CE_LL_catches_ecoregion_reasigned$Ecoregion_ID)

CE_LL_catches_ecoregion_reasigned[,16] <- NULL
CE_LL_catches_ecoregion_reasigned[,15] <- NULL
CE_LL_catches_ecoregion_reasigned[,1] <- NULL

# Ready to be save. You just need to do this once. And then just use "CE_LL_catches_ecoregion_reasigned.csv" for analysis.
write.csv(CE_LL_catches_ecoregion_reasigned, "CE_LL_catches_ecoregion_reasigned.csv")
#####################################################################################################################


# Set the Working directory
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")

##################################################
### Purse Seine and BB
##################################################

# Open the dataset
datos_PSBB<-read.csv(file="IOTC-2024-WPB22-DATA05-CESurface.csv",header=TRUE, sep=",")
head(datos_PSBB)
names(datos_PSBB)
summary(datos_PSBB)
class(datos_PSBB) # See how it is a data.frame

# Select only the columns that interests us
datos_PSBB <- datos_PSBB[,c(1,2,3,7,8,9,12,13,14,15,16,17,18,19,20,21)]

# Fill NA values of catches with 0
datos_PSBB[is.na(datos_PSBB)] <- 0

##################################################
# Read shapefile
##################################################

setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")

ecoregions<-readOGR("Final_area_for_calculations.shp", layer="Final_area_for_calculations") # Don't worry about the warnings()
class(ecoregions) # See how it is a "SpatialPolygonsDataFrame"

### Plot ecoregions and points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

##################################################
# Transform fishing ground code into latitude and longitude
##################################################
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
# Split grid column into: size, quadrant, latitude and longitude
# In hemisphere: 1=N and 2=S
datos <- datos_PSBB %>% separate(col = Grid, sep = c(1, 2, 4), 
                               into = c("Grid_size", "hemisphere", "latitude", "longitude"))
unique(datos$Grid_size)
head(datos)
str(datos) 
# We have to convert the new columns from character to numeric 
datos$hemisphere <- as.numeric(datos$hemisphere)
datos$latitude <- as.numeric(datos$latitude)
datos$longitude <- as.numeric(datos$longitude)
str(datos) 
summary(datos)

##################################################
#Correct corners of latitude and longitude into the center of the square or rectangle
# Grid_sizes:
# 5 --> 1ºx1º
# 6 --> 5ºx5º
# 1 --> 5ºx10º
# 2 --> 10ºx20º
# 3 --> 10ºx10º
# 4 --> 20ºx20º
##################################################

# Add a negative sign in front of latitudes if their hemisphere=2(SH)
datos$latitude <- ifelse(datos$hemisphere==2, -datos$latitude, datos$latitude)

# Split the dataset into two datasets depending on the grid size to correct the catch coordinates
datos5gs <- subset(datos, datos$Grid_size==5)
datos6gs <- subset(datos, datos$Grid_size==6)

# Transform the latitude and longitude so they represent the center of the 
# square and not the borders of it.
# When Grid_size is 6 in the northern hemisphere we have to add 2.5, and in the southern hemisphere we have to 
# add -2.5, and other corresponding values for other grid-sizes
datos5gs$latitude <- ifelse((datos5gs$Grid_size==5 & datos5gs$hemisphere==2), -0.5+datos5gs$latitude, 0.5+datos5gs$latitude)
datos6gs$latitude <- ifelse((datos6gs$Grid_size==6 & datos6gs$hemisphere==2), -2.5+datos6gs$latitude, 2.5+datos6gs$latitude)

# 3rd, correct the value of the longitude to be in the middle of the square.
# As all our values are in the Eastern hemisphere, we only need to add the 
# positive value of half of the longitude size to all the values
datos5gs$longitude <- ifelse(datos5gs$Grid_size==5, 0.5+datos5gs$longitude, datos5gs$longitude)
datos6gs$longitude <- ifelse(datos6gs$Grid_size==6, 2.5+datos6gs$longitude, datos6gs$longitude)

# Here we merge the datasets, so we have the coordinates corrected all together
datos<-rbind(datos5gs,datos6gs)
##################################################
# Visualizing which points fall over land
##################################################

map1 <-ggplot(data = world) + geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", 
                                           fill = NA) +geom_point(data=datos,aes(x=longitude, y=latitude)) 
print(map1)
# notice how some points are OUTSIDE the ecoregions OVER land. when we merge the shapefile with the Catch & Effort 
# points, this points over land will be assigned a NA. There are ways to fix this. But so far, we are not fixing it.

##################################################
# Transforming and merging rised cath and the ecorregions
##################################################

### To merge the Catch & Effort and Ecoregion they need to be the same class, and have the same Coordinates System.
proj4string(ecoregions) #"+proj=longlat +datum=WGS84 +no_defs 
proj4string(datos) # This is a dataframe, and below we convert it to a "SpatialPointsDataFrame"

datos$longitude2<-datos$longitude # Just making a copy
datos$latitude2<-datos$latitude # Just making a copy

coordinates(datos) <- ~longitude2 + latitude2 # Converts DATOS into a "SpatialPointsDataFrame"
class(datos) # Cee how now it is "SpatialPointsDataFrame"
proj4string(datos) # Still not coordiantes systems 

proj4string(datos)<-CRS("+proj=longlat +datum=WGS84 +no_defs") # This assigs the coordinate systems
proj4string(datos) 

datos<-spTransform(datos,CRS(proj4string(ecoregions)))

identical(proj4string(datos),proj4string(ecoregions)) # We want to see here TRUE

##################################################
# Find overlaping and adding extra column to the catch
##################################################
# Now we find the overlap between the points and poligons of the shapefiles so we can 
# add the extra columns with ecoregion information into the catch database 
# Here we do the spatial overlap between points and polygons
points_with_ecoregions <- over(datos, ecoregions)
class(points_with_ecoregions)
head(points_with_ecoregions) # We still need to add the data attributes from the ecoregion shapefiles to the datapoint file. 
datos$Ecoregion_name <- points_with_ecoregions$Ecoregion # Important line
datos$Ecoregion_ID <- points_with_ecoregions$Region_ID # Important line


datos$Ecoregion_name<-as.factor(datos$Ecoregion_name)
datos$Ecoregion_ID<-as.factor(datos$Ecoregion_ID) # Don't worry about the error: '[[...
class(datos)
summary(datos)
levels(datos$Ecoregion_name)

class(datos) # Still it is a "SpatialPointsDataFrame"
dim(datos)

datos_df<-as.data.frame(datos) # Convert back to data.frame before saving it to .csv
head(datos_df)
######
# Save data with the ecoregion info added at the end. Once you do this once, you dont´t need to do it anymore 
# and just use the file "Catch_&_Effort_PSBB_with_ecoregions_(no-reassigned).csv" for analysis
######
write.csv(datos_df,file="Catch_&_Effort_PSBB_with_ecoregions_(no-reassigned).csv") # SAVE FILE

### Yet there is a problem
### Plot the data. See how there are some points are still OUTSIDE the ecoregions and OVER LAND. For those points the ecoregion assiged was a NA-we need to fix this.
map2 <- ggplot(data = world) + 
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", fill = NA) +geom_point(data=datos_df,aes(x=longitude, y=latitude)) 

print(map2)

###################################################
### Code for reasigning points outside poligons (ecoregions) to the closest poligons (ecoregions)
#########################################################
# Open the Catch & Effort with the ecoregion information that we added in the previous step
catches_ecoregion <-read.csv("Catch_&_Effort_PSBB_with_ecoregions_(no-reassigned).csv", header=TRUE, sep=",")
head(catches_ecoregion)
summary(catches_ecoregion)

# Read and plot ecoregion poligons
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")
ecoregions_shp="Final_area_for_calculations.shp"
ecoregion<-readOGR(dsn = ecoregions_shp, stringsAsFactors = F) # Don't worry about the warnings()

# Set the working directory to the first one
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
# Plot shapefile and points of catches
ggplot() + geom_polygon(data = ecoregion, aes(x = long, y = lat, group = group), colour = "grey", fill = NA) + geom_point(data=catches_ecoregion, aes(longitude,latitude))

##############################
##### reassign outside cells with NA values into ecoregions shapefile
########################################

# convert to data.frame>spatialpointsdataframe
coordinates(catches_ecoregion)<- c("longitude","latitude")

# Assign same projection as Ecoregion shapefile
proj4string(catches_ecoregion) <- proj4string(ecoregion)

# Removes the data points outside the IOTC grid
# Make a vector of logical values (true or false) of which points fall inside the ecorregion
inside.grid <- !is.na(over(catches_ecoregion, as(ecoregion, "SpatialPolygons"))) # Make a vector of logical values (true or false) of which points fall inside the ecorregions
summary(inside.grid)

# Points inside ecoregion
head(catches_ecoregion)

# Here we create a df called inside with all the points that all inside the ecorregions
inside<-as.data.frame(catches_ecoregion[inside.grid,])  
dim(inside)

#### Points outside
outside<-catches_ecoregion[!inside.grid,]
dim(outside)
catch_coords<-as.data.frame(unique(outside@coords))
head(catch_coords)
dim(catch_coords) # This tells us how many points fall outside the IOTC grid
outside<-as.data.frame(outside)

library(rgeos)

coordinates(catch_coords)<-c('longitude','latitude')
proj4string(catch_coords)<-proj4string(ecoregion)
head(outside)

# This code finds the distance of a point to all the ecoregins, and picks the smallest distance, and then assigns the name of the ecoregions to the Columns Ecoregion_name, which before had a NA
#First we do it for the column ecoregion$Ecoregion
grid<-NULL
for (i in 1:length(catch_coords)) {
  grid[i] <- ecoregion$Ecoregion[which.min(gDistance(catch_coords[i,], ecoregion, byid=TRUE))]
  outside[which(outside$latitude==catch_coords@coords[i,2] & outside$longitude==catch_coords@coords[i,1]),'Ecoregion_name']<-grid[i]
} # Don't worry about the error

warnings() # Second we do it for the column ecoregion$Region_ID 
grid<-NULL
for (i in 1:length(catch_coords)) {
  grid[i] <- ecoregion$Region_ID[which.min(gDistance(catch_coords[i,], ecoregion, byid=TRUE))]
  outside[which(outside$latitude==catch_coords@coords[i,2] & outside$longitude==catch_coords@coords[i,1]),'Ecoregion_ID']<-grid[i]
} # Don't worry about the error

head(grid)
head(outside)
summary(outside)

# Plot a map to see if the identification of points that fall on land has been done correctly
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, aes(longitude,latitude), size=3, col='red') + 
  geom_point(data=inside, aes(longitude,latitude), size=3,col="blue") +
  ggtitle("Inside and outside points")

### Making sure the asigment of the ecoregions has been done correctly!
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, size=3, aes(longitude,latitude, col=Ecoregion_name)) + 
  geom_point(data=inside, size=3, aes(longitude,latitude,col=Ecoregion_name)) +
  ggtitle("Point assignation by ecorregion")

# Now that we have assiged all the "outside" points to an ecoregion, then combine both, the inside points and outside points into a database again.
CE_PSBB_catches_ecoregion_reasigned<-rbind(inside,outside)

dim(CE_PSBB_catches_ecoregion_reasigned)
summary(CE_PSBB_catches_ecoregion_reasigned)

CE_PSBB_catches_ecoregion_reasigned$Ecoregion_name<-as.factor(CE_PSBB_catches_ecoregion_reasigned$Ecoregion_name)

CE_PSBB_catches_ecoregion_reasigned$Ecoregion_ID<-as.factor(CE_PSBB_catches_ecoregion_reasigned$Ecoregion_ID)

levels(CE_PSBB_catches_ecoregion_reasigned$Ecoregion_name)
levels(CE_PSBB_catches_ecoregion_reasigned$Ecoregion_ID)

summary(CE_PSBB_catches_ecoregion_reasigned$Ecoregion_name)
summary(CE_PSBB_catches_ecoregion_reasigned$Ecoregion_ID)

CE_PSBB_catches_ecoregion_reasigned[,23] <- NULL
CE_PSBB_catches_ecoregion_reasigned[,22] <- NULL
CE_PSBB_catches_ecoregion_reasigned[,1] <- NULL

# Ready to be save. You just need to do this once. And then just use "2024_CE_PSBB_catches_ecoregion_reasigned.csv" for analysis.
write.csv(CE_PSBB_catches_ecoregion_reasigned, "2024_CE_PSBB_catches_ecoregion_reasigned.csv")
#####################################################################################################################
#####################################################################################################################

# Set the WD
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")

##################################################
### Others (GN, LI, Oth)
##################################################

# Open the dataset
datos_OT<-read.csv(file="IOTC-2024-WPB22-DATA06-CEOther.csv",header=TRUE, sep=",")
head(datos_OT)
names(datos_OT)
summary(datos_OT)
class(datos_OT) # See how it is a data.frame

# Select only the columns that interests us
datos_OT <- datos_OT[,c(1,2,3,7,8,9,13,15,17)]

# Fill NA values of catches with 0
datos_OT[is.na(datos_OT)] <- 0

##################################################
# Read shapefile
##################################################
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")

ecoregions<-readOGR("Final_area_for_calculations.shp", layer="Final_area_for_calculations") # Don't worry about the warnings()
class(ecoregions) # See how it is a "SpatialPolygonsDataFrame"

### Plot ecoregions and points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

##################################################
#Transform fishing ground code into latitude and longitude
##################################################
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
# Split grid column into: size, quadrant, latitude and longitude
# In hemisphere: 1=N and 2=S
datos <- datos_OT %>% separate(col = Grid, sep = c(1, 2, 4), 
                               into = c("Grid_size", "hemisphere", "latitude", "longitude"))
head(datos)
str(datos) 

# We have to convert the new columns from chracter to numeric 
datos$hemisphere <- as.numeric(datos$hemisphere)
datos$latitude <- as.numeric(datos$latitude)
datos$longitude <- as.numeric(datos$longitude)
str(datos) 
summary(datos)

##################################################
#Correct corners of latitude and longitude into the center of the square or rectangle
# Grid_sizes:
# 5 --> 1ºx1º
# 6 --> 5ºx5º
# 1 --> 5ºx10º
# 2 --> 10ºx20º
# 3 --> 10ºx10º
# 4 --> 20ºx20º
##################################################
# Check Grid sizes
unique(datos$Grid_size)

# Add a negative sign in front of latitudes if their hemisphere=2(SH)
datos$latitude <- ifelse(datos$hemisphere==2, -datos$latitude, datos$latitude)

# Split the dataset into two datasets depending on the grid size to correct the catch coordinates
datos5gs <- subset(datos, datos$Grid_size==5)
datos6gs <- subset(datos, datos$Grid_size==6)
datos1gs <- subset(datos, datos$Grid_size==1)
datos2gs <- subset(datos, datos$Grid_size==2)
datos3gs <- subset(datos, datos$Grid_size==3)

# Transform the latitude and longitude so they represent the center of the 
# square and not the borders of it.
# When Grid_size is 6 in the northern hemisphere we have to add 2.5, and in the southern hemisphere we have to 
# add -2.5, and other corresponding values for other grid-sizes
datos5gs$latitude <- ifelse((datos5gs$Grid_size==5 & datos5gs$hemisphere==2), -0.5+datos5gs$latitude, 0.5+datos5gs$latitude)
datos6gs$latitude <- ifelse((datos6gs$Grid_size==6 & datos6gs$hemisphere==2), -2.5+datos6gs$latitude, 2.5+datos6gs$latitude)
datos1gs$latitude <- ifelse((datos1gs$Grid_size==1 & datos1gs$hemisphere==2), -2.5+datos1gs$latitude, 2.5+datos1gs$latitude)
datos2gs$latitude <- ifelse((datos2gs$Grid_size==2 & datos2gs$hemisphere==2), -5+datos2gs$latitude, 5+datos2gs$latitude)
datos3gs$latitude <- ifelse((datos3gs$Grid_size==3 & datos3gs$hemisphere==2), -5+datos3gs$latitude, 5+datos3gs$latitude)

# 3rd, correct the value of the longitude to be in the middle of the square.
# As all our values are in the Eastern hemisphere, we only need to add the 
# positive value of half of the longitude size to all the values
datos5gs$longitude <- ifelse(datos5gs$Grid_size==5, 0.5+datos5gs$longitude, datos5gs$longitude)
datos6gs$longitude <- ifelse(datos6gs$Grid_size==6, 2.5+datos6gs$longitude, datos6gs$longitude)
datos1gs$longitude <- ifelse(datos1gs$Grid_size==1, 5+datos1gs$longitude, datos1gs$longitude)
datos2gs$longitude <- ifelse(datos2gs$Grid_size==2, 10+datos2gs$longitude, datos2gs$longitude)
datos3gs$longitude <- ifelse(datos3gs$Grid_size==3, 5+datos3gs$longitude, datos3gs$longitude)

# Here we merge the datasets, so we have the coordinates corrected all together
datos <- rbind(datos5gs, datos6gs, datos1gs, datos2gs, datos3gs)

##################################################
# Visualizing which points fall over land
##################################################

map1 <-ggplot(data = world) + geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", 
                                           fill = NA) +geom_point(data=datos,aes(x=longitude, y=latitude)) 
print(map1)
# Notice how some points are OUTSIDE the ecoregions OVER LAND. When we merge the shapefile with the Catch & Effort 
# points, this points over land will be assigned a NA. There are ways to fix this. But so far, we are not fixing it.


##################################################
# Transforming and merging effort data and the ecorregions
##################################################

### To merge the Catch & Effort data and Ecoregion they need to be the same class, and have the same Coordinates System.
proj4string(ecoregions) #"+proj=longlat +datum=WGS84 +no_defs
proj4string(datos) # this is a dataframe, and below we convert it to a "SpatialPointsDataFrame"

datos$longitude2<-datos$longitude # Just making a copy
datos$latitude2<-datos$latitude # Just making a copy

coordinates(datos) <- ~longitude2 + latitude2 # converts DATOS into a "SpatialPointsDataFrame"
class(datos) # See how now it is "SpatialPointsDataFrame"
proj4string(datos) # Still not coordiantes systems 

proj4string(datos)<-CRS("+proj=longlat +datum=WGS84 +no_defs") # This assigs the coordinate systems
proj4string(datos) 

datos<-spTransform(datos,CRS(proj4string(ecoregions)))
identical(proj4string(datos),proj4string(ecoregions)) # We want to see here TRUE

##################################################
# Find overlaping and adding extra column to the catch
##################################################

# Now we find the overlap between the points and poligons of the shapefiles so we can add the extra columns with
# ecoregion information into the catch database 

# Here we do the spatial overlap between points and polygons
points_with_ecoregions <- over(datos, ecoregions)

class(points_with_ecoregions)
head(points_with_ecoregions) #we still need to add the data attributes from the ecoregion shapefiles to the datapoint file. 
datos$Ecoregion_name <- points_with_ecoregions$Ecoregion #important line
datos$Ecoregion_ID <- points_with_ecoregions$Region_ID #important line


datos$Ecoregion_name<-as.factor(datos$Ecoregion_name)
datos$Ecoregion_ID<-as.factor(datos$Ecoregion_ID) # Don't worry about the Error: '[[...
class(datos)
summary(datos)
levels(datos$Ecoregion_name)

class(datos) # Still it is a "SpatialPointsDataFrame"
dim(datos)

datos_df<-as.data.frame(datos) # Convert back to data.frame before saving it to csv
head(datos_df)

######
# Save Catch & Effort with the ecoregion info added at the end. Once you do this once, you dont´t need to do it anymore and just use the file "Catch_&_Effort_OT_with_ecoregions_(no-reassigned).csv" for analysis
######
write.csv(datos_df,file="Catch_&_Effort_OT_with_ecoregions_(no-reassigned).csv") # SAVE FILE

### Yet there is a problem
### Plot the data - # see how there are some points are still OUTSIDE the ecoregions and OVER LAND. For those points the ecoregion assiged was a NA-we need to fix this.
map2 <- ggplot(data = world) + 
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", fill = NA) +geom_point(data=datos_df,aes(x=longitude, y=latitude)) 
print(map2)

###################################################
### Code for reasigning points outside poligons (ecoregions) to the closest poligons (ecoregions)
#########################################################

#open the Catch & Effort with the ecoregion information that we added in the previous step
catches_ecoregion <-read.csv("Catch_&_Effort_OT_with_ecoregions_(no-reassigned).csv", header=TRUE, sep=",")
head(catches_ecoregion)
summary(catches_ecoregion)

# Read and plot ecoregion poligons
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")
ecoregions_shp="Final_area_for_calculations.shp"
ecoregion<-readOGR(dsn = ecoregions_shp, stringsAsFactors = F) # Don't worry about the warnings()

setwd("~/.../1. Data_and_files_2024")
# Plot shapefile and points of catches
ggplot() + geom_polygon(data = ecoregion, aes(x = long, y = lat, group = group), colour = "grey", fill = NA) + geom_point(data=catches_ecoregion, aes(longitude,latitude))

##############################
##### Reassign outside cells with NA values into ecoregions shapefile
########################################
# Convert to data.frame>spatialpointsdataframe
coordinates(catches_ecoregion)<- c("longitude","latitude")

# Assign same projection as Ecoregion shapefile
proj4string(catches_ecoregion) <- proj4string(ecoregion)

# Removes the data points outside the IOTC grid
# Make a vector of logical values (true or false) of which points fall inside the ecorregions
inside.grid <- !is.na(over(catches_ecoregion, as(ecoregion, "SpatialPolygons"))) 
summary(inside.grid)

# Points inside ecoregion
head(catches_ecoregion)

# Here we create a df called inside with all the points that fall inside the ecorregions 
inside<-as.data.frame(catches_ecoregion[inside.grid,]) 
dim(inside)

#### Points outside
outside<-catches_ecoregion[!inside.grid,]
dim(outside)
catch_coords<-as.data.frame(unique(outside@coords))
head(catch_coords)
dim(catch_coords) # This tells us how many points fall outside the IOTC grid
outside<-as.data.frame(outside)

library(rgeos)

coordinates(catch_coords)<-c('longitude','latitude')
proj4string(catch_coords)<-proj4string(ecoregion)

head(outside)

# This code finds the distance of a point to all the ecoregins, and picks the smallest distance,
# and then assigns the name of the ecoregions to the columns Ecoregion_name, which before had a NA

#First we do it for the column ecoregion$Ecoregion
grid<-NULL
for (i in 1:length(catch_coords)) {
  grid[i] <- ecoregion$Ecoregion[which.min(gDistance(catch_coords[i,], ecoregion, byid=TRUE))]
  outside[which(outside$latitude==catch_coords@coords[i,2] & outside$longitude==catch_coords@coords[i,1]),'Ecoregion_name']<-grid[i]
} # Don't worry about the error: `*tmp*`[[jj]]...

warnings()#Second we do it for the column ecoregion$Region_ID 

grid<-NULL
for (i in 1:length(catch_coords)) {
  grid[i] <- ecoregion$Region_ID[which.min(gDistance(catch_coords[i,], ecoregion, byid=TRUE))]
  outside[which(outside$latitude==catch_coords@coords[i,2] & outside$longitude==catch_coords@coords[i,1]),'Ecoregion_ID']<-grid[i]
} # Don't worry about the error: `*tmp*`[[jj]]...

head(grid)
head(outside)
summary(outside)

# Plot a map to see if the identification of points that fall on land has been done correctly
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, aes(longitude,latitude), size=3, col='red') + 
  geom_point(data=inside, aes(longitude,latitude), size=3,col="blue") +
  ggtitle("Inside and outside points")

# Making sure the asigment to each ecoregion have been done correctly!
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, size=3, aes(longitude,latitude, col=Ecoregion_name)) + 
  geom_point(data=inside, size=3, aes(longitude,latitude,col=Ecoregion_name)) +
  ggtitle("Point assignation by ecorregion")

# Now that we have assiged all the "outside" points to an ecoregion, then combine both, 
# the inside points and outside points into a database again.
CE_OT_catches_ecoregion_reasigned<-rbind(inside,outside)

dim(CE_OT_catches_ecoregion_reasigned)
summary(CE_OT_catches_ecoregion_reasigned)

CE_OT_catches_ecoregion_reasigned$Ecoregion_name<-as.factor(CE_OT_catches_ecoregion_reasigned$Ecoregion_name)

CE_OT_catches_ecoregion_reasigned$Ecoregion_ID<-as.factor(CE_OT_catches_ecoregion_reasigned$Ecoregion_ID)

levels(CE_OT_catches_ecoregion_reasigned$Ecoregion_name)
levels(CE_OT_catches_ecoregion_reasigned$Ecoregion_ID)

summary(CE_OT_catches_ecoregion_reasigned$Ecoregion_name)
summary(CE_OT_catches_ecoregion_reasigned$Ecoregion_ID)

CE_OT_catches_ecoregion_reasigned[,16] <- NULL
CE_OT_catches_ecoregion_reasigned[,15] <- NULL
CE_OT_catches_ecoregion_reasigned[,1] <- NULL

# Ready to be save. You just need to do this once. And then just use "CE_OT_catches_ecoregion_reasigned.csv" for analysis.
write.csv(CE_OT_catches_ecoregion_reasigned, "CE_OT_catches_ecoregion_reasigned.csv")

##############################################################################################################################

#########################################################################################################################
##### Part 2/2:                                                                                                     #####
##### This script, calculates the proportion of catches inside and outside the model area for the fleets selected   #####
##### to obtain their catches using the Catch & Effot proportion aplied to their catches of the Nominal Catch       #####
##### Author: Roger Amate (AZTI)                                                                                    #####
##### Year: 2024                                                                                                    #####
#########################################################################################################################

########### FLEET: LL_CHN ###########
########### Calculation of proportions inside and outside MA ###########
# Set the working directory
setwd("~/.../4. Catches_of_fleets_C&E/1. LL_CHN")

# Read the data
LL_CHN<-read.csv(file="CE_LL_CHN_catches_ecoregion_reasigned.csv",header=TRUE, sep=",")

# Delate the columns that doesn't interest us
LL_CHN[,c(1,3:8)]<-NULL

###### Calculations ######
LL_CHN <- aggregate(CATCH ~  YEAR + SPECIES_CODE + Ecoregion_name, data = LL_CHN, sum)

LL_CHN$MA <- ifelse(LL_CHN$Ecoregion_name=='Model_area', 'YES', 'NO')

LL_CHN <- aggregate(CATCH ~  YEAR + SPECIES_CODE + MA, data = LL_CHN, sum)

LL_CHN_MA <- LL_CHN%>%subset(MA=='YES')
LL_CHN_OUT <- LL_CHN%>%subset(MA=='NO')

LL_CHN_inMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_CHN_MA, sum)
LL_CHN_outMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_CHN_OUT, sum)

# Crear una matriz con YEAR como columnas, SPECIES_CODE como filas, y CATCH como datos
LL_CHN_inMA_col <- reshape(LL_CHN_inMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_CHN_outMA_col <- reshape(LL_CHN_outMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")

LL_CHN_inMA_col[is.na(LL_CHN_inMA_col)] <- 0
LL_CHN_outMA_col[is.na(LL_CHN_outMA_col)] <- 0

###############################################

########### Calculation of catches of the fleet in the NC ###########
LL_CHN<-read.csv(file="IOTC_NC_RAW_[2000-2022][LL][CHN].csv",header=TRUE, sep=",")

# Delate the columns that don't interest us
LL_CHN[,c(2,4,6:11,13:15,17)]<-NULL
LL_CHN[,c(2,3)]<-NULL
LL_CHN_catch <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_CHN, sum)
LL_CHN_catch <- reshape(LL_CHN_catch, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_CHN_catch[is.na(LL_CHN_catch)] <- 0



########### FLEET: LL_EUESP ###########
########### Calculation of proportions inside and outside MA ###########

# Set the working directory
setwd("~/.../4. Catches_of_fleets_C&E/2. LL_EUESP")

# Read the data
LL_EUESP<-read.csv(file="CE_LL_EUESP_catches_ecoregion_reasigned.csv",header=TRUE, sep=",")

# Delate the columns that doesn't interest us
LL_EUESP[,c(1,3:6)]<-NULL

###### Calculations ######
LL_EUESP <- aggregate(CATCH ~  YEAR + SPECIES_CODE + Ecoregion_name, data = LL_EUESP, sum)

LL_EUESP$MA <- ifelse(LL_EUESP$Ecoregion_name=='Model_area', 'YES', 'NO')

LL_EUESP <- aggregate(CATCH ~  YEAR + SPECIES_CODE + MA, data = LL_EUESP, sum)

LL_EUESP_MA <- LL_EUESP%>%subset(MA=='YES')
LL_EUESP_OUT <- LL_EUESP%>%subset(MA=='NO')

LL_EUESP_inMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_EUESP_MA, sum)
LL_EUESP_outMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_EUESP_OUT, sum)

# Crear una matriz con YEAR como columnas, SPECIES_CODE como filas, y CATCH como datos
LL_EUESP_inMA_col <- reshape(LL_EUESP_inMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_EUESP_outMA_col <- reshape(LL_EUESP_outMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")

LL_EUESP_inMA_col[is.na(LL_EUESP_inMA_col)] <- 0
LL_EUESP_outMA_col[is.na(LL_EUESP_outMA_col)] <- 0

###############################################

########### Calculation of catches of the fleet in the NC ###########
LL_EUESP<-read.csv(file="IOTC_NC_RAW_[2000-2022][LL][EUESP].csv",header=TRUE, sep=",")

# Delate the columns that don't interest us
LL_EUESP[,c(2:11,13:15,17)]<-NULL
LL_EUESP_catch <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_EUESP, sum)
LL_EUESP_catch <- reshape(LL_EUESP_catch, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_EUESP_catch[is.na(LL_EUESP_catch)] <- 0



########### FLEET: LL_JPN ###########
########### Calculation of proportions inside and outside MA ###########
# Set the working directory
setwd("~/.../4. Catches_of_fleets_C&E/3. LL_JPN")

# Read the data
LL_JPN<-read.csv(file="Catch_and_Effort_LL_JPN_catches_ecoregion_reasigned.csv",header=TRUE, sep=",")

# Delate the columns that doesn't interest us
LL_JPN[,c(1)]<-NULL

###### Calculations ######
LL_JPN <- aggregate(CATCH ~  YEAR + SPECIES_CODE + Ecoregion_name, data = LL_JPN, sum)

LL_JPN$MA <- ifelse(LL_JPN$Ecoregion_name=='Model_area', 'YES', 'NO')

LL_JPN <- aggregate(CATCH ~  YEAR + SPECIES_CODE + MA, data = LL_JPN, sum)

LL_JPN_MA <- LL_JPN%>%subset(MA=='YES')
LL_JPN_OUT <- LL_JPN%>%subset(MA=='NO')

LL_JPN_inMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_JPN_MA, sum)
LL_JPN_outMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_JPN_OUT, sum)

# Crear una matriz con YEAR como columnas, SPECIES_CODE como filas, y CATCH como datos
LL_JPN_inMA_col <- reshape(LL_JPN_inMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_JPN_outMA_col <- reshape(LL_JPN_outMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")

LL_JPN_inMA_col[is.na(LL_JPN_inMA_col)] <- 0
LL_JPN_outMA_col[is.na(LL_JPN_outMA_col)] <- 0

###############################################

########### Calculation of catches of the fleet in the NC ###########
LL_JPN<-read.csv(file="IOTC_NC_RAW_[2000-2022][LL][JPN].csv",header=TRUE, sep=",")

# Delate the columns that don't interest us
LL_JPN[,c(2:11,13:15,17)]<-NULL
LL_JPN_catch <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_JPN, sum)
LL_JPN_catch <- reshape(LL_JPN_catch, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_JPN_catch[is.na(LL_JPN_catch)] <- 0



########### FLEET: LL_KOR ############
########### Calculation of proportions inside and outside MA ###########
# Set the orking directory
setwd("~/.../4. Catches_of_fleets_C&E/4. LL_KOR")

# Read the data
LL_KOR<-read.csv(file="CE_LL_KOR_catches_ecoregion_reasigned.csv",header=TRUE, sep=",")

# Delate the columns that doesn't interest us
LL_KOR[,c(1)]<-NULL

###### Calculations ######
LL_KOR <- aggregate(CATCH ~  YEAR + SPECIES_CODE + Ecoregion_name, data = LL_KOR, sum)

LL_KOR$MA <- ifelse(LL_KOR$Ecoregion_name=='Model_area', 'YES', 'NO')

LL_KOR <- aggregate(CATCH ~  YEAR + SPECIES_CODE + MA, data = LL_KOR, sum)

LL_KOR_MA <- LL_KOR%>%subset(MA=='YES')
LL_KOR_OUT <- LL_KOR%>%subset(MA=='NO')

LL_KOR_inMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_KOR_MA, sum)
LL_KOR_outMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_KOR_OUT, sum)

# Crear una matriz con YEAR como columnas, SPECIES_CODE como filas, y CATCH como datos
LL_KOR_inMA_col <- reshape(LL_KOR_inMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_KOR_outMA_col <- reshape(LL_KOR_outMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")

LL_KOR_inMA_col[is.na(LL_KOR_inMA_col)] <- 0
LL_KOR_outMA_col[is.na(LL_KOR_outMA_col)] <- 0

###############################################

########### Calculation of catches of the fleet in the NC ###########
LL_KOR<-read.csv(file="IOTC_NC_RAW_[2000-2022][LL][KOR].csv",header=TRUE, sep=",")

# Delate the columns that don't interest us
LL_KOR[,c(2:11,13:15,17)]<-NULL
LL_KOR_catch <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_KOR, sum)
LL_KOR_catch <- reshape(LL_KOR_catch, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_KOR_catch[is.na(LL_KOR_catch)] <- 0



########### FLEET: LL_SYC ###########
########### Calculation of proportions inside and outside MA ###########
# Set the orking directory
setwd("~/.../4. Catches_of_fleets_C&E/5. LL_SYC")

# Read the data
LL_SYC<-read.csv(file="CE_LL_SYC_catches_ecoregion_reasigned.csv",header=TRUE, sep=",")

# Delate the columns that doesn't interest us
LL_SYC[,c(1)]<-NULL

###### Calculations ######
LL_SYC <- aggregate(CATCH ~  YEAR + SPECIES_CODE + Ecoregion_name, data = LL_SYC, sum)

LL_SYC$MA <- ifelse(LL_SYC$Ecoregion_name=='Model_area', 'YES', 'NO')

LL_SYC <- aggregate(CATCH ~  YEAR + SPECIES_CODE + MA, data = LL_SYC, sum)

LL_SYC_MA <- LL_SYC%>%subset(MA=='YES')
LL_SYC_OUT <- LL_SYC%>%subset(MA=='NO')

LL_SYC_inMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_SYC_MA, sum)
LL_SYC_outMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_SYC_OUT, sum)

# Crear una matriz con YEAR como columnas, SPECIES_CODE como filas, y CATCH como datos
LL_SYC_inMA_col <- reshape(LL_SYC_inMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_SYC_outMA_col <- reshape(LL_SYC_outMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")

LL_SYC_inMA_col[is.na(LL_SYC_inMA_col)] <- 0
LL_SYC_outMA_col[is.na(LL_SYC_outMA_col)] <- 0

###############################################

########### Calculation of catches of the fleet in the NC ###########
LL_SYC<-read.csv(file="IOTC_NC_RAW_[2000-2022][LL][SYC].csv",header=TRUE, sep=",")

# Delate the columns that don't interest us
LL_SYC[,c(2:11,13:15,17)]<-NULL
LL_SYC_catch <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_SYC, sum)
LL_SYC_catch <- reshape(LL_SYC_catch, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_SYC_catch[is.na(LL_SYC_catch)] <- 0



########### FLEET: LL_TWN ###########
########### Calculation of proportions inside and outside MA ###########
# Set the working directory
setwd("~/.../4. Catches_of_fleets_C&E/6. LL_TWN")

# Read the data
LL_TWN<-read.csv(file="CE_LL_TWN_catches_ecoregion_reasigned.csv",header=TRUE, sep=",")

# Delate the columns that doesn't interest us
LL_TWN[,c(1)]<-NULL

###### Calculations ######
LL_TWN <- aggregate(CATCH ~  YEAR + SPECIES_CODE + Ecoregion_name, data = LL_TWN, sum)

LL_TWN$MA <- ifelse(LL_TWN$Ecoregion_name=='Model_area', 'YES', 'NO')

LL_TWN <- aggregate(CATCH ~  YEAR + SPECIES_CODE + MA, data = LL_TWN, sum)

LL_TWN_MA <- LL_TWN%>%subset(MA=='YES')
LL_TWN_OUT <- LL_TWN%>%subset(MA=='NO')

LL_TWN_inMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_TWN_MA, sum)
LL_TWN_outMA <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_TWN_OUT, sum)

# Crear una matriz con YEAR como columnas, SPECIES_CODE como filas, y CATCH como datos
LL_TWN_inMA_col <- reshape(LL_TWN_inMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_TWN_outMA_col <- reshape(LL_TWN_outMA, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")

LL_TWN_inMA_col[is.na(LL_TWN_inMA_col)] <- 0
LL_TWN_outMA_col[is.na(LL_TWN_outMA_col)] <- 0

###############################################

########### Calculation of catches of the fleet in the NC ###########
LL_TWN<-read.csv(file="IOTC_NC_RAW_[2000-2022][LL][TWN].csv",header=TRUE, sep=",")

# Delate the columns that doesn't interest us
LL_TWN[,c(2:11,13:15,17)]<-NULL
LL_TWN_catch <- aggregate(CATCH ~  YEAR + SPECIES_CODE, data = LL_TWN, sum)
LL_TWN_catch <- reshape(LL_TWN_catch, idvar = "YEAR", timevar = "SPECIES_CODE", direction = "wide")
LL_TWN_catch[is.na(LL_TWN_catch)] <- 0

####################################################################################################################
####################################################################################################################


