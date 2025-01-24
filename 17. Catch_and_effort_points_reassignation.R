
##################################################
##################################################

# Set the WD
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")


##################################################
### Longline
##################################################

# Open the dataset
datos_LL<-read.csv(file="IOTC-2024-WPB22-DATA04-CELongline.csv",header=TRUE, sep=",")

#####################################################################################################################
#####################################################################################################################

##################################################
#This script, opens the selected Catch and Effort dataset, opens IOTC shapefile, add the ecoregion information into the Rised catch dataset,
# then identify points outside the ecoregions which where assinged NAs, and find the closest ecoregion and assignes
# the closest ecoregions to the NAs.
#Use:
#Final_area_for_calculations.shp
##################################################

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
library("zoo")

##################################################
#read data 
##################################################
head(datos_LL)
names(datos_LL)
summary(datos_LL)
class(datos_LL)# see how it is a data.frame

# Select only the columns that interests us
datos_LL <- datos_LL[,c(1,2,3,7,8,9,13,15,17)]

# Fill NA values of catches with 0
datos_LL[is.na(datos_LL)] <- 0


##################################################
#read shapefile
##################################################

setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")

ecoregions<-readOGR("Final_area_for_calculations.shp", layer="Final_area_for_calculations")
class(ecoregions) # see how it is a "SpatialPolygonsDataFrame"

### plot ecoregions and points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

##################################################
#Transform fishing ground code into latitude and longitude
##################################################
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
# Split grid column into: size, quadrant, latitude and longitude
# In Grid size: 6=5ºx5º
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
# Transform the latitude and longitude so they represent the center of the 
# square and not the borders of it.
# When Grid_size is 6 in the northern hemisphere we have to add 2.5, and in the southern hemisphere we have to 
# add -2.5
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
datos$Ecoregion_ID<-as.factor(datos$Ecoregion_ID) # ni caso si da Error in '[[...
class(datos)
summary(datos)
levels(datos$Ecoregion_name)

class(datos) # still it is a "SpatialPointsDataFrame"
dim(datos)

datos_df<-as.data.frame(datos) #convert back to data.frame before saving it to csv
head(datos_df)

######
#save effort data with the ecoregion info added at the end. Once you do this once, you dont´t need to do it anymore and just use the file "CATDIS_with_ecoregions.csv" for analysis
######
write.csv(datos_df,file="Catch_&_Effort_LL_with_ecoregions_(no-reassigned).csv") # SAVE FILE


### Yet there is a problem
### Plot the data - # see how there are some points are still OUTSIDE the ecoregions and OVER LAND. For those points the ecoregion assiged was a NA-we need to fix this.
map2 <- ggplot(data = world) + 
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", fill = NA) +geom_point(data=datos_df,aes(x=longitude, y=latitude)) 

print(map2)

###################################################
### Code for reasigning points outside poligons (ecoregions) to the closest poligons (ecoregions)
#########################################################
#########################################################
#########################################################

#open the Rised catch with the ecoregion information that we added in the previous step
catches_ecoregion <-read.csv("Catch_&_Effort_LL_with_ecoregions_(no-reassigned).csv", header=TRUE, sep=",")

head(catches_ecoregion)

summary(catches_ecoregion)

#read and plot ecoregion poligons
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")
ecoregions_shp="Final_area_for_calculations.shp"
ecoregion<-readOGR(dsn = ecoregions_shp, stringsAsFactors = F)

setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
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
#ready to be save. You just need to do this once. And then just use "CATDIS_with_ecoregions_outside_pointsreasigned.csv" for analysis.

write.csv(CE_LL_catches_ecoregion_reasigned, "CE_LL_catches_ecoregion_reasigned.csv")


#####################################################################################################################
#####################################################################################################################


# Set the WD
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")


##################################################
### Purse Seine and BB
##################################################

# Open the dataset
datos_PSBB<-read.csv(file="IOTC-2024-WPB22-DATA05-CESurface.csv",header=TRUE, sep=",")

#####################################################################################################################
#####################################################################################################################

##################################################
#This script, opens the selected Catch and Effort dataset, opens IOTC shapefile, add the ecoregion information into the Rised catch dataset,
# then identify points outside the ecoregions which where assinged NAs, and find the closest ecoregion and assignes
# the closest ecoregions to the NAs.
#Use:
#Final_area_for_calculations.shp
##################################################

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
library("zoo")

##################################################
#read data 
##################################################
head(datos_PSBB)
names(datos_PSBB)
summary(datos_PSBB)
class(datos_PSBB)# see how it is a data.frame

# Select only the columns that interests us
datos_PSBB <- datos_PSBB[,c(1,2,3,7,8,9,12,13,14,15,16,17,18,19,20,21)]

# Fill NA values of catches with 0
datos_PSBB[is.na(datos_PSBB)] <- 0


##################################################
#read shapefile
##################################################

setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")

ecoregions<-readOGR("Final_area_for_calculations.shp", layer="Final_area_for_calculations")
class(ecoregions) # see how it is a "SpatialPolygonsDataFrame"

### plot ecoregions and points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

##################################################
#Transform fishing ground code into latitude and longitude
##################################################
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
# Split grid column into: size, quadrant, latitude and longitude
# In Grid size: 6=5ºx5º
# In hemisphere: 1=N and 2=S
datos <- datos_PSBB %>% separate(col = Grid, sep = c(1, 2, 4), 
                               into = c("Grid_size", "hemisphere", "latitude", "longitude"))
unique(datos$Grid_size)
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

# Transform the latitude and longitude so they represent the center of the 
# square and not the borders of it.
# When Grid_size is 6 in the northern hemisphere we have to add 2.5, and in the southern hemisphere we have to 
# add -2.5
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
datos$Ecoregion_ID<-as.factor(datos$Ecoregion_ID) # ni caso si da Error in '[[...
class(datos)
summary(datos)
levels(datos$Ecoregion_name)

class(datos) # still it is a "SpatialPointsDataFrame"
dim(datos)

datos_df<-as.data.frame(datos) #convert back to data.frame before saving it to csv
head(datos_df)

######
#save data with the ecoregion info added at the end. Once you do this once, you dont´t need to do it anymore and just use the file "CATDIS_with_ecoregions.csv" for analysis
######
write.csv(datos_df,file="Catch_&_Effort_PSBB_with_ecoregions_(no-reassigned).csv") # SAVE FILE


### Yet there is a problem
### Plot the data - # see how there are some points are still OUTSIDE the ecoregions and OVER LAND. For those points the ecoregion assiged was a NA-we need to fix this.
map2 <- ggplot(data = world) + 
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", fill = NA) +geom_point(data=datos_df,aes(x=longitude, y=latitude)) 

print(map2)

###################################################
### Code for reasigning points outside poligons (ecoregions) to the closest poligons (ecoregions)
#########################################################
#########################################################
#########################################################

#open the Rised catch with the ecoregion information that we added in the previous step
catches_ecoregion <-read.csv("Catch_&_Effort_PSBB_with_ecoregions_(no-reassigned).csv", header=TRUE, sep=",")

head(catches_ecoregion)

summary(catches_ecoregion)

#read and plot ecoregion poligons
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")
ecoregions_shp="Final_area_for_calculations.shp"
ecoregion<-readOGR(dsn = ecoregions_shp, stringsAsFactors = F)

setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
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
#ready to be save. You just need to do this once. And then just use "CATDIS_with_ecoregions_outside_pointsreasigned.csv" for analysis.

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

#####################################################################################################################
#####################################################################################################################

##################################################
#This script, opens the selected Catch and Effort dataset, opens IOTC shapefile, add the ecoregion information into the Rised catch dataset,
# then identify points outside the ecoregions which where assinged NAs, and find the closest ecoregion and assignes
# the closest ecoregions to the NAs.
#Use:
#Final_area_for_calculations.shp
##################################################

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
library("zoo")

##################################################
#read data 
##################################################
head(datos_OT)
names(datos_OT)
summary(datos_OT)
class(datos_OT)# see how it is a data.frame

# Select only the columns that interests us
datos_OT <- datos_OT[,c(1,2,3,7,8,9,13,15,17)]

# Fill NA values of catches with 0
datos_OT[is.na(datos_OT)] <- 0



##################################################
#read shapefile
##################################################

setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")

ecoregions<-readOGR("Final_area_for_calculations.shp", layer="Final_area_for_calculations")
class(ecoregions) # see how it is a "SpatialPolygonsDataFrame"

### plot ecoregions and points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

##################################################
#Transform fishing ground code into latitude and longitude
##################################################
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
# Split grid column into: size, quadrant, latitude and longitude
# In Grid size: 6=5ºx5º
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
# add -2.5
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
# notice how some points are OUTSIDE the ecoregions OVER land. when we merge the shapefile with the Rised catches 
# points, this points over land will be assigned a NA. There are ways to fix this. But so far, we are not fixing it.


##################################################
# Transforming and merging effort data and the ecorregions
##################################################

### To merge the Reffort data and Ecoregion_they need to be the same class, and have the same Coordinates System.

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
head(points_with_ecoregions) #we still need to add the data attributes from the ecoregion shapefiles to the datapoint file. 
datos$Ecoregion_name <- points_with_ecoregions$Ecoregion #important line
datos$Ecoregion_ID <- points_with_ecoregions$Region_ID #important line


datos$Ecoregion_name<-as.factor(datos$Ecoregion_name)
datos$Ecoregion_ID<-as.factor(datos$Ecoregion_ID) # ni caso si da Error in '[[...
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
write.csv(datos_df,file="Catch_&_Effort_OT_with_ecoregions_(no-reassigned).csv") # SAVE FILE


### Yet there is a problem
### Plot the data - # see how there are some points are still OUTSIDE the ecoregions and OVER LAND. For those points the ecoregion assiged was a NA-we need to fix this.
map2 <- ggplot(data = world) + 
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", fill = NA) +geom_point(data=datos_df,aes(x=longitude, y=latitude)) 

print(map2)

###################################################
### Code for reasigning points outside poligons (ecoregions) to the closest poligons (ecoregions)
#########################################################
#########################################################
#########################################################

#open the Rised catch with the ecoregion information that we added in the previous step
catches_ecoregion <-read.csv("Catch_&_Effort_OT_with_ecoregions_(no-reassigned).csv", header=TRUE, sep=",")

head(catches_ecoregion)

summary(catches_ecoregion)

#read and plot ecoregion poligons
setwd("~/OneDrive - AZTI/1. Tesis/5. Data/4. GIS/1. Layers/IOTC_fisheries_reassignation_layers")
ecoregions_shp="Final_area_for_calculations.shp"
ecoregion<-readOGR(dsn = ecoregions_shp, stringsAsFactors = F)

setwd("~/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/1. Catch and Effort/1. Good Data/1. Data_and_files_2024")
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
#ready to be save. You just need to do this once. And then just use "CATDIS_with_ecoregions_outside_pointsreasigned.csv" for analysis.

write.csv(CE_OT_catches_ecoregion_reasigned, "CE_OT_catches_ecoregion_reasigned.csv")









