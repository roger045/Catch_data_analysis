
#########################################################################################################################
###### This script opens the raw Rised catch database previously obtained from IOTC (.csv)                         ######
###### Merges the 5 species .csv files previousli separated manually                                               ######
###### Opens the shapefile created with the ecoregions of our interest (.shp)                                      ######
###### Adds the ecoregion information into the Rised catch dataset based on the latitude and longitude parameters  ######
###### Identifys points outside the ecoregions which where assinged NAs                                            ######
###### Finds the closest ecoregion for this points and assignes it instead of NAs                                  ######
###### Author: Maria José Juan Jordá (IEO-CSIC) and Roger Amate (AZTI)                                             ######
###### Year: 2023                                                                                                  ######
#########################################################################################################################

################################################################################
##### FINAL PRODUCT OF THE SCRIPT: Each register of a catch is assigned to an ecoregion based on the latitude and longitude 
################################################################################

### Requiered packages
library("ggplot2")
library('dplyr')
library('tidyr')
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library('rgdal')
library("rgeos")
library("maps")
library(rgdal)
library(sf)
theme_set(theme_bw())
###

rm(list=ls())

#This opens the 5 species datsheets and merge them into one big dataset called "datos"
# Set the working dierectory where the data of the rised catch of the 5 species separated is stored. The data must be in .csv format
setwd("C:/.../2. Workind data/6.Ecor_fleet_reassing")

alb<-read.csv(file="ALB.csv",header=TRUE, sep=";")
bet<-read.csv(file="BET.csv",header=TRUE, sep=";") 
skj<-read.csv(file="SKJ.csv",header=TRUE, sep=";")
swo<-read.csv(file="SWO.csv",header=TRUE, sep=";")
yft<-read.csv(file="YFT.csv",header=TRUE, sep=";")

# Combine files
datos<-rbind(alb,bet,skj,swo,yft)
summary(datos)

# Save the file
write.csv(datos, file="5sp.csv", row.names = FALSE)
##################################################

# Set the working dierectory where the data of the rised catch is stored. The data must be in .csv format
setwd("C:/.../2. Workind data/6.Ecor_fleet_reassing")

# Read the dataset
datos<-read.csv("5sp.csv", sep=",")
head(datos) # check the headers to know which variables there are
names(datos)
summary(datos) # get a summary of the data contained
class(datos)# see how it is a data.frame
##################################################

# Read shapefile
# Set the working dierectory where the shapefile of the ecoregions is stored. It must be in .shp format.
setwd("C:/.../1. Layers/IOTC_fisheries_reassignation_layers")

ecoregions<-readOGR("Final_area_for_calculations.shp", layer="Final_area_for_calculations")
class(ecoregions) # see how it is a "SpatialPolygonsDataFrame"

### create a basemap of the world for future maps
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Set the working dierectory back to where the data of the rised catch is stored or to a new one, where the newly generated datasets will be stored.
setwd("...")
##################################################

#Transform fishing ground code into latitude and longitude

# In the fishing ground column, the fishing ground code can be interpreted as, the first number is the grid size, 
# the second one is the hemisphere, the next two are the latitude, and the last three are the longitude
# Split grid column into: size, hemisphere, latitude and longitude
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

# Add a negative sign in front of latitudes if hemisphere=2 (southern hemisphere)
datos$latitude <- ifelse(datos$hemisphere==2, -datos$latitude, datos$latitude)

# Transform the latitude and longitude so they represent the center of the square and not the top left corner.
# In the northern hemisphere we have to add 2.5, and in the southern hemisphere we have to 
# add -2.5
datos$latitude <- ifelse(datos$hemisphere==2, -2.5+datos$latitude, 2.5+datos$latitude)

# 3rd, correct the value of the longitude 
# As all our values are in the Eastern hemisphere, we only need to add 2.5
# to all the values

datos$longitude <- 2.5+datos$longitude

# Trim white spaces
datos$FLEET_CODE <- trimws(datos$FLEET_CODE)
##################################################

# Visualizing which points fall over land
ggplot(data = world) + geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", 
                                    fill = NA) +geom_point(data=datos,aes(x=longitude, y=latitude)) 
# notice how some points are OUTSIDE the ecoregions OVER land. When we merge the shapefile with the Rised catches 
# points, this points over land will be assigned a NA. There are ways to fix this. But so far, we are not fixing it.
##################################################

# Transforming and merging rised cath and the ecorregions
### To merge the Rised Catch and Ecoregion they need to be the same class, and have the same Coordinates System.

proj4string(ecoregions) #"+proj=longlat +datum=WGS84 +no_defs
proj4string(datos) # this is a dataframe, and below we convert it to a "SpatialPointsDataFrame"

datos$longitude2<-datos$longitude #just making a copy
datos$latitude2<-datos$latitude #just making a copy

coordinates(datos) <- ~longitude2 + latitude2 # converts DATOS into a "SpatialPointsDataFrame"
class(datos) # see how now it is "SpatialPointsDataFrame"
proj4string(datos) # It still doesn't have a coordiantes reference systems assigned

proj4string(datos)<-CRS("+proj=longlat +datum=WGS84 +no_defs") #this assigs the coordinate systems
proj4string(datos) 

datos<-spTransform(datos,CRS(proj4string(ecoregions))) # here we convert the datos data frame to the same CRS as the ecoregions

identical(proj4string(datos),proj4string(ecoregions)) # We want to see here TRUE meanning that both have the same CRS
##################################################

### Find overlaping and adding extra column to Rised catch
# We find the overlap between the points and poligons of the shapefiles so we can add the extra columns with
# ecoregion information (names) into the Rised catch database 

# 1st We split the dataset of the points into 10 parts so the function over can be performed as if not it can't be performed
# by some computers due to too much computational power needed.

# Number of subsets
num_subsets <- 10

# Create a list to store the subsets
datos_subsets <- split(datos, cut(1:length(datos), breaks = num_subsets))

# Initialize an empty list to store the results
results_list <- list()

for (i in 1:num_subsets) {
  # Perform the spatial join for the i-th subset
  print(i) # with this line we see in which subset it is working
  points_with_ecoregions1 <- over(datos_subsets[[i]], ecoregions)
  
  # Append the result to the list
  results_list[[i]] <- points_with_ecoregions1
}

points_with_ecoregions <- do.call(rbind, results_list)
write.csv(points_with_ecoregions,file="points_with_ecoregions.csv")



# Here we do the spatial overlap between points and polygons looking for the points that fall over the ecoregions (can take a while 10-15minutes)
points_with_ecoregions <- over(datos, ecoregions)

class(points_with_ecoregions)
head(points_with_ecoregions) #we still need to add the data attributes from the ecoregion shapefiles to the datapoint file. 
datos$Ecoregion_name <- points_with_ecoregions$Ecoregion #important line
datos$Ecoregion_ID <- points_with_ecoregions$Region_ID #important line


datos$Ecoregion_name<-as.factor(datos$Ecoregion_name) # set the variables as factors
datos$Ecoregion_ID<-as.factor(datos$Ecoregion_ID) # set the variables as factors
class(datos)
summary(datos)
levels(datos$Ecoregion_name)

class(datos) # still it is a "SpatialPointsDataFrame"
dim(datos)

datos_df<-as.data.frame(datos) #convert back to data.frame before saving it to csv
head(datos_df)

# Save the rised cath with the ecoregion info added at the end. Once you do this once, you dont´t need to do it anymore and just use the file created for analysis
write.csv(datos_df,file="Rised_catch_5sp_with_the_model_ecoregions.csv") # SAVE FILE
##################################################

### Yet there is a problem
### Plot the data - see how there are some points are still OUTSIDE the ecoregions and OVER LAND. For those points the ecoregion assiged was a NA, we need to fix this.
ggplot(data = world) + 
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "red", fill = NA) +geom_point(data=datos_df,aes(x=longitude, y=latitude)) 

### Code for reasigning points outside poligons (ecoregions) to the closest poligons (ecoregions)

# Open the Rised catch with the ecoregion information that we added in the previous step
catches_ecoregion <-read.csv("Rised_catch_5sp_with_ecoregions.csv", header=TRUE, sep=",")

head(catches_ecoregion)
summary(catches_ecoregion)

# Read shapefile
# Set the working dierectory where the shapefile of the ecoregions is stored. It must be in .shp format.
setwd("...")

ecoregions_shp="Final_area_for_calculations.shp"
ecoregion<-readOGR(dsn = ecoregions_shp, stringsAsFactors = F)

# Set the working dierectory back to where the data of the rised catch is stored or to a new one, where the newly generated datasets will be stored.
setwd("...")

# Plot shapefile and points of catches
ggplot() + geom_polygon(data = ecoregion, aes(x = long, y = lat, group = group), colour = "grey", fill = NA) + geom_point(data=catches_ecoregion, aes(longitude,latitude))
##############################

#### Reassign outside cells with NA values into ecoregions shapefile

# Convert to data.frame>spatialpointsdataframe
coordinates(catches_ecoregion)<- c("longitude","latitude")

# Assign same projection as Ecoregion shapefile
proj4string(catches_ecoregion) <- proj4string(ecoregion)

# Remove the data points outside the ecoregion (points that fall over land)
# Find where catch points lie within the IOTC grid
# We make a vector of logical values (true or false) of which points fall inside the ecorregions
inside.grid <- !is.na(over(catches_ecoregion, as(ecoregion, "SpatialPolygons"))) 
inside.grid <- catches_ecoregion[!is.na(catches_ecoregion$Ecoregion_name), ] #subset with only the lines that fall inside the ecorregions area
outside <- catches_ecoregion[is.na(catches_ecoregion$Ecoregion_name), ] #subset with all the lines that fall on land and have NA in the ecoregion value

summary(inside.grid)

#points inside ecoregion
head(catches_ecoregion)

n <- nrow(inside.grid)
chunk_size <- ceiling(n / 20)  # Divide into 20 chunks for faster processing

# This function converts every chunk of the inside.grid data into spatial data and then convertts it into a data frame
convert_chunk <- function(start, end) {
  message(paste("Procesando chunk", start, "-", end))
  spdf_chunk <- inside.grid[start:min(end, n), ]
  sf_df_chunk <- st_as_sf(spdf_chunk)
  as.data.frame(sf_df_chunk)
}

# Here we unite all resulting dataframes into one big data frame called inside
inside <- bind_rows(
  lapply(seq(1, n, by = chunk_size), function(i) {
    convert_chunk(i, i + chunk_size - 1)
  })
)
dim(inside)

#### points outside
outside<-catches_ecoregion[!inside.grid,]
dim(outside)

proj4string(outside)<-CRS("+proj=longlat +datum=WGS84 +no_defs") # This assigs the coordinate systems
proj4string(outside)

unique_coords <- unique(outside[, c("longitude", "latitude")]) # Creates a data frame with each unique combination of latitude and longitude in the outside dataframe
catch_coords<-(unique(outside@coords))
head(catch_coords)
dim(catch_coords) # This tells us how many points fall outside the IOTC grid
outside<-as.data.frame(outside) # Transform it into a data frame

library(rgeos)

coordinates(unique_coords)<-c('longitude','latitude') # Trasnform it into a data frame of coordinates
proj4string(unique_coords)<-proj4string(ecoregion)  # Assign the same CRS

head(outside)
##############################

#This code finds the distance of a point to all the ecoregins, and picks the smallest distance, and then assigns the name of the ecoregions to the column Ecoregion_name, which before had a NA

#First we do it for the column ecoregion$Ecoregion
grid<-NULL
for (i in 1:length(unique_coords)) {
  grid[i] <- ecoregion$Ecoregion[which.min(gDistance(unique_coords[i,], ecoregion, byid=TRUE))] # Here for each value of a pair of lat and long in registerd in the catch coordinates it finds the nearest ecoregion 

  # Here it assigns to the outside data frame, for each pair of catch_coordinates, the Ecoregion assigned to that point in the previous step
  outside[which(outside$latitude==unique_coords@coords[i,2] & outside$longitude==unique_coords@coords[i,1]),'Ecoregion_name']<-grid[i] 
}

warnings() 
#Second we do it for the column ecoregion$Region_ID 
grid<-NULL
for (i in 1:length(unique_coords)) {
  grid[i] <- ecoregion$Region_ID[which.min(gDistance(unique_coords[i,], ecoregion, byid=TRUE))]
  
  outside[which(outside$latitude==unique_coords@coords[i,2] & outside$longitude==unique_coords@coords[i,1]),'Ecoregion_ID']<-grid[i]
}

head(grid)
head(outside)
summary(outside)

# Rename in the inside dataframe the longitude2 and latitude2 to longitude and latitude
names(inside)[13] <- 'longitude'
names(inside)[14] <- 'latitude'

ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, aes(longitude,latitude), size=3, col='red') + # Combine both datasets in the same plot, outside points 
  geom_point(data=inside, aes(longitude,latitude), size=3,col="blue") + # Combine both datasets in the same plot, inside points
  ggtitle("Inside and outside points")

### making sure the asigments have been done correct!
ggplot(data = world) +
  geom_polygon(data = ecoregions, aes(x = long, y = lat, group = group), colour = "grey10", fill = NA)+
  geom_sf(color = "darkgrey", fill = "lightgrey")+
  coord_sf(xlim = c(10,160), ylim = c(40,-60), expand = FALSE)+ 
  geom_point(data=outside, size=3, aes(longitude,latitude, col=Ecoregion_name)) + # Representation of both datasets with diferent colors by ecoregion
  geom_point(data=inside, size=3, aes(longitude,latitude,col=Ecoregion_name)) + # Representation of both datasets with diferent colors by ecoregion
  ggtitle("Point assignation by ecorregion")
##############################

write.csv(inside,file="inside.csv")
write.csv(outside,file="outside.csv")

# rename all the columns of the inside and outside dataframes so they match
colnames(inside) <- c('no1', 'Species', 'Year', 'Month', 'Grid_size', 'Hemisphere', 'Gear', 'SchoolType', 'Fleet', 'Num', 'MT', 'Longitude', 'Latitude', 
           'Ecoregion_name', 'no', 'no2')
colnames(outside) <- c('no1', 'no2', 'Species', 'Year', 'Month', 'Grid_size', 'Hemisphere','Latitude', 'Longitude', 'Gear', 'SchoolType', 'Fleet', 'Num', 'MT', 'Long', 'Lat',
                      'Ecoregion_name')
# Changes columns order to match Outside df
inside <- inside %>%
  select(1:6, 12:13, 7:11, 14:ncol(inside))
inside <- inside %>%
  select(1:6, 8, 7, 9:ncol(inside))

# Delate the columns that we don't need
inside[1] <- NULL
inside[15] <- NULL
inside[14] <- NULL

outside[16] <- NULL
outside[15] <- NULL
outside[2] <- NULL
outside[1] <- NULL

#now that we have assiged all the "outside" points to an ecoregion, then combine both, the inside points and outside points into a database again.
Rised_catch_5sp_catches_ecoregion_with_model_reasinged<-rbind(inside,outside)

dim(Rised_catch_5sp_catches_ecoregion_reasinged)
summary(Rised_catch_5sp_catches_ecoregion_reasinged)

Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_name<-as.factor(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_name) # Ecoregion name as factor

Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_ID<-as.factor(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_ID) # Ecoregion_ID as factor

levels(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_name)
levels(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_ID)

summary(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_name)
summary(Rised_catch_5sp_catches_ecoregion_reasinged$Ecoregion_ID)
# Ready to be save. You just need to do this once. And then just use ir for analysis.
write.csv(Rised_catch_5sp_catches_ecoregion_with_model_reasinged, "Rised_catch_5sp_catches_ecoregion_with_model_reasinged.csv")
##################################################################################################################
