#Q3 Non interactive script

#Loading in packages and necessary scripts for GIS analysis ----

library(leaflet)
library(leafem)
library(mapview)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(raster)
library(rgbif)
library(ggplot2)
library(dplyr)
library(rgbif)
library(BIRDS)

#NBN data ----

#Cuckoos first
cuckoo_records <- read.csv("NBN/Cuckoo_records.csv")

#Plotting to observe trends
ggplot(cuckoo_records, aes(x=year.processed)) +
  geom_histogram()

cuckoo_records_per_yr <- cuckoo_records %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(cuckoo_records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Birds observed")

#Hen harriers
henharriers_records <- read.csv("NBN/Hen_harriers_records.csv")

#observing trends
ggplot(henharriers_records, aes(x=year.processed)) +
  geom_histogram()

henharriers_records_per_yr <- henharriers_records %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(henharriers_records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Birds observed")

#Long eared owl data
longearedowl_records <- read.csv("NBN/Longearedowl_records.csv")

#observing trends
ggplot(longearedowl_records, aes(x=year.processed)) +
  geom_histogram()

longearedowl_records_per_yr <- longearedowl_records %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(longearedowl_records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Birds observed")

#Now joining all datapoints together
bird_plot <- leaflet() %>%
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(cuckoo_records$decimalLongitude.processed, cuckoo_records$decimalLatitude.processed, label = cuckoo_records$scientificName.processed, 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red", popup = cuckoo_records$scientificNameprocessed) %>%
  addCircleMarkers(henharriers_records$decimalLongitude.processed, henharriers_records$decimalLatitude.processed, label = henharriers_records$scientificName.processed, 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", popup = henharriers_records$scientificName.processed) %>%
  addCircleMarkers(longearedowl_records$decimalLongitude.processed, longearedowl_records$decimalLatitude.processed, label = longearedowl_records$scientificName.processed,
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="yellow", popup = longearedowl_records$scientificNameprocessed) %>%  
                      addLayersControl(
                     baseGroups = c("OSM (default)", "Satellite"), 
                     overlayGroups = c("Cuckoo", "Hen Harrier", "Long eared owl"),
                     options = layersControlOptions(collapsed = TRUE)) %>% 
                     addLegend("bottomright", colors = "reds", 
                    opacity=1, labels="Cuckoo", "Hen harrier", "Long eared owl")
  

bird_plot

#Testing to see if leaflet will read in vectors ----

settlement <- st_read("www/cumbria_settlements.shp")
settlemnt_ll <- st_transform(settlement,crs=ll_crs)

settlement_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addFeatures(settlemnt_ll) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Settlements"),
    options = layersControlOptions(collapsed = TRUE)
  )

settlement_view

lakes <- st_read("www/cumbria_lakes.shp")
lakes_ll <- st_transform(lakes,crs=ll_crs)

lake_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
  addFeatures(lakes_ll) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes"),
    options = layersControlOptions(collapsed = TRUE)
  )

lake_view #This works now

elevation_view <- leaflet() %>% 
addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  #setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
  addRasterImage(elevation_ll,col=terrain.colors(30)) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Elevation"),
    options = layersControlOptions(collapsed = TRUE)
  )

elevation_view #This also works 



#Plotting the raster elevation maps ----

# Import raster elevation data Ordnance Survey projection
elevation <- raster("www/elevation.tif")

plot(elevation) 

#These colours are reversed so we need to flip them back. 
# use the terrain.colors option to set low elevation to green, high to brown, 
# with 30 colour categories

plot(elevation, col=terrain.colors(30)) #Looks clearer now

#Creating an interactive map and also changing the projection so it can be viewed ----

ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)

mapview(elevation_ll)

#Add DTM data to site data (wind turbines) ----

wind_turbines <- st_read("www/windfarm.shp")

print(wind_turbines) 
#This just looks at the layout from BIO8069 that I set for
#the wind turbines

plot(elevation, col=terrain.colors(30))
plot(wind_turbines, add=TRUE) #Viewing on map, appears as circle around South Western lakes

mapview(st_transform(wind_turbines, 4326)) #Viewing on interactive map

wind_turbines #viewing the changes

#Create a viewshed (scrapped) ----

source("LOS.R")


# Convert to latitude-longitude; EPSG code 4326
wind_turbines_ll <- st_transform(wind_turbines, 4326)
mapview(wind_turbines_ll)

windfarm_5 <- dplyr::filter(wind_turbines, Turb_ID == "5")

# Change to coarser 500m elevation map for speed
elevation500m <- aggregate(elevation, fact=10) # fact=10 is the number of cells aggregated together


