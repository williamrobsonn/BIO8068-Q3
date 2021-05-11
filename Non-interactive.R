#Q3 Non interactive script

#Loading in packages and necessary scripts for Map analysis ----

library(leaflet)
library(leafem)
library(mapview)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(raster)
library(ggplot2)
library(dplyr)

#Elevation data for analysis ----

# Import raster elevation data Ordnance Survey projection
elevation <- raster("www/elevation.tif")

#Ensuring the elevation maps colours are plotted in correct order
plot(elevation, col=terrain.colors(30))

#Creating an interactive map and also changing the projection so it can be viewed in leaflet ----

#The elevation500m is also changing the quality so it can be loaded in faster when being displayed 
#via leaflet and mapview

ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude

elevation_ll <- projectRaster(elevation, crs=ll_crs)

elevation500m <- aggregate(elevation, fact=10) # fact=10 is the number of cells aggregated together

elevation500m_ll <- projectRaster(elevation500m, crs=ll_crs)

mapview(elevation500m_ll)

#Saving as RDS file for import later on

saveRDS(elevation500m_ll, file = "elevation500m_ll.RDS")


#Now we will display the elevation data in leaflet 

elevation_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "Elevation") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Elevation"),
    options = layersControlOptions(collapsed = TRUE)
  )

elevation_view #This displays it 

#Displaying vector data and transforming so it can be displayed in leaflet ----

#This is all done in a similar fashion to the elevation data, using a conversion to long lat 
#aka the crs=ll_crs function.

#Settlement data 

settlement <- st_read("www/cumbria_settlements.shp")

settlement_ll <- st_transform(settlement,crs=ll_crs)

settlement_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addFeatures(settlemnt_ll, group = "Settlements") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Settlements"),
    options = layersControlOptions(collapsed = TRUE)
  )

settlement_view

#Saving as RDS

saveRDS(settlement_ll, file = "settlement_ll.RDS")

#Lake data

lakes <- st_read("www/cumbria_lakes.shp")

lakes_ll <- st_transform(lakes,crs=ll_crs)

lake_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
  addFeatures(lakes_ll, group = "Lakes") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes"),
    options = layersControlOptions(collapsed = TRUE)
  )

lake_view

#Saving as RDS

saveRDS(lakes_ll, file = "lakes_ll.RDS")

#Roads data

roads <- st_read("www/cumbria_roads.shp")

roads_ll <- st_transform(roads,crs=ll_crs)

addFeatures(roads_ll, group = "Roads")

#Saving as RDS

saveRDS(roads_ll, file = "roads_ll.RDS")

#rivers data 

river <- st_read("www/cumbria_rivers.shp")

rivers_ll <- st_transform(river,crs=ll_crs)

addFeatures(rivers_ll, group = "Rivers")

#Saving as RDS

saveRDS(rivers_ll, file = "rivers_ll.RDS")

#Collecting data from NBN data ----

#Data is from a manual download as R code for data failed to complete
#All of the data is for the Cumbrian county

#Cuckoo records data
cuckoo_records <- read.csv("NBN/Cuckoo_records.csv")

#Now getting rid of the data that is not in Accepted filter
cuckoo_records <- cuckoo_records[cuckoo_records$identificationVerificationStatus.processed == "Accepted",]

#Plotting to observe trends over time

ggplot(cuckoo_records, aes(x=year.processed)) +
  geom_histogram()

#Now viewing trends over time
cuckoo_records_per_yr <- cuckoo_records %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

#Using ggplot as more interactive
ggplot(cuckoo_records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Birds observed")

#Now to carry out the same code for the remaining two species 

#Hen harriers
henharriers_records <- read.csv("NBN/Hen_harriers_records.csv")
henharriers_records <- henharriers_records[henharriers_records$identificationVerificationStatus.processed == "Accepted",]

#There is no need to look at Hen harrier data over time as there is only one record due to the filtering of data

#Long eared owl data
longearedowl_records <- read.csv("NBN/Longearedowl_records.csv")
longearedowl_records <- longearedowl_records[longearedowl_records$identificationVerificationStatus.processed == "Accepted",]

#observing trends
ggplot(longearedowl_records, aes(x=year.processed)) +
  geom_histogram()

longearedowl_records_per_yr <- longearedowl_records %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(longearedowl_records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Birds observed")

#Now joining all bird datapoints together in leaflet view 
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
                     options = layersControlOptions(collapsed = TRUE))  
        
bird_plot


#Plotting all inputs together on one leaflet map ----

#Now I have all of my data in the correct format I can display them in one large leaflet map and create toggles 
#so it doesn't appear too crowded for the interactive version.

leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addFeatures(elevation500m_ll, group = "Elevation") %>% 
  addCircleMarkers(cuckoo_records$decimalLongitude.processed, cuckoo_records$decimalLatitude.processed, label = cuckoo_records$scientificName.processed, group = "Cuckoo", 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red", popup = cuckoo_records$scientificNameprocessed) %>%
  addCircleMarkers(henharriers_records$decimalLongitude.processed, henharriers_records$decimalLatitude.processed, label = henharriers_records$scientificName.processed, group = "Hen harrier", 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", popup = henharriers_records$scientificName.processed) %>%
  addCircleMarkers(longearedowl_records$decimalLongitude.processed, longearedowl_records$decimalLatitude.processed, label = longearedowl_records$scientificName.processed, group = "Long eared owl",
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="yellow", popup = longearedowl_records$scientificNameprocessed) %>%
  addFeatures(lakes_ll, group = "Lakes") %>% 
  addFeatures(settlement_ll, group = "Settlements", label = settlement_ll$NAME, labelOptions = labelOptions(interactive = "TRUE")) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Elevation", "Lakes", "Settlements", "Cuckoo", "Hen harrier", "Long eared owl"),
    options = layersControlOptions(collapsed = TRUE)
  )
