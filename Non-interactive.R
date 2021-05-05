#Q3 Non interactive script

#Loading in packages and necessary scripts for GIS analysis ----

library(leaflet)
library(leafem)
library(mapview)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(raster)

#Testing to see if leaflet will read in vectors ----
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

#Creating contours from raster DTM ----

elevation_contours <- rasterToContour(elevation) %>% st_as_sf()
plot(elevation, col=terrain.colors(30)) #Remember to add this code each time to alter colouring patterns
plot(elevation_contours, add=TRUE)

mapview(elevation_contours, add=TRUE) #Interactive view again

#Add DTM data to site data (wind turbines) ----

wind_turbines <- st_read("www/windfarm.shp")

print(wind_turbines) 
#This just looks at the layout from BIO8069 that I set for
#the wind turbines

plot(elevation, col=terrain.colors(30))
plot(wind_turbines, add=TRUE) #Viewing on map, appears as circle around South Western lakes

mapview(st_transform(wind_turbines, 4326)) #Viewing on interactive map

#Calculating slope and aspect aspect ----

slope  <- terrain(elevation, unit="degrees") # defaults to slope
aspect <- terrain(elevation, opt="aspect", unit="degrees")

plot(slope, col=terrain.colors(30))
plot(aspect, col=terrain.colors(30))

#Add slope and aspect values to wind turbines attributes ----
#(This may not be needed as windfarm already has slope and aspect)

wind_turbines$slope <- extract(slope, wind_turbines)
wind_turbines$aspect <- extract(aspect, wind_turbines)

wind_turbines #viewing the changes

#Create a viewshed ----

source("LOS.R")


# Convert to latitude-longitude; EPSG code 4326
wind_turbines_ll <- st_transform(wind_turbines, 4326)
mapview(wind_turbines_ll)

viewshed_windfarm <- dplyr::filter(wind_turbines, Turb_ID == "5")

# Change to coarser 500m elevation map for speed
elevation500m <- aggregate(elevation, fact=10) # fact=10 is the number of cells aggregated together

# Extract just the geometry for a single mast, and pass to viewshed function.
# Adding a 5km maximum radius
# Takes 1 to 2 minutes to run viewshed depending on your PC
windfarm_geom <- st_geometry(viewshed)[[1]]
viewshed <- viewshed(dem=elevation500m, windfarm=windfarm_geom,
                          h1=1.5, h2=50, radius=5000) #Fix this one as broken 


