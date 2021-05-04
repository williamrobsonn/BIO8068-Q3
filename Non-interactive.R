#Q3 Non interactice script

#Loading in packages and necessary scripts for GIS analysis ----

library(leaflet)
library(leafem)
library(mapview)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(sf)
library(raster)

#Plotting the raster elevation maps ----

# Import raster elevation data Ordnance Survey projection
elevation <- raster("gis_data/elevation.tif")
plot(elevation)

#These colours are reversed so we need to flip them back. 
# use the terrain.colors option to set low elevation to green, high to brown, 
# with 30 colour categories
plot(elevation, col=terrain.colors(30)) #Looks clearer now

#Creating an interactive map and also changing the projection so it can be viewed ----

ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)
mapview(elevation_ll)





