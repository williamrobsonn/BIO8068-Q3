#Non-interactive 2 ----
.
#Loading the packages for use ----
library(leaflet)
library(leafem)
library(mapview)
library(sf)

#Loading the data to be viewed ----
brampton_fields <- st_read("www/crop_map.shp")

st_crs(brampton_fields)

# First reset brampton fields to OS 27700  ----
brampton_fields <- brampton_fields %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)

# Transform to latitude longitude ----
brampton_fields_ll <- st_transform(brampton_fields, 4326) # Lat-Lon

plot(brampton_fields_ll)

#Creating interactive version ----
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addFeatures(brampton_fields_ll)

#Test view without added features ----
brampton_fields_ll[brampton_fields$crop_name=="Potatoes",]
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addFeatures(brampton_fields_ll[brampton_fields$crop_name=="Oilseed rape",],
              fillColor="green",
              color="white",
              opacity =0.7,
              fillOpacity=1) %>% 
  addFeatures(brampton_fields_ll[brampton_fields_ll$crop_name=="Grass",],
              fillColor="red",
              color="yellow", 
              fillOpacity=1) %>% 
  addFeatures(brampton_fields_ll[brampton_fields_ll$crop_name=="Potatoes",],
              fillColor="purple",
              color="orange", 
              fillOpacity=1)
  
#Testing legends ----
# Set the bins to divide up your areas
bins <- c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000)

# Decide on the colour palatte
pal <- colorQuantile(palette = "Greens", n=3, domain = brampton_fields_ll$crop_code)

# Create the map (experimental)

field_info <- paste("Crop: ", brampton_fields_ll$crop_name)
                    "<strong>")

leaflet(brampton_fields_ll) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addFeatures(brampton_fields_ll,
              opacity = 0.7,
              fillColor = "white",
              color = "green",
              fillOpacity = 1,
              highlightOptions = highlightOptions(color = "yellow", weight = 5,
                                                  bringToFront = TRUE),
              popup = field_info) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Potatoes", "Oilseed rape", "Grass", "Other groups", "Winter wheat", "Maize"),
    options = layersControlOptions(collapsed = TRUE)
  )


#Almost final design ----

leaflet() %>%
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addFeatures(brampton_fields_ll[brampton_fields_ll$crop_name=="Potatoes",],
              fillColor="white",
              color="green",
              opacity =0.7,
              fillOpacity=1,
              group = "Potatoes") %>% 
  addFeatures(brampton_fields_ll[brampton_fields_ll$crop_name=="Maize",],
              fillColor="white",
              color="green",
              opacity =0.7,
              fillOpacity=1,
              group = "Maize") %>% 
  addFeatures(brampton_fields_ll[brampton_fields_ll$crop_name=="Oilseed rape",],
              fillColor="red",
              color="yellow", 
              fillOpacity=1,
              group = "Oilseed rape") %>% 
  addFeatures(brampton_fields_ll[brampton_fields_ll$crop_name=="Winter wheat",],
              fillColor="white",
              color="green",
              opacity =0.7,
              fillOpacity=1,
              group = "Winter wheat ") %>% 
  addFeatures(brampton_fields_ll[brampton_fields_ll$crop_name=="Grass",],
              fillColor="purple",
              color="orange", 
              fillOpacity=1,
              group = "Grass") %>% 
  addFeatures(brampton_fields_ll[brampton_fields_ll$crop_name=="Spring barley",],
              fillColor="white",
              color="green",
              opacity =0.7,
              fillOpacity=1,
              group = "Spring barley") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Potatoes", "Oilseed rape", "Grass", "Spring Barley", "Winter wheat", "Maize"),
    options = layersControlOptions(collapsed = TRUE)
  )

