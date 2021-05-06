#Shiny app for Q3 of the assessment

library(shiny)
library(shinipsum)
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

#Elevation data ----
elevation <- raster("www/elevation.tif")
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)
elevation500m <- aggregate(elevation, fact=10) # fact=10 is the number of cells aggregated together
elevation500m_ll <- aggregate(elevation500m, crs=ll_crs)

#Lakes data ----
lakes <- st_read("www/cumbria_lakes.shp")
lakes_ll <- st_transform(lakes,crs=ll_crs)

#Settlements data ----
settlement <- st_read("www/cumbria_settlements.shp")
settlement_ll <- st_transform(settlement,crs=ll_crs)

#Birds data ----
longearedowl_records <- read.csv("NBN/Longearedowl_records.csv")
longearedowl_records <- longearedowl_records[longearedowl_records$identificationVerificationStatus.processed == "Accepted",]
henharriers_records <- read.csv("NBN/Hen_harriers_records.csv")
henharriers_records <- henharriers_records[henharriers_records$identificationVerificationStatus.processed == "Accepted",]
cuckoo_records <- read.csv("NBN/Cuckoo_records.csv")
cuckoo_records <- cuckoo_records[cuckoo_records$identificationVerificationStatus.processed == "Accepted",]


# Define UI for application that displays image of cumbria
ui <- fluidPage(
    
    # Application title
    titlePanel("The environment of Cumbria"),
    
    # Sidebar  
    sidebarLayout(
        sidebarPanel(),
        
        mainPanel(
            leafletOutput(outputId = "map")
        )
    )
)


server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles(group = "OSM (default)") %>% 
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
            addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "Elevation") %>%
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
                options = layersControlOptions(collapsed = FALSE)
            ) 
        
    })
    observeEvent(input$map, {
        click<-input$map
        text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
        print(text)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

