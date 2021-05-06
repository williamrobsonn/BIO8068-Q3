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

#Images ----

cuckoo_image <- base64enc::dataURI(file="www/cuckoos.PNG", mime="image/png")
hen_harrier_image <- base64enc::dataURI(file="www/hen_harriers.PNG", mime="image/png")
longeared_owl_image <- base64enc::dataURI(file="www/long_eared_owls.PNG", mime="image/png")
# Define UI for application that displays image of cumbria
ui <- fluidPage(
    
    # Application title
    titlePanel("The environment of Cumbria"),
    
    # Sidebar  
    sidebarLayout(
        sidebarPanel(p("The Common Cuckoo (Cuculus canorus) (marker colour red) is a visitor to Cumbria. Heard from early Spring, until the
        end of the mating season. They have a distinctive call and are the only bird to be named after their call. They are becoming more rare
        in Cumbria, due to disturbances during their annual migration from Africa. An image of the bird will be displayed below"), 
                     
                     img(src=cuckoo_image,height="30%", width="30%", align = "centre"),
                     
                     p("Now we will also look at an image of a Hen harrier (Circus cyaneus) (marker colour blue), they are an incredibly rare raptor species
        that has a long history of being persecuted on active moorland (where shooting and farming activities takes place), if
        you view the map you will notice that there is only one displayed. There are numerous recordings of the Hen harrier in Cumbria per year, but most are
        not verified. Therefore, for accuracy sake, cannot be used/trusted as true sightings. Below is an image of the Hen harrier"), width = 12, height = 20,
                     
                     img(src=hen_harrier_image,height="30%", width="30%", align = "centre"), 
                     
                     p("Finally we will look at the Long eared owls (Asio otus) (marker colour yellow). 
                       They prefer to hunt on low land areas and are most active at dusk/dawn.
                       Below is an image of the Long eared owl"),
                     
                     img(src=longeared_owl_image,height="30%", width="30%", align = "centre")), 
        
        mainPanel( p("This interactive website has been created to display various environmental data 
        for the county of Cumbria. It includes data for three", em("rare"), "bird species, these
        species being: Cuckoos, Hen harriers and Long eared owls (pictured and briefly described above). There will also be other environmental 
        data displayed that the user can toggle on/off at their pleasure. Such data includes: elevation,
        lake data (can you locate the only", strong('lake'), "in the lake district? Hint: Its above Keswick) 
        and settlement data. Please allow a few moments for the data to load in as some delays can be expected."),
               
               leafletOutput(outputId = "map"))))


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

