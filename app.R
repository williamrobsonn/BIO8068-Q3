#Shiny app for Q3 of the assessment

#Loading in libraries to analysis and plotting can happen ----

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

#A lot of data is required to get this working so read in all of the following sections

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

longearedowl_records_per_yr <- longearedowl_records %>% 
    group_by(year.processed) %>% 
    summarise(count_per_year = n())

cuckoo_records_per_yr <- cuckoo_records %>% 
    group_by(year.processed) %>% 
    summarise(count_per_year = n())

#Images ----
cuckoo_image <- base64enc::dataURI(file="www/cuckoos.PNG", mime="image/png")

hen_harrier_image <- base64enc::dataURI(file="www/hen_harriers.PNG", mime="image/png")

longeared_owl_image <- base64enc::dataURI(file="www/long_eared_owls.PNG", mime="image/png")

# Define UI for application that displays map and images relevant to Cumbria ----

ui <- fluidPage(
    
    # Application title
    titlePanel("The environment of Cumbria"),
    
    # Sidebar  
    sidebarLayout(
        sidebarPanel( p("Before you dive into the interactive map located in the centre panel of this page, it is worth explaining some of the data 
        included. In the sidebar will be images and brief descriptions of the bird species so that readers can decipher the interactive map."),
                      
                      p("The Common Cuckoo (Cuculus canorus) (marker colour red) is a visitor to Cumbria. Heard from early Spring, until the
        end of the mating season. They have a distinctive call and are the only bird to be named after their call. They are becoming more rare
        in Cumbria, due to disturbances during their annual migration from Africa. An image of the bird will be displayed below. If you draw your attention
                        to the chart in the main section, you will see a figure displaying the number of cuckoos observed over time. According to this 
                        figure, no cuckoos have been sighted in Cumbria in recent time."), 
                      
                      img(src=cuckoo_image,height="90%", width="90%", align = "centre"),
                      
                      p("Now we will also look at an image of a Hen harrier (Circus cyaneus) (marker colour blue), they are an incredibly rare raptor species
        that has a long history of being persecuted on active moorland (where shooting and farming activities takes place), if
        you view the map you will notice that there is only one displayed. There are numerous recordings of the Hen harrier in Cumbria per year, but most are
        not verified. Therefore, for accuracy sake, cannot be used/trusted as true sightings. Below is an image of the Hen harrier. There is no plot in the central panel
                        of this shiny webApp, this is simply because there is only one confirmed record so will not display on a figure."), width = 4, height = 25,
                      
                      img(src=hen_harrier_image,height="100%", width="100%", align = "centre"), 
                      
                      p("Finally we will look at the Long eared owls (Asio otus) (marker colour yellow). 
                       They prefer to hunt on low land areas and are most active at dusk/dawn.
                       Below is an image of the Long eared owl. In the centre panel there is a figure displaying the number of records sighted over time, in 
                        this case, it is slightly increasing!"),
                      
                      img(src=longeared_owl_image,height="100%", width="100%", align = "centre"),
                      
                      p("Now we have looked at the images of the species we can now display them on the interactive map. Use the side panel to change 
                       which data can be observed and if you are willing use the buttons below to enter your answer to the question."),
                      
                     
                     actionButton(inputId="my_submitstatus", label="Enter Submission"),
                     radioButtons(inputId = "my_checkgroup", 
                                  h2("Select the species you have seen"), 
                                  choices = list("Cuckoo" = 1, 
                                                 "Hen harrier" = 2, 
                                                 "Long eared owl" = 3,
                                                "None" = 4),
                                  selected = 1)), 
        
        mainPanel( p("As explained in the side bar, this interactive website has been created to display different types of environmental data 
        for the county of Cumbria. It includes data for three", em("rare"), "bird species, these
        species being: Cuckoos, Hen harriers and Long eared owls (pictured and briefly described at the side). There will also be other environmental 
        data displayed that the user can toggle on/off at their pleasure. Such data includes: elevation,
        lake data (can you locate the only", strong('lake'), "in the lake district? Hint: Its above Keswick) 
        and settlement data. Please allow a few moments for the data to load in as some delays can be expected."),
                   
                   plotOutput(outputId = "owls_plot"),
                   
                   plotOutput(outputId = "cuckoo_plot"),
                   
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
        text<-paste("Lattitude ", click$lat, "Longititude ", click$lng)
        print(text)
    })
    
    output$owls_plot <- renderPlot(
        ggplot(longearedowl_records_per_yr, aes(x = year.processed, y=count_per_year)) +
            geom_line() + xlab("Years of observation") + ylab("Owls observed") + 
            theme_classic())
    
    output$cuckoo_plot <- renderPlot(
        ggplot(cuckoo_records_per_yr, aes(x = year.processed, y=count_per_year)) +
            geom_line() + xlab("Years of observation") + ylab("Cuckoos observed") +
            theme_classic())
    
}

# Run the shiny----

shinyApp(ui = ui, server = server)

