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



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("An environmental overview of Cumbria"),
    
    # Sidebar  
    sidebarLayout(
        sidebarPanel(),
        
        mainPanel(
            leafletOutput(outputId = "elevation_view")
        )
    )
)


server <- function(input, output) {
    output$elevation_view <- renderLeaflet({
        leaflet() %>% 
            addTiles(group = "OSM (default)") %>% 
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
            #setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
            addRasterImage(elevation_ll,col=terrain.colors(30)) %>% 
            addLayersControl(
                baseGroups = c("OSM (default)", "Satellite"), 
                overlayGroups = c("Elevation"),
                options = layersControlOptions(collapsed = TRUE)
            )
    })
    observeEvent(input$elevation_view, {
        click<-input$elevation_view
        text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
        print(text)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

