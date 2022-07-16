library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)

# dat = data.frame(name = "Sidney Airport - Stirling Way",
#            productivity = "High")
# 
# dat = st_set_geometry(dat, st_geometry(st_cast(c(st_point(c(48.64726984376748, -123.40917029173556)),
#   st_point(c(48.64734072836236, -123.40892352850621)),
#   st_point(c(48.6468516226296, -123.40661682875333)),
#   st_point(c(48.64527794591409, -123.40584435255703)),
#   st_point(c(48.64468957669249, -123.40657391340909)),
#   st_point(c(48.64468248785449, -123.40792574675264)),
#   st_point(c(48.64599325820387, -123.40891219680323))), "POLYGON")))
# 
# write_sf(dat, "blackberry_patches.gpkg")

dat = read_sf("blackberry_patches.gpkg")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Blackberry Map"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("search",
                        label = "Number of bins:",
                        options = unique(dat$name)),
            fileInput("user_poly", "Upload Polygon (geopackage or zipped shapefile)",
                      multiple = T, accept = c(".gpkg")),
            h3("Describe New Patch"),
            textInput(inputId = "drawn_patch_name",
                      label = "Patch Name"),
            selectizeInput(inputId = "drawn_patch_richness",
                           label = "Patch Productivity",
                           options = c("Low","Medium","High")),
            actionButton("add_drawn_poly", "Add drawn patch to database")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("leafmap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    Dat = reactive(dat)
    
    UserPoly = reactive({
        user_file = input$user_poly
        if(is.null(user_file)) return(NULL)
        #If user uploads geopackage, read in and add to dat.
        if(str_detect(user_file$datapath, ".gpkg")){
            userpoly = read_sf(user_file$datapath) %>% 
                st_transform(crs = 4326)
            
            userpoly
        }
        #If user uploads zipped shapefile, unzip then read in.
        if(str_detect(user_file$datapath, ".zip")){
            
            filelist = unzip(user_file$datapath)
            userpoly = read_sf(filelist[str_detect(filelist, ".shp")]) %>% 
                st_transform(crs = 4326)
            
            userpoly
        }
    })
    
    output$leafmap <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles("Esri.WorldImagery",
                             group = "Satellite",
                             options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
            addProviderTiles("OpenStreetMap",
                             group = "OSM",
                             options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
            addProviderTiles(providers$Stamen.Toner,
                             group = "Stamen",
                             options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
            addPolygons(Dat()) %>% 
            addScaleBar(position = "bottomright") %>% 
            setView(lat = 48.453, -123.340, zoom = 5) %>% 
            leaflet.extras::addResetMapButton() %>% 
            leaflet.extras::addDrawToolbar(targetLayerId = ,
                                           singleFeature = T,
                                           editOptions  = editToolbarOptions())
    })
    
    #If user uploads their own shapefile / gpkg, add to Dat and map.
    observe({
            if(length(UserPoly()) >= 1){
                if(st_geometry_type(UserPoly()) %in% c("POLYGON","MULTIPOLYGON")){
                    
                    #Add user polygon to reactive Dat object.
                    Dat = reactive({
                        Dat() %>% bind_rows(UserPoly())
                        })
                    
                    leafletProxy("leafmap") %>% 
                        clearShapes() %>% 
                        addPolygons(Dat())
                }
            }
    })
    
    #If user draws a polygon and enters info, add to Dat.
    DrawnPoly = eventReactive(input$leafmap_draw_new_feature, {
        print("New Feature")
        
        drawn_poly = as.data.frame(matrix(unlist(input$leafmap_draw_new_feature$geometry$coordinates), ncol = 2, byrow = T))
        
        names(drawn_poly) = c("lon","lat")
        
        drawn_poly = st_as_sf(drawn_poly, coords = c("lon","lat"), crs = 4326)
        
        drawn_poly = drawn_poly %>% 
            summarise(geometry = st_combine(geometry)) %>% 
            st_cast("POLYGON")
        
        drawn_poly
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
