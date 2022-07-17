library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(git2rdata)

# dat = data.frame(location = "Sidney Airport - Stirling Way",
#            productivity = "High")
# 
# dat$patch_number = nrow(dat)
# 
# dat = st_set_geometry(dat, st_geometry(st_cast(c(st_point(c(48.64726984376748, -123.40917029173556)),
#   st_point(c(48.64734072836236, -123.40892352850621)),
#   st_point(c(48.6468516226296, -123.40661682875333)),
#   st_point(c(48.64527794591409, -123.40584435255703)),
#   st_point(c(48.64468957669249, -123.40657391340909)),
#   st_point(c(48.64468248785449, -123.40792574675264)),
#   st_point(c(48.64599325820387, -123.40891219680323))), "POLYGON")))
# 
# dat = st_set_crs(dat, value = 4326)
# 
# my_geom = st_as_text(st_sfc(dat$geometry)) #Get geometry as text.
# my_fields = st_drop_geometry(dat)
# dat_to_write = cbind(my_fields, my_geom)
# dat_to_write = dat_to_write %>% 
#     mutate(patch_number = row_number())
# 
# repo <- repository("F:/R Projects/blackberrymapping.github.io")
# write_vc(dat_to_write, file = "blackberry_patches", root = repo, stage = TRUE)
# commit(repo, message = paste0("Updated data on ",Sys.time()))
# push(repo,credentials = git2r::cred_user_pass('cpmadsen', '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'))

# write_sf(dat, "blackberry_patches.gpkg")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Blackberry Map"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Search for a patch"),
            uiOutput('searchbar'),
            h3("Have a blackberry patch \n you'd like to add?"),
            h4("Find and draw it on the map, \nname it and classify it, \nthen click Add Patch!"),
            h3("Describe New Patch"),
            textInput(inputId = "drawn_patch_name",
                      label = "Patch Name"),
            selectizeInput(inputId = "drawn_patch_richness",
                           label = "Patch Productivity",
                           choices = c("Low","Medium","High")),
            actionButton("add_drawn_poly", "Add drawn patch to data"),
            tableOutput('submission_error_message'),
            actionButton('upload_to_github',"Upload Dataset to Github"),
            # h3("Or, upload your polygon!"),
            # fileInput("user_poly", "Upload Polygon (geopackage or zipped shapefile)",
            #           multiple = T, accept = c(".gpkg")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("leafmap"),
           dataTableOutput('data_as_table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    repo <- repository("F:/R Projects/blackberrymapping.github.io")
    
    Dat = reactive({
        #Reading data from github.
        dat = read_vc(file = "blackberry_patches", root = repo)
        
        #Wrangle text column of geometry into sf geometry column.
        dat = dat %>% 
            mutate(coords = list(paste0(
                str_extract_all(strsplit(my_geom, ", ")[[1]],"^[0-9]*\\.[0-9]*"),
                ", ",
                str_extract_all(strsplit(my_geom, ", ")[[1]],"(-)?[0-9]*\\.[0-9]*$")))) %>% 
            select(-my_geom) %>%
            unnest_longer(col = c("coords")) %>% 
            mutate(lat = str_extract(coords, ".*(?=,)"),
                   lon = str_extract(coords, "(?<=,).*")) %>% 
            group_by(patch_number) %>% 
            mutate(lat = replace(lat, lat == "character(0)", last(lat)),
                   lon = replace(lon, lon == " character(0)", dplyr::first(lon))) %>% 
            select(-coords) %>% 
            mutate(lat = as.numeric(lat),
                   lon = as.numeric(lon))
        
        #Convert to sf object!
        dat = st_as_sf(dat,
                            coords = c("lon","lat"), 
                            crs = 4326) %>%
            group_by(location, productivity, patch_number) %>%  
            summarise() %>% 
            st_convex_hull()
        
        dat
    })
    
    #Make UI for filtering data by location.
    output$searchbar = renderUI({
        selectInput(inputId = "search_filter",
                    label = "",
                    multiple = T,
                    selectize = T,
                    selected = unique(Dat() %>% arrange(location) %>% dplyr::pull(location)),
                    choices = unique(Dat() %>% arrange(location) %>% dplyr::pull(location)))
    })
    
    #If user has filtered Dat(), apply and make MappingDat()
    MappingDat = reactive({
        Dat() %>% 
            filter(location %in% input$search_filter)
    })
    
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
                             options = providerTileOptions(minZoom = 2, maxZoom = 19)) %>%
            addProviderTiles("OpenStreetMap",
                             group = "OSM",
                             options = providerTileOptions(minZoom = 2, maxZoom = 19)) %>%
            addScaleBar(position = "bottomright") %>%
            setView(lat = 48.55, -123.340, zoom = 9.5) %>%
            leaflet.extras::addResetMapButton() %>%
            leaflet.extras::addDrawToolbar(targetLayerId = ,
                                           singleFeature = T,
                                           editOptions  = editToolbarOptions()) %>% 
            hideGroup(c("Satellite")) %>% 
            addLayersControl(baseGroups = c("OSM","Satellite"),
                             options = layersControlOptions(collapsed = F))
    })
    
    observe({
        leafletProxy("leafmap") %>% 
            clearShapes() %>% 
            addPolygons(data = MappingDat(),
                        label = ~paste0(location,": ",productivity)
                        )
    })
    # 
    # #If user uploads their own shapefile / gpkg, add to Dat and map.
    # observe({
    #         if(length(UserPoly()) >= 1){
    #             if(st_geometry_type(UserPoly()) %in% c("POLYGON","MULTIPOLYGON")){
    #                 
    #                 #Add user polygon to reactive Dat object.
    #                 Dat = reactive({
    #                     Dat() %>% bind_rows(UserPoly())
    #                     })
    #                 
    #                 leafletProxy("leafmap") %>% 
    #                     clearShapes() %>% 
    #                     addPolygons(Dat())
    #             }
    #         }
    # })
    # 
    
    DrawnPoly = reactive(NULL)
    
    #If user draws a polygon and enters info, add to Dat.
    DrawnPoly = eventReactive(input$leafmap_draw_new_feature, {
        
        drawn_poly = as.data.frame(matrix(unlist(input$leafmap_draw_new_feature$geometry$coordinates), ncol = 2, byrow = T))

        names(drawn_poly) = c("lon","lat")

        drawn_poly = st_as_sf(drawn_poly, coords = c("lon","lat"), crs = 4326)

        drawn_poly = drawn_poly %>%
            summarise(geometry = st_combine(geometry)) %>%
            st_cast("POLYGON")

        drawn_poly
    })
    # 
    # #If the user presses add_drawn_poly button, write data to github.
    # output$submission_error_message = renderText(paste(str_count(input$drawn_patch_name,"[a-zA-Z]")))
    # output$submission_error_message = renderTable({
    #     Dat() %>% 
    #         bind_rows(
    #             DrawnPoly() %>% 
    #                 mutate(location = "Test",
    #                        productivity = "Low")
    #         )
    # })
    
    # #Once the user presses 'add drawn poly', join this poly to the original dataset.
    DatJoined = eventReactive(input$add_drawn_poly, {

        # # # # # # # # # # # #
        #   POSSIBLE ERRORS   #
        # # # # # # # # # # # #
        #Reset warning messages, if they have been triggered.
        output$submission_error_message = NULL

        #If the user has not drawn a polygon, ask them to do so.
        if(is.null(input$leafmap_draw_new_feature)){
            output$submission_error_message = renderText("<span style=color:red>Please draw patch on map</span>")
            return(Dat())
        }
        #If the user has not named the patch yet, update the name input box.
        if(str_count(input$drawn_patch_name,"[a-zA-Z]") == 0){
            # updateTextInput(inputId = 'drawn_patch_name',
            #                           placeholder = HTML("<span style=color:red>Please name patch</span>"))
            output$submission_error_message = renderText("<span style=color:red>Please name patch</span>")
            return(Dat())
        }
        else{
            #Combine drawn poly with original dataset.
            dat_with_new_row = Dat() %>%
                bind_rows(
                    DrawnPoly() %>%
                        mutate(location = input$drawn_patch_name,
                               productivity = input$drawn_patch_richness)
                ) %>% 
                mutate(patch_number = row_number())

            return(dat_with_new_row)
        }
    })
    
    output$data_as_table = renderDataTable({
        DatJoined() %>% st_drop_geometry()
    })
    #Upload data to github.
    observeEvent(input$upload_to_github, {
        
        my_geom = st_as_text(st_sfc(DatJoined()$my_geom)) #Get geometry as text.
        my_fields = st_drop_geometry(DatJoined())
        dat_to_write = cbind(my_fields, my_geom)
        dat_to_write = dat_to_write %>% 
            mutate(patch_number = row_number())
        
        write_vc(dat_to_write, file = "blackberry_patches", root = repo, stage = TRUE)
        commit(repo, all = T, message = paste0("Updated data on ",Sys.time()))
        push(repo,credentials = git2r::cred_user_pass('cpmadsen', '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'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
