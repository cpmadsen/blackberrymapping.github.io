# Authors: Chris Madsen, Saeesh Mangwani, 
# Date: 2022-07-30

# Description: Shiny app server

# ==== Code ====

server <- function(input, output) {
  
  # Getting the url for the database file on google drive
  fid <- as_id('https://drive.google.com/file/d/1A3SQjyXHgkBRwxwGynzdSnPlY0J0eyNx/view?usp=sharing')
  # Creating a download link from the file ID
  fpath <- paste0('https://drive.google.com/uc?export=download&id=', fid)
  # Downloading into tempfile
  temp <- tempfile()
  download.file(fpath, temp)
  
  
  Dat = reactive({
    dat <- suppressMessages(st_read(temp))
    return(dat)
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

  # #Once the user presses 'add drawn poly', join this poly to the original
  # dataset.
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
    DatJoined()
  })
  #Upload data to github.
  observeEvent(input$upload_to_github, {
    
    #Update local geoJSON file.
    st_write(DatJoined(), paste0(temp, '.geojson'))
    
    #'push' updated geoJSON file to google drive.
    drive_update(fid, paste0(temp, '.geojson'))
  })
}