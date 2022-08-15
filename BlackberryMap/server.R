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
  
  #Read in the municipalities geopackage from the www/ folder.
  muns = read_sf('www/municipalities.shp') %>% st_transform(crs = 4326)
  
  #Create the UI selector for neighbourhoods.
  # output$neigh_sel_UI = renderUI({
  #   selectInput(inputId = "neighbourhood_selection",
  #               label = "",
  #               selected = 'All',
  #               selectize = F,
  #               choices = c('All',muns$ABRVN))
  # })
  
  Neighbourhood_Coords = reactive({
    if(input$neighbourhood_selection == "All"){
      data.frame(lat = 48.51, lon = -123.340, zoom = 9.5)
    }else{
      muns %>% 
        filter(ABRVN == input$neighbourhood_selection) %>% 
        st_centroid() %>% 
        st_coordinates() %>% 
        as_tibble() %>% 
        rename(lon = X, lat = Y) %>% 
        mutate(zoom = 11.5)
    }
  })
  
  Dat = reactive({
    dat <- suppressMessages(st_read(temp))
    return(dat)
  })
  
  # #Make UI for filtering data by location.
  # output$searchbar = renderUI({
  #   selectInput(inputId = "search_filter",
  #               label = "",
  #               multiple = T,
  #               selectize = T,
  #               selected = unique(Dat() %>% arrange(location) %>% dplyr::pull(location)),
  #               choices = unique(Dat() %>% arrange(location) %>% dplyr::pull(location)))
  # })
  
  # UserPoly = reactive({
  #   user_file = input$user_poly
  #   if(is.null(user_file)) return(NULL)
  #   #If user uploads geopackage, read in and add to dat.
  #   if(str_detect(user_file$datapath, ".gpkg")){
  #     userpoly = read_sf(user_file$datapath) %>% 
  #       st_transform(crs = 4326)
  #     
  #     userpoly
  #   }
  #   #If user uploads zipped shapefile, unzip then read in.
  #   if(str_detect(user_file$datapath, ".zip")){
  #     
  #     filelist = unzip(user_file$datapath)
  #     userpoly = read_sf(filelist[str_detect(filelist, ".shp")]) %>% 
  #       st_transform(crs = 4326)
  #     
  #     userpoly
  #   }
  # })
  
  #If user has filtered Dat(), apply and make MappingDat()
  MappingDat = reactive({
    Dat() %>% 
      filter(productivity %in% input$prod_filter)
  })
  
  my.pal = reactive({
    colorFactor(palette = c("#8d8b43",
                            "#e91434",
                            "#262d4b"),
                domain = Dat() %>% 
                  mutate(productivity = factor(productivity,
                                               levels = c("Low","Medium","High"))) %>% 
                  st_drop_geometry() %>% 
                  select(productivity) %>% 
                  distinct() %>% 
                  pull(productivity))
  })
  
  foundationalMap = reactive({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Satellite",
                       options = providerTileOptions(minZoom = 2, maxZoom = 19)) %>%
      addProviderTiles("OpenStreetMap",
                       group = "OSM",
                       options = providerTileOptions(minZoom = 2, maxZoom = 19)) %>%
      addScaleBar(position = "bottomright") %>%
      addLegend(pal = my.pal(),
                values = Dat()$productivity) %>% 
      addPolygons(data = muns,
                  fillColor = 'transparent',
                  color = "black",
                  weight = 2,
                  label = ~ABRVN,
                  opacity = 0.25,
                  group = 'Neighbourhoods'
      ) %>% 
      addPolygons(data = MappingDat(),
                  label = ~paste0(location,": ",productivity),
                  color = my.pal()(MappingDat()$productivity),
                  fillColor = my.pal()(MappingDat()$productivity),
                  layerId = 'all_patches') %>% 
      setView(lat = 48.55, -123.340, zoom = 9.5) %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet.extras::addDrawToolbar(targetLayerId = ,
                                     singleFeature = T,
                                     polylineOptions = F,
                                     circleOptions = F,
                                     markerOptions = F,
                                     rectangleOptions = F,
                                     editOptions  = editToolbarOptions()) %>% 
      hideGroup(c("Satellite")) %>% 
      addLayersControl(baseGroups = c("OSM","Satellite"),
                       overlayGroups = "Neighbourhoods",
                       options = layersControlOptions(collapsed = F))
  })
  
  output$leafmap <- renderLeaflet({ 
    foundationalMap()
    })
  
  #Reactively zoom in to selected neighbourhood.
  observe({
    leafletProxy("leafmap") %>% 
      setView(lat = Neighbourhood_Coords()$lat,
              lng = Neighbourhood_Coords()$lon,
              zoom = Neighbourhood_Coords()$zoom)
  })

  # Store the current user's view of the Leaflet map for 
  # download in a reactive expression
  user.created.map <- reactive({
    
    # call the foundational Leaflet map
    foundationalMap() %>%
      
      # store the view based on UI
      setView(lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom)
  })
  
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  output$DownloadMap <- downloadHandler(
    filename = paste0(Sys.Date(),"_blackberryMap.pdf"), 
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      htmlwidgets::saveWidget(user.created.map(), "temp.html", selfcontained = FALSE)
      #webshot::webshot("temp.html", file = file, cliprect = "viewport")
      # mapview::mapshot( x = user.created.map(), 
      #          file = file,
      #          cliprect = "viewport", # the clipping rectangle matches the height & width from the viewing port
      #          selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      #)
    } # end of content() function
  ) # end of downloadHandler() function
  
  DrawnPoly = reactive(NULL)
  
  #If user draws a polygon or a single marker and enters info, add to Dat.
  DrawnPoly = eventReactive(input$leafmap_draw_new_feature, {
    
    drawn_poly = as.data.frame(matrix(unlist(input$leafmap_draw_new_feature$geometry$coordinates), ncol = 2, byrow = T))
    
    names(drawn_poly) = c("lon","lat")
    
    drawn_poly = st_as_sf(drawn_poly, coords = c("lon","lat"), crs = 4326)
    
    #If it's a single row of coordinates, i.e. a marker, make a box around point..
    if(nrow(drawn_poly) == 1){
      
      #Convert to a projection system that uses meters.
      drawn_poly_nad83 = st_transform(drawn_poly, crs = 3005)
      
      #Snag coordinates from marker.
      lon_coord = unlist(map(drawn_poly_nad83$geometry, 1))
      lat_coord = unlist(map(drawn_poly_nad83$geometry, 2))

      #Make a 1-meter-squared box around marker.
      drawn_poly = st_as_sf(data.frame(lon = c(lon_coord-0.5, lon_coord+0.5),
                                       lat = c(lat_coord-0.5, lat_coord+0.5)),
                            coords = c("lon", "lat"), 
                            crs = 3005) %>% 
        st_bbox() %>% 
        st_as_sfc() %>% 
        st_as_sf() %>% 
        st_transform(crs = 4326) %>% 
        rename(geometry = x)
    }else{
      #And if it's a polygon, take points, combine them and cast to polygon.
      drawn_poly = drawn_poly %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
    }
    
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
                   productivity = input$drawn_patch_richness) %>% 
            st_join(muns %>% select(ABRVN) %>% rename(neighbourhood = ABRVN))
        ) %>% 
        mutate(patch_number = row_number()) %>% 
        mutate(date_added = as.Date(Sys.Date()))
      
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