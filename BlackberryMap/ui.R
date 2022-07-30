# Authors: Chris Madsen, Saeesh Mangwani
# Date: 2022-07-30

# Description: Shiny app UI

# ==== Code ====

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

