# Authors: Chris Madsen, Saeesh Mangwani
# Date: 2022-07-30

# Description: Shiny app UI

# ==== Code ====

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #theme = bslib::bs_theme(bootswatch = 'lux'), # add theme
  
  # Application title
  titlePanel("Blackberry Map"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Search Patches",
                 h3("Filter patches based on productivity"),
                 selectizeInput(inputId = 'prod_filter',
                                label = "",
                                choices = c("High","Medium","Low"),
                                selected = c("High","Medium","Low"),
                                multiple = T
                 ),
                 h3("Zoom to Municipality"),
                 selectInput(inputId = "neighbourhood_selection",
                             label = "",
                             selected = 'All',
                             selectize = F,
                             choices = c('All',muns$ABRVN))
                 #uiOutput('searchbar'),
        ),
        tabPanel("Add Patch to Database",
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
                 actionButton('upload_to_github',"Upload Dataset to Github")
        )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("leafmap"),
      downloadButton('DownloadMap'),
      dataTableOutput('data_as_table')
    )
  )
)

