# Authors: Chris Madsen, Saeesh Mangwani
# Date: 2022-07-30

# Description: Blackberry map main

# ==== Libraries ====
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(git2rdata)

# ==== Calling setup scripts ====
source('BlackberryMap/ui.R')
source('BlackberryMap/server.R')
source('BlackberryMap/create_test_data.R')

# ==== Shiny Application ====
shinyApp(ui = ui, server = server)

