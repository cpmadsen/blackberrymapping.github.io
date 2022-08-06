# Authors: Chris Madsen, Saeesh Mangwani
# Date: 2022-07-30

# Description: Blackberry map main

# ==== Libraries ====
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(googledrive)

# ==== Calling setup scripts ====
source('BlackberryMap/ui.R')
source('BlackberryMap/server.R')

# ==== Setting authentication options ====
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "credentials/.secrets"
)

# ==== Shiny Application ====
shinyApp(ui = ui, server = server)

