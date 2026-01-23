# global.R
# Waterlogged Shiny Application

library(shiny) # Main shiny library
library(shinydashboard) # Dashboard multipage app
library(shinyjs) # shinyjs for working with JavaScript files
library(shinycssloaders) # loading spinner
library(shinyWidgets) # feedback toast confirmations to user
library(htmlwidgets) # Used throughout
library(stringr) # For various text manipulation
library(DT) # JS DataTables
library(plotly) # Plotly for interactive plots
library(leaflet) # For interactive maps

# Load package dependencies for waterlogged workflows
library(dplyr) # tidyverse operations
library(tidyr) # data reshaping
library(ggplot2) # plotting
library(lubridate) # date handling
library(purrr) # functional programming

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)

# Load demo data at startup
demo_sites <- read.csv("inst/extdata/site_station_manager.csv")
demo_metadata <- read.csv("inst/extdata/logger_manager.csv")

# Load barometric data for multiple stations
demo_baro_SLUG_ST_01 <- readRDS("inst/extdata/baro_demo.rds")
demo_baro_BUGS_WE_01 <- readRDS("inst/extdata/baro_BUGS_WE_01.rds")

# Combine into a list for easy access by station
demo_baro_list <- list(
  "SLUG_ST_01" = demo_baro_SLUG_ST_01,
  "BUGS_WE_01" = demo_baro_BUGS_WE_01
)

# Load water level data for multiple stations
demo_waterlevel_FROG_WE_01 <- readRDS("inst/extdata/waterlevel_demo.rds")
demo_waterlevel_BUGS_WE_01 <- readRDS("inst/extdata/waterlevel_BUGS_WE_01.rds")

demo_waterlevel_list <- list(
  "FROG_WE_01" = demo_waterlevel_FROG_WE_01,
  "BUGS_WE_01" = demo_waterlevel_BUGS_WE_01
)

# Load dissolved oxygen data (single station for demo)
demo_dissox_FROG_WE_01 <- read.csv("inst/extdata/dissox_demo.csv")
demo_dissox_FROG_WE_01$timestamp <- as.POSIXct(demo_dissox_FROG_WE_01$timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")

demo_dissox_list <- list(
  "FROG_WE_01" = demo_dissox_FROG_WE_01
)

# Load conductivity data (single station for demo)
demo_conductivity_BUGS_WE_01 <- read.csv("inst/extdata/conductivity_demo.csv")
demo_conductivity_BUGS_WE_01$timestamp <- as.POSIXct(demo_conductivity_BUGS_WE_01$timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")

demo_conductivity_list <- list(
  "BUGS_WE_01" = demo_conductivity_BUGS_WE_01
)

# Keep single dataframes for backwards compatibility with review modules
demo_baro <- demo_baro_SLUG_ST_01
demo_waterlevel <- demo_waterlevel_FROG_WE_01
demo_dissox <- demo_dissox_FROG_WE_01
demo_conductivity <- demo_conductivity_BUGS_WE_01
