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
demo_metadata <- read.csv("inst/extdata/metadata.csv")
demo_sites <- unique(demo_metadata[, c("site_station_code", "latitude", "longitude")])

# Load barometric data for multiple stations
demo_baro_list <- list(
  "ALBR_ST_30" = read.csv("inst/extdata/baro_ALBR_ST_30.csv"),
  "DARL_ST_30" = read.csv("inst/extdata/baro_DARL_ST_30.csv")
)
# Parse timestamps
for (nm in names(demo_baro_list)) {
  demo_baro_list[[nm]]$timestamp <- as.POSIXct(demo_baro_list[[nm]]$timestamp, tz = "UTC")
}

# Load water level data
demo_waterlevel_list <- list(
  "ALBR_ST_30" = read.csv("inst/extdata/waterlevel_ALBR_ST_30.csv")
)
for (nm in names(demo_waterlevel_list)) {
  demo_waterlevel_list[[nm]]$timestamp <- as.POSIXct(demo_waterlevel_list[[nm]]$timestamp, tz = "UTC")
}

# Load dissolved oxygen data
demo_dissox_list <- list(
  "COOK_WE_40" = read.csv("inst/extdata/dissox_COOK_WE_40.csv")
)
for (nm in names(demo_dissox_list)) {
  demo_dissox_list[[nm]]$timestamp <- as.POSIXct(demo_dissox_list[[nm]]$timestamp, tz = "UTC")
}

# Load conductivity data
demo_conductivity_list <- list(
  "TUMT_WE_40" = read.csv("inst/extdata/conductivity_TUMT_WE_40.csv")
)
for (nm in names(demo_conductivity_list)) {
  demo_conductivity_list[[nm]]$timestamp <- as.POSIXct(demo_conductivity_list[[nm]]$timestamp, tz = "UTC")
}

# Supplementary data
demo_stage_readings <- read.csv("inst/extdata/stage_readings.csv")
demo_field_references <- read.csv("inst/extdata/field_references.csv")
demo_benchmarks <- read.csv("inst/extdata/benchmarks.csv")
