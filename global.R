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

# Robust timestamp parser: handles both "2023-10-19 13:00:00" and "2023-10-19"
# (midnight rows where CSV export drops the time component)
.parse_timestamp <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  x <- as.character(x)
  parsed <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  na_idx <- is.na(parsed) & !is.na(x) & nchar(trimws(x)) > 0
  if (any(na_idx)) {
    parsed[na_idx] <- as.POSIXct(x[na_idx], format = "%Y-%m-%d", tz = "UTC")
  }
  parsed
}

# Load package dependencies for waterlogged workflows
library(dplyr) # tidyverse operations
library(tidyr) # data reshaping
library(ggplot2) # plotting
library(lubridate) # date handling
library(purrr) # functional programming
library(waterlogged)

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
  demo_baro_list[[nm]]$timestamp <- .parse_timestamp(demo_baro_list[[nm]]$timestamp)
}

# Load water level data
demo_waterlevel_list <- list(
  "ALBR_ST_30" = read.csv("inst/extdata/waterlevel_ALBR_ST_30.csv")
)
for (nm in names(demo_waterlevel_list)) {
  demo_waterlevel_list[[nm]]$timestamp <- .parse_timestamp(demo_waterlevel_list[[nm]]$timestamp)
}

# Load dissolved oxygen data
demo_dissox_list <- list(
  "COOK_WE_40" = read.csv("inst/extdata/dissox_COOK_WE_40.csv")
)
for (nm in names(demo_dissox_list)) {
  demo_dissox_list[[nm]]$timestamp <- .parse_timestamp(demo_dissox_list[[nm]]$timestamp)
}

# Load conductivity data
demo_conductivity_list <- list(
  "TUMT_WE_40" = read.csv("inst/extdata/conductivity_TUMT_WE_40.csv")
)
for (nm in names(demo_conductivity_list)) {
  demo_conductivity_list[[nm]]$timestamp <- .parse_timestamp(demo_conductivity_list[[nm]]$timestamp)
}

# Supplementary data
demo_stage_readings <- read.csv("inst/extdata/stage_readings.csv")
demo_field_references <- read.csv("inst/extdata/field_references.csv")
demo_benchmarks <- read.csv("inst/extdata/benchmarks.csv")
