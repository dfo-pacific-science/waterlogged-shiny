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

# Robust timestamp parser: handles "2023-10-19 13:00:00", "2023-10-19 13:00"
# (no seconds), and "2023-10-19" (date-only midnight rows)
.parse_timestamp <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  x <- as.character(x)
  # Pad "YYYY-MM-DD HH:MM" to "YYYY-MM-DD HH:MM:SS"
  hhmm_only <- grepl("^\\d{4}-\\d{2}-\\d{2} \\d{1,2}:\\d{2}$", trimws(x))
  if (any(hhmm_only, na.rm = TRUE)) {
    x[hhmm_only] <- paste0(trimws(x[hhmm_only]), ":00")
  }
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

# Load waterlogged: use installed package if available, otherwise source
# from embedded copy (for shinyapps.io where the private repo is not accessible).
# To install locally: devtools::install("./waterlogged/")
if (requireNamespace("waterlogged", quietly = TRUE)) {
  library(waterlogged)
} else {
  message("waterlogged package not installed - sourcing from embedded copy...")
  for (.f in sort(list.files("waterlogged/R", full.names = TRUE, pattern = "\\.R$"))) {
    source(.f, local = globalenv())
  }
  rm(.f)
}




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

# Load temperature metadata
demo_temp_metadata <- read.csv("inst/extdata/water_temperature/metadata_wt.csv")

# Supplementary data
demo_stage_readings <- read.csv("inst/extdata/stage_readings.csv")
demo_field_references <- read.csv("inst/extdata/field_references.csv")
demo_benchmarks <- read.csv("inst/extdata/benchmarks.csv")
