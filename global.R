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
library(lubridate) # date handling

# Robust timestamp parser: handles diverse formats including ISO 8601 ("2023-10-19 13:00:00"),
# common US/European formats ("10/19/2023 13:00", "19-10-2023"), and date-only entries
# (e.g., "2023-10-19") which are automatically parsed as midnight UTC.
.parse_timestamp <- function(x, col_name = "timestamp") {
  if (inherits(x, "POSIXct")) return(x)

  x <- as.character(x)
  all_blank <- is.na(x) | trimws(x) %in% c("", "NA")

  if (all(all_blank)) {
    return(as.POSIXct(rep(NA_character_, length(x)), tz = "UTC"))
  }

  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      x,
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "mdy HMS", "mdy HM", "mdy",
        "dmy HMS", "dmy HM", "dmy"
      ),
      tz = "UTC",
      quiet = TRUE
    )
  )

  if (all(is.na(parsed[!all_blank]))) {
    stop(paste0("Could not parse dates in column '", col_name,
                "'. Please check the format (e.g., YYYY-MM-DD or MM/DD/YYYY HH:MM)."))
  }

  parsed
}


# Load package dependencies for waterlogged workflows
library(waterlogged)
library(dplyr) # tidyverse operations
library(tidyr) # data reshaping
library(ggplot2) # plotting
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
