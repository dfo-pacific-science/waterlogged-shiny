# global.R
# https://moot.shinyapps.io/FDLToolbox/

library(shiny) # Main shiny library
library(shinydashboard) # Dashboard multipage app
library(shinyjs) # shinyjs for working with JavaScript files
library(shinycssloaders) # loading spinner
library(shinyWidgets) # feeback toast confirmations to user
library(htmlwidgets) # Used throughout
library(stringr) # For various text manipulation
library(DT) # JS DataTables
library(plotly) # Plotly for interactive plots
library(jsonlite) # JSON format conversion
#library(shinyFeedback)


# Load all package dependencies
library(dplyr) # tidyverse operations
library(readxl) # Excel files (read)
library(writexl) # Excel files (west)


# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)
