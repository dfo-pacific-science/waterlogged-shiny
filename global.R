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
