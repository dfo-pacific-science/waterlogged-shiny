## app.R ##

# Load Libraries
# rm(list = ls()); library(shiny); runApp()

# --- TODOs: March 13, 2023 ---
# 1. Clean up page fix UI... add some nice CSS
# 2. Remove react log - turn off...


# Load R-Shiny libraries
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

# Load all package dependencies
library(dplyr) # tidyverse operations
library(readxl) # Excel files (read)
library(writexl) # Excel files (west)

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)


# ------------------------------------------------
# Setup UI function for Shiny app
ui <- dashboardPage(
  title = "Farm Development Leases (FDL) Toolbox",
  skin = "blue",
  header = dashboardHeader(
    title = "Farm Development Leases (FDL) Toolbox",
    titleWidth = 450
  ),

  # Sidebar page menu
  sidebar = page_sidebar_tab_ui("sidebar_navigation"),
  # Main body panel
  body = dashboardBody( # Boxes need to be put in a row (or column)

    # Import custom css files to html head tag
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_style.css")
    ),

    # Import custom js files - note these are used with DT
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),

    # Individual pages - right side tab content
    tabItems(
      tabItem(
        "t_overview",
        module_overview_ui("overview")
      ),
      tabItem(
        "t_excel_upload",
        module_excel_upload_ui("excel_upload")
      ),
      tabItem(
        "t_average_yields",
        module_average_yields_ui("average_yields")
      ),
      tabItem(
        "t_price_fractions",
        module_price_fractions_ui("price_fractions")
      ),
     tabItem(
        "t_lsrs",
        module_lsrs_ui("lsrs")
      ),
     tabItem(
        "t_gis",
        module_parcel_gis_ui("parcel_gis")
      ),
     tabItem(
        "t_select_crops",
        module_select_crops_ui("select_crops")
      ),
     tabItem(
        "t_ltay",
        module_ltay_ui("ltay")
      ),
      tabItem(
        "t_parcel_rates",
        module_parcel_report_ui("ltay")
      )
    )
  )
)


# ------------------------------------------------
# Setup server function for Shiny app
server <- function(input, output, session) {
  # Prepare reactive values for session and use in modules
  module_setup_reactives(session)


 
  # Load moules
  module_excel_upload_server("excel_upload")
  module_average_yields_server("average_yields")
  module_price_fractions_server("price_fractions")
  module_lsrs_server("lsrs")
  module_parcel_gis_server("parcel_gis")
  module_select_crops_server("select_crops")
  module_ltay_server("ltay")
  module_parcel_report_server("ltay")


}

shinyApp(ui, server)
