#' Welcome Page Module - UI
module_welcome_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Waterlogged: Water Level & Water Quality Data Processing", class = "custom-h1"),

      tags$h2("Welcome", class = "custom-h2"),
      tags$p(
        "Waterlogged is a data processing package for water data from continuous
        water data loggers. The goal of waterlogged is to streamline the process
        from raw data to clean data, document QAQC processing via R scripts, and
        do so without relying on proprietary software.",
        class = "clean-print"
      ),

      tags$p(
        "This version of the package was built for data from Onset HOBO barometric
        pressure (U20), dissolved oxygen (U26), and TidbiT loggers. Future versions
        will test this package on data from other types of loggers to make it a
        more universal package.",
        class = "clean-print"
      ),

      tags$h2("Workflow Overview", class = "custom-h2"),
      tags$p(
        "This application guides you through a step-by-step workflow for processing
        your water logger data. Each step builds on the previous one:",
        class = "clean-print"
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin: 15px 0;",
        tags$table(
          style = "width: 100%; border-collapse: collapse;",
          tags$tr(
            tags$td(style = "padding: 10px; vertical-align: top; width: 120px;", tags$strong("Prep")),
            tags$td(style = "padding: 10px;", "Download data from HOBO loggers using HOBOware software and organize your raw CSV files and metadata")
          ),
          tags$tr(style = "background-color: #fff;",
            tags$td(style = "padding: 10px; vertical-align: top;", tags$strong("Step 1")),
            tags$td(style = "padding: 10px;", "Bind logger data for each logging station - combine multiple logger files, match with metadata, and trim to deployment dates")
          ),
          tags$tr(
            tags$td(style = "padding: 10px; vertical-align: top;", tags$strong("Step 2")),
            tags$td(style = "padding: 10px;", "Convert raw measurements to usable metrics - apply barometric compensation to get water level (m) and dissolved oxygen (% saturation)")
          ),
          tags$tr(style = "background-color: #fff;",
            tags$td(style = "padding: 10px; vertical-align: top;", tags$strong("Step 3")),
            tags$td(style = "padding: 10px;", "QAQC - Quality assurance/quality control including flagging ice, dry periods, disturbances, and correcting spikes")
          ),
          tags$tr(
            tags$td(style = "padding: 10px; vertical-align: top;", tags$strong("Step 4")),
            tags$td(style = "padding: 10px;", "Save clean site data - export processed data with QAQC flags for each station")
          ),
          tags$tr(style = "background-color: #fff;",
            tags$td(style = "padding: 10px; vertical-align: top;", tags$strong("Step 5")),
            tags$td(style = "padding: 10px;", "Summarize data - generate daily, monthly, and annual summaries and visualizations")
          )
        )
      ),

      tags$h2("Supported Logger Types", class = "custom-h2"),
      tags$p(
        "The waterlogged package currently supports the following Onset HOBO data logger types:",
        class = "clean-print"
      ),
      tags$div(
        class = "clean-print",
        tags$ul(
          tags$li(tags$strong("HOBO U20 Water Level Logger (waterlevel_U20)"), " - Measures absolute pressure and temperature for calculating water level"),
          tags$li(tags$strong("HOBO U20 Barometric Pressure Logger (baro_U20)"), " - Measures atmospheric pressure for barometric compensation"),
          tags$li(tags$strong("HOBO U26 Dissolved Oxygen Logger (dissolvedoxygen_U26)"), " - Measures dissolved oxygen (mg/L) and water temperature"),
          tags$li(tags$strong("HOBO U24 Conductivity Logger (conductivity_U24)"), " - Measures electrical conductivity"),
          tags$li(tags$strong("HOBO TidbiT Temperature Logger (watertemp_TidbiT, airtemp_TidbiT)"), " - Measures water or air temperature")
        )
      ),

      tags$h2("Key Functions", class = "custom-h2"),
      tags$p(
        "The waterlogged R package provides the following key functions used in this workflow:",
        class = "clean-print"
      ),

      tags$div(
        class = "clean-print",
        style = "font-family: monospace; background-color: #f5f5f5; padding: 15px; border-radius: 5px;",
        tags$p(tags$strong("Data Binding:"), " bind_hobo_files()"),
        tags$p(tags$strong("Barometric Compensation:"), " add_nearest_baro()"),
        tags$p(tags$strong("Unit Conversion:"), " convert_waterlevel_kPa_m(), convert_do_mgl_percsat()"),
        tags$p(tags$strong("QAQC Functions:"), " waterlevel_qaqc(), dissox_qaqc()"),
        tags$p(tags$strong("Adjustments:"), " adjust_waterlevel_spike(), adjust_waterlevel_zero(), adjust_waterlevel_cablebreak()"),
        tags$p(tags$strong("Data Export:"), " write_clean_data(), get_logger_data()")
      ),

      tags$h2("Getting Started", class = "custom-h2"),
      tags$p(
        "To begin processing your data:",
        class = "clean-print"
      ),
      tags$div(
        class = "clean-print",
        tags$ol(
          tags$li("Navigate to the ", tags$strong("Upload Data"), " tab to learn about data requirements"),
          tags$li("Follow each step in sequence using the sidebar navigation"),
          tags$li("Each step includes detailed instructions and example R code"),
          tags$li("Use the interactive tools to process and visualize your data")
        )
      ),

      tags$h2("Authors & Contributors", class = "custom-h2"),
      tags$p(
        "Department of Fisheries and Oceans, Canada",
        class = "clean-print",
        style = "font-weight: bold; font-size: 14px; margin-bottom: 15px;"
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin: 15px 0;",

        tags$h4("Authors", style = "margin-top: 0; color: #00AFDB;"),
        tags$div(
          style = "margin-left: 15px; margin-bottom: 15px;",
          tags$p(style = "margin: 5px 0;",
            tags$strong("Daniella Lo"),
            tags$span(" (", tags$a(href = "https://github.com/daniella-lo", "@daniella-lo", target = "_blank"), ")", style = "color: #666;")
          ),
          tags$p(style = "margin: 5px 0;",
            tags$strong("Amanda Martens"),
            tags$span(" (", tags$a(href = "https://github.com/amandamartens", "@amandamartens", target = "_blank"), ")", style = "color: #666;")
          ),
          tags$p(style = "margin: 5px 0;",
            tags$strong("Julian Gan"),
            tags$span(" (", tags$a(href = "https://github.com/jclgan", "@jclgan", target = "_blank"), ")", style = "color: #666;")
          )
        ),

        tags$h4("Coding Development & Support", style = "color: #00AFDB;"),
        tags$div(
          style = "margin-left: 15px; margin-bottom: 15px;",
          tags$p(style = "margin: 5px 0;", tags$strong("Paula Soto"))
        ),

        tags$h4("Technical Support", style = "color: #00AFDB;"),
        tags$div(
          style = "margin-left: 15px;",
          tags$p(style = "margin: 5px 0;",
            tags$strong("Frank Poulsen"),
            tags$span(" - Ashlu Environmental", style = "color: #666;")
          ),
          tags$p(style = "margin: 5px 0;",
            tags$strong("Matthew Bayly"),
            tags$span(" - M.J. Bayly Analytics Ltd.", style = "color: #666;")
          )
        )
      ),

      tags$br(),

      tags$div(
        class = "img-fit container",
        tags$img(
          src = "./img/dfo_logo.png",
          width = "80%",
          style = "max-width: 250px; margin: 15px"
        )
      )
    ),

    fluidRow(column(
      width = 12,
      tags$p(
        "Waterlogged Water Data Processing Application",
        style = "text-align: center; color: #8A939A; margin-top: -15px; font-size: 12px;"
      )
    ))
  )
}

#' Welcome Page Module - Server
module_welcome_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Welcome page is static, no server logic needed for wireframe
  })
}
