#' Upload Data Module - UI
#' Displays metadata and data upload/preview sections with explanatory sidebar
module_upload_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,

      tags$h1("Upload Data", class = "custom-h1"),
      tags$h2("Import raw logger data and deployment metadata", class = "custom-h2"),

      # Introduction section
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 25px;",
        tags$p(
          "Before processing your data, you need to upload the raw CSV files exported from
          HOBOware along with a metadata file containing deployment information. This page
          guides you through organizing and uploading your data files."
        ),
        tags$p(
          "In most cases, you will have data from multiple loggers or downloads for a single
          station. These need to be bound together to create a single data file for each
          station. The data also needs to be trimmed to the install and removal dates in
          case loggers were activated prior to install, or if they were removed before
          downloading the data (which is typically the case)."
        )
      ),

      hr(),

      # ============================================================
      # Section 1: Metadata
      # ============================================================
      tags$h3("1. Logger Deployment Metadata", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Why do we need metadata?", style = "margin-top: 0;"),
            tags$p(
              "The metadata file tells the application which loggers were deployed where
              and when. This information is essential for:",
              style = "margin-bottom: 10px;"
            ),
            tags$ul(
              style = "padding-left: 20px; margin-bottom: 15px;",
              tags$li("Matching raw data files with deployment locations"),
              tags$li("Trimming data to actual deployment periods"),
              tags$li("Binding data from multiple downloads at the same station"),
              tags$li("Finding the nearest barometric logger for compensation")
            ),
            tags$h4("Required columns:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("sn"), " - serial number of the logger"),
              tags$li(tags$strong("site_station_code"), " - unique ID for the deployment location"),
              tags$li(tags$strong("timestamp_deployed"), " - when the logger was installed"),
              tags$li(tags$strong("timestamp_removed"), " - when the logger was retrieved"),
              tags$li(tags$strong("status"), " - logger status (logging, lost, removed)"),
              tags$li(tags$strong("latitude/longitude"), " - site coordinates (for mapping and finding nearest baro)")
            )
          )
        ),
        # Right column - upload and preview
        column(
          width = 8,
          tags$div(
            style = "max-width: 400px; margin-bottom: 20px;",
            fileInput(ns("metadata_upload"), "Upload Station Metadata CSV",
              accept = c(".csv"),
              multiple = FALSE,
              placeholder = "No file selected"
            )
          ),

          tags$h4("Deployment Records", style = "margin-bottom: 10px; color: #333;"),
          tags$p("The table below shows deployment records for each logger. Each row represents
                 a logger deployment at a specific site.", style = "font-size: 13px; color: #666; margin-bottom: 10px;"),
          DT::DTOutput(ns("metadata_table")) %>% withSpinner(),

          tags$h4("Site Locations", style = "margin-top: 25px; margin-bottom: 10px; color: #333;"),
          tags$p("The map shows the locations of all monitoring stations. Click on a marker
                 to see the site code and coordinates.", style = "font-size: 13px; color: #666; margin-bottom: 10px;"),
          leafletOutput(ns("site_map"), height = 350) %>% withSpinner()
        )
      ),

      hr(),

      # ============================================================
      # Section 2: Barometric Pressure
      # ============================================================
      tags$h3("2. Barometric Pressure Loggers (HOBO U20)", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What is barometric data?", style = "margin-top: 0;"),
            tags$p(
              "Barometric pressure data is collected from HOBO U20 loggers deployed in air
              (typically in a nearby tree or shelter). This data is essential for compensating
              water pressure readings to calculate true water depth."
            ),
            tags$p(
              "HOBO water level loggers are not vented, meaning they read both the pressure
              of the water above them AND the atmospheric pressure. To get actual water depth,
              we must subtract the atmospheric pressure from the total pressure reading."
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("Data columns:", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("timestamp"), " - date and time of reading"),
              tags$li(tags$strong("airpress_kPa_U20"), " - atmospheric pressure in kilopascals"),
              tags$li(tags$strong("airtemp_C_U20"), " - air temperature in degrees Celsius")
            ),
            tags$p(
              style = "margin-top: 15px; font-size: 13px; color: #666;",
              tags$strong("Tip:"), " We recommend that the atmospheric pressure logger be within
              30 km of the water level logger for accurate compensation."
            )
          )
        ),
        # Right column - upload and preview
        column(
          width = 8,
          tags$div(
            style = "max-width: 400px; margin-bottom: 20px;",
            fileInput(ns("baro_upload"), "Upload Barometric CSV Files",
              accept = c(".csv"),
              multiple = TRUE,
              placeholder = "No files selected"
            )
          ),

          tags$h4("Data Preview by Station", style = "margin-bottom: 10px;"),
          tags$p(
            "Select a station to preview its barometric data. Each station may have data from
            multiple logger downloads that are bound together.",
            style = "font-size: 13px; color: #666; margin-bottom: 15px;"
          ),
          selectInput(ns("baro_station_select"), "Select Station:",
            choices = c("SLUG_ST_01", "BUGS_WE_01"),
            selected = "SLUG_ST_01",
            width = "250px"
          ),
          uiOutput(ns("baro_station_info")),
          DT::DTOutput(ns("baro_preview")) %>% withSpinner()
        )
      ),

      hr(),

      # ============================================================
      # Section 3: Water Level
      # ============================================================
      tags$h3("3. Water Level Loggers (HOBO U20)", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What is water level data?", style = "margin-top: 0;"),
            tags$p(
              "Water level data is collected from HOBO U20 loggers deployed underwater.
              The logger measures absolute pressure, which includes both the water pressure
              (from the column of water above the sensor) and atmospheric pressure."
            ),
            tags$p(
              "In Step 2, we will apply barometric compensation to convert these raw pressure
              readings into actual water depth in metres. This requires at least one manual
              measurement of water level (either true depth or stage) taken during the
              deployment period."
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("Data columns:", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("timestamp"), " - date and time of reading"),
              tags$li(tags$strong("waterpress_kPa_U20"), " - absolute pressure in kilopascals"),
              tags$li(tags$strong("watertemp_C_U20"), " - water temperature in degrees Celsius")
            ),
            tags$p(
              style = "margin-top: 15px; font-size: 13px; color: #666;",
              tags$strong("Note:"), " The water pressure shown in raw data is NOT the same
              as water depth. It includes atmospheric pressure and must be corrected."
            )
          )
        ),
        # Right column - upload and preview
        column(
          width = 8,
          tags$div(
            style = "max-width: 400px; margin-bottom: 20px;",
            fileInput(ns("wl_upload"), "Upload Water Level CSV Files",
              accept = c(".csv"),
              multiple = TRUE,
              placeholder = "No files selected"
            )
          ),

          tags$h4("Data Preview by Station", style = "margin-bottom: 10px;"),
          tags$p(
            "Select a station to preview its water level data. Each station may have data from
            multiple logger downloads that are bound together.",
            style = "font-size: 13px; color: #666; margin-bottom: 15px;"
          ),
          selectInput(ns("wl_station_select"), "Select Station:",
            choices = c("FROG_WE_01", "BUGS_WE_01"),
            selected = "FROG_WE_01",
            width = "250px"
          ),
          uiOutput(ns("wl_station_info")),
          DT::DTOutput(ns("wl_preview")) %>% withSpinner()
        )
      ),

      hr(),

      # ============================================================
      # Section 4: Dissolved Oxygen
      # ============================================================
      tags$h3("4. Dissolved Oxygen Loggers (HOBO U26)", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What is dissolved oxygen data?", style = "margin-top: 0;"),
            tags$p(
              "Dissolved oxygen (DO) data is collected from HOBO U26 loggers deployed underwater.
              DO levels are critical for aquatic life and vary with temperature, time of day,
              and biological activity."
            ),
            tags$p(
              "The raw data is recorded in mg/L (milligrams per litre). In Step 2, we can
              convert this to percent air saturation using water temperature and barometric
              pressure, which makes it easier to compare across different conditions."
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("Data columns:", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("timestamp"), " - date and time of reading"),
              tags$li(tags$strong("do_mgl_U26"), " - dissolved oxygen in mg/L"),
              tags$li(tags$strong("watertemp_C_U26"), " - water temperature in degrees Celsius")
            ),
            tags$h4("Common issues:", style = "margin-top: 15px;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Gradual decline indicating biofouling"),
              tags$li("Error codes (-888.88) indicating sensor problems")
            )
          )
        ),
        # Right column - upload and preview
        column(
          width = 8,
          tags$div(
            style = "max-width: 400px; margin-bottom: 20px;",
            fileInput(ns("do_upload"), "Upload Dissolved Oxygen CSV Files",
              accept = c(".csv"),
              multiple = TRUE,
              placeholder = "No files selected"
            )
          ),

          tags$h4("Data Preview by Station", style = "margin-bottom: 10px;"),
          tags$p(
            "Select a station to preview its dissolved oxygen data.",
            style = "font-size: 13px; color: #666; margin-bottom: 15px;"
          ),
          selectInput(ns("do_station_select"), "Select Station:",
            choices = c("FROG_WE_01"),
            selected = "FROG_WE_01",
            width = "250px"
          ),
          uiOutput(ns("do_station_info")),
          DT::DTOutput(ns("do_preview")) %>% withSpinner()
        )
      ),

      hr(),

      # ============================================================
      # Section 5: Conductivity
      # ============================================================
      tags$h3("5. Conductivity Loggers (HOBO U24)", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #fff8e1; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What is conductivity data?", style = "margin-top: 0;"),
            tags$p(
              "Electrical conductivity data is collected from HOBO U24 loggers deployed underwater.
              Conductivity measures the water's ability to conduct electricity, which is related
              to the concentration of dissolved ions (salts, minerals)."
            ),
            tags$p(
              "Conductivity data requires additional processing steps beyond other logger types:
              drift correction (using field reference measurements) and temperature compensation
              to produce specific conductance values that can be compared across different
              temperatures."
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("Data columns:", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("timestamp"), " - date and time of reading"),
              tags$li(tags$strong("conduct_uScm_U24"), " - conductivity in microsiemens/cm"),
              tags$li(tags$strong("watertemp_C_U24"), " - water temperature in degrees Celsius")
            ),
            tags$h4("Common issues:", style = "margin-top: 15px;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Sudden spikes from sediment disturbance"),
              tags$li("Drops to near zero when logger is dry or frozen")
            )
          )
        ),
        # Right column - upload and preview
        column(
          width = 8,
          tags$div(
            style = "max-width: 400px; margin-bottom: 20px;",
            fileInput(ns("cond_upload"), "Upload Conductivity CSV Files",
              accept = c(".csv"),
              multiple = TRUE,
              placeholder = "No files selected"
            )
          ),

          tags$h4("Data Preview by Station", style = "margin-bottom: 10px;"),
          tags$p(
            "Select a station to preview its conductivity data.",
            style = "font-size: 13px; color: #666; margin-bottom: 15px;"
          ),
          selectInput(ns("cond_station_select"), "Select Station:",
            choices = c("BUGS_WE_01"),
            selected = "BUGS_WE_01",
            width = "250px"
          ),
          uiOutput(ns("cond_station_info")),
          DT::DTOutput(ns("cond_preview")) %>% withSpinner()
        )
      ),

      hr(),

      # Next step callout
      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next Step:"),
          " After uploading your data files, proceed to Step 1: Review Series to inspect
          each time series for data quality issues before applying conversions. You should
          look for gaps in coverage, unexpected values, and periods where loggers may have
          been dry, frozen, or disturbed.",
          style = "margin: 0;"
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

#' Upload Data Module - Server
module_upload_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Metadata DataTable
    output$metadata_table <- DT::renderDT({
      DT::datatable(
        demo_metadata,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "tip"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # Leaflet Map
    output$site_map <- renderLeaflet({
      leaflet(demo_sites) %>%
        addTiles() %>%
        addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~paste0(
            "<strong>", site_station_code, "</strong><br>",
            "Lat: ", round(latitude, 4), "<br>",
            "Lon: ", round(longitude, 4)
          ),
          label = ~site_station_code
        ) %>%
        setView(
          lng = mean(demo_sites$longitude),
          lat = mean(demo_sites$latitude),
          zoom = 6
        )
    })

    # ============================================================
    # Barometric data - reactive based on station selection
    # ============================================================
    baro_data_selected <- reactive({
      req(input$baro_station_select)
      demo_baro_list[[input$baro_station_select]]
    })

    output$baro_station_info <- renderUI({
      req(input$baro_station_select)
      data <- baro_data_selected()
      n_records <- nrow(data)
      date_range <- paste(
        format(min(data$timestamp), "%Y-%m-%d"),
        "to",
        format(max(data$timestamp), "%Y-%m-%d")
      )
      tags$div(
        style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 13px;",
          tags$strong("Station: "), input$baro_station_select, " | ",
          tags$strong("Records: "), format(n_records, big.mark = ","), " | ",
          tags$strong("Date Range: "), date_range
        )
      )
    })

    output$baro_preview <- DT::renderDT({
      DT::datatable(
        head(baro_data_selected(), 50),
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "tip"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # ============================================================
    # Water level data - reactive based on station selection
    # ============================================================
    wl_data_selected <- reactive({
      req(input$wl_station_select)
      demo_waterlevel_list[[input$wl_station_select]]
    })

    output$wl_station_info <- renderUI({
      req(input$wl_station_select)
      data <- wl_data_selected()
      n_records <- nrow(data)
      date_range <- paste(
        format(min(data$timestamp), "%Y-%m-%d"),
        "to",
        format(max(data$timestamp), "%Y-%m-%d")
      )
      tags$div(
        style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 13px;",
          tags$strong("Station: "), input$wl_station_select, " | ",
          tags$strong("Records: "), format(n_records, big.mark = ","), " | ",
          tags$strong("Date Range: "), date_range
        )
      )
    })

    output$wl_preview <- DT::renderDT({
      DT::datatable(
        head(wl_data_selected(), 50),
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "tip"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # ============================================================
    # Dissolved oxygen data - reactive based on station selection
    # ============================================================
    do_data_selected <- reactive({
      req(input$do_station_select)
      demo_dissox_list[[input$do_station_select]]
    })

    output$do_station_info <- renderUI({
      req(input$do_station_select)
      data <- do_data_selected()
      n_records <- nrow(data)
      date_range <- paste(
        format(min(data$timestamp), "%Y-%m-%d"),
        "to",
        format(max(data$timestamp), "%Y-%m-%d")
      )
      tags$div(
        style = "background-color: #fce4ec; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 13px;",
          tags$strong("Station: "), input$do_station_select, " | ",
          tags$strong("Records: "), format(n_records, big.mark = ","), " | ",
          tags$strong("Date Range: "), date_range
        )
      )
    })

    output$do_preview <- DT::renderDT({
      DT::datatable(
        head(do_data_selected(), 50),
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "tip"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # ============================================================
    # Conductivity data - reactive based on station selection
    # ============================================================
    cond_data_selected <- reactive({
      req(input$cond_station_select)
      demo_conductivity_list[[input$cond_station_select]]
    })

    output$cond_station_info <- renderUI({
      req(input$cond_station_select)
      data <- cond_data_selected()
      n_records <- nrow(data)
      date_range <- paste(
        format(min(data$timestamp), "%Y-%m-%d"),
        "to",
        format(max(data$timestamp), "%Y-%m-%d")
      )
      tags$div(
        style = "background-color: #fff8e1; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 13px;",
          tags$strong("Station: "), input$cond_station_select, " | ",
          tags$strong("Records: "), format(n_records, big.mark = ","), " | ",
          tags$strong("Date Range: "), date_range
        )
      )
    })

    output$cond_preview <- DT::renderDT({
      DT::datatable(
        head(cond_data_selected(), 50),
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "tip"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })
  })
}
