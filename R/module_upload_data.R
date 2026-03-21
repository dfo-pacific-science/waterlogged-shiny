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
      tags$div(
        class = "box box-solid box-primary collapsed-box",
        style = "border: 1px solid #ddd; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          class = "box-header with-border",
          style = "cursor: pointer; padding: 12px 15px; background-color: #f8f9fa; border-radius: 5px;",
          `data-widget` = "collapse",
          tags$h3("1. Logger Deployment Metadata", class = "custom-h2",
                   style = "display: inline; margin: 0;"),
          tags$div(class = "box-tools pull-right",
            tags$button(class = "btn btn-box-tool", `data-widget` = "collapse",
              tags$i(class = "fa fa-plus")
            )
          )
        ),
        tags$div(
          class = "box-body",
          style = "padding: 15px;",

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
              DT::DTOutput(ns("metadata_table")) %>% withSpinner()
            )
          ),

          # Full-width map below the two-column layout
          hr(),
          tags$h4("Site Locations", style = "margin-bottom: 10px; color: #333;"),
          tags$p("The map shows the locations of all monitoring stations. Click on a marker
                 to see the site code and coordinates. Use the layer control to switch basemaps.",
                 style = "font-size: 13px; color: #666; margin-bottom: 10px;"),
          leafletOutput(ns("site_map"), height = 400) %>% withSpinner()
        )
      ),

      # ============================================================
      # Section 2: Barometric Pressure
      # ============================================================
      tags$div(
        class = "box box-solid box-primary collapsed-box",
        style = "border: 1px solid #ddd; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          class = "box-header with-border",
          style = "cursor: pointer; padding: 12px 15px; background-color: #f8f9fa; border-radius: 5px;",
          `data-widget` = "collapse",
          tags$h3("2. Barometric Pressure Loggers (HOBO U20)", class = "custom-h2",
                   style = "display: inline; margin: 0;"),
          tags$div(class = "box-tools pull-right",
            tags$button(class = "btn btn-box-tool", `data-widget` = "collapse",
              tags$i(class = "fa fa-plus")
            )
          )
        ),
        tags$div(
          class = "box-body",
          style = "padding: 15px;",

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
                  tags$li(tags$strong("airpress_kPa"), " - atmospheric pressure in kilopascals"),
                  tags$li(tags$strong("airtemp_C"), " - air temperature in degrees Celsius")
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
                fileInput(ns("baro_upload"), "Upload Barometric HOBO CSV Files",
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
                choices = NULL,
                width = "250px"
              ),
              uiOutput(ns("baro_station_info")),
              DT::DTOutput(ns("baro_preview")) %>% withSpinner()
            )
          )
        )
      ),

      # ============================================================
      # Section 3: Water Level
      # ============================================================
      tags$div(
        class = "box box-solid box-primary collapsed-box",
        style = "border: 1px solid #ddd; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          class = "box-header with-border",
          style = "cursor: pointer; padding: 12px 15px; background-color: #f8f9fa; border-radius: 5px;",
          `data-widget` = "collapse",
          tags$h3("3. Water Level Loggers (HOBO U20)", class = "custom-h2",
                   style = "display: inline; margin: 0;"),
          tags$div(class = "box-tools pull-right",
            tags$button(class = "btn btn-box-tool", `data-widget` = "collapse",
              tags$i(class = "fa fa-plus")
            )
          )
        ),
        tags$div(
          class = "box-body",
          style = "padding: 15px;",

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
                  tags$li(tags$strong("waterpress_kPa"), " - absolute pressure in kilopascals"),
                  tags$li(tags$strong("watertemp_C"), " - water temperature in degrees Celsius")
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
                fileInput(ns("wl_upload"), "Upload Water Level HOBO CSV Files",
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
                choices = NULL,
                width = "250px"
              ),
              uiOutput(ns("wl_station_info")),
              DT::DTOutput(ns("wl_preview")) %>% withSpinner()
            )
          )
        )
      ),

      # ============================================================
      # Section 4: Stage Readings (Water Level)
      # ============================================================
      tags$div(
        class = "box box-solid box-primary collapsed-box",
        style = "border: 1px solid #ddd; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          class = "box-header with-border",
          style = "cursor: pointer; padding: 12px 15px; background-color: #f8f9fa; border-radius: 5px;",
          `data-widget` = "collapse",
          tags$h3("4. Stage Readings (Water Level)", class = "custom-h2",
                   style = "display: inline; margin: 0;"),
          tags$div(class = "box-tools pull-right",
            tags$button(class = "btn btn-box-tool", `data-widget` = "collapse",
              tags$i(class = "fa fa-plus")
            )
          )
        ),
        tags$div(
          class = "box-body",
          style = "padding: 15px;",

          fluidRow(
            # Left column - explanatory text
            column(
              width = 4,
              tags$div(
                style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
                tags$h4("What are stage readings?", style = "margin-top: 0;"),
                tags$p(
                  "Stage readings are manual water level measurements taken during field visits.
                  These are essential for converting raw pressure data (kPa) into water depth
                  or stage in metres."
                ),
                tags$p(
                  "At least one manual measurement is required per station to calibrate the
                  pressure-to-depth conversion. Multiple measurements improve accuracy and help
                  detect sensor drift."
                )
              ),

              tags$div(
                style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                tags$h4("Required columns:", style = "margin-top: 0;"),
                tags$ul(
                  style = "padding-left: 20px; font-size: 13px;",
                  tags$li(tags$strong("metric"), " - measurement type"),
                  tags$li(tags$strong("site_station_code"), " - station identifier"),
                  tags$li(tags$strong("site_name"), " - human-readable site name"),
                  tags$li(tags$strong("timestamp"), " - when the measurement was taken"),
                  tags$li(tags$strong("stage_m"), " or ", tags$strong("depth_m"), " - water level in metres")
                )
              )
            ),
            # Right column - upload and preview
            column(
              width = 8,
              tags$div(
                style = "max-width: 400px; margin-bottom: 20px;",
                fileInput(ns("stage_upload"), "Upload Stage Readings CSV",
                  accept = c(".csv"),
                  multiple = FALSE,
                  placeholder = "No file selected"
                )
              ),

              tags$h4("Stage Readings Preview", style = "margin-bottom: 10px;"),
              tags$p(
                "The table below shows manual water level measurements for calibrating
                the pressure-to-depth conversion.",
                style = "font-size: 13px; color: #666; margin-bottom: 15px;"
              ),
              DT::DTOutput(ns("stage_preview")) %>% withSpinner()
            )
          )
        )
      ),

      # ============================================================
      # Section 5: Dissolved Oxygen
      # ============================================================
      tags$div(
        class = "box box-solid box-primary collapsed-box",
        style = "border: 1px solid #ddd; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          class = "box-header with-border",
          style = "cursor: pointer; padding: 12px 15px; background-color: #f8f9fa; border-radius: 5px;",
          `data-widget` = "collapse",
          tags$h3("5. Dissolved Oxygen Loggers (HOBO U26)", class = "custom-h2",
                   style = "display: inline; margin: 0;"),
          tags$div(class = "box-tools pull-right",
            tags$button(class = "btn btn-box-tool", `data-widget` = "collapse",
              tags$i(class = "fa fa-plus")
            )
          )
        ),
        tags$div(
          class = "box-body",
          style = "padding: 15px;",

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
                  tags$li(tags$strong("do_mgl"), " - dissolved oxygen in mg/L"),
                  tags$li(tags$strong("watertemp_C"), " - water temperature in degrees Celsius")
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
                fileInput(ns("do_upload"), "Upload Dissolved Oxygen HOBO CSV Files",
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
                choices = NULL,
                width = "250px"
              ),
              uiOutput(ns("do_station_info")),
              DT::DTOutput(ns("do_preview")) %>% withSpinner()
            )
          )
        )
      ),

      # ============================================================
      # Section 6: Conductivity
      # ============================================================
      tags$div(
        class = "box box-solid box-primary collapsed-box",
        style = "border: 1px solid #ddd; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          class = "box-header with-border",
          style = "cursor: pointer; padding: 12px 15px; background-color: #f8f9fa; border-radius: 5px;",
          `data-widget` = "collapse",
          tags$h3("6. Conductivity Loggers (HOBO U24)", class = "custom-h2",
                   style = "display: inline; margin: 0;"),
          tags$div(class = "box-tools pull-right",
            tags$button(class = "btn btn-box-tool", `data-widget` = "collapse",
              tags$i(class = "fa fa-plus")
            )
          )
        ),
        tags$div(
          class = "box-body",
          style = "padding: 15px;",

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
                  tags$li(tags$strong("conduct_uScm"), " - conductivity in microsiemens/cm"),
                  tags$li(tags$strong("watertemp_C"), " - water temperature in degrees Celsius")
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
                fileInput(ns("cond_upload"), "Upload Conductivity HOBO CSV Files",
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
                choices = NULL,
                width = "250px"
              ),
              uiOutput(ns("cond_station_info")),
              DT::DTOutput(ns("cond_preview")) %>% withSpinner()
            )
          )
        )
      ),

      # ============================================================
      # Section 7: Field Reference Measurements (DO & Conductivity)
      # ============================================================
      tags$div(
        class = "box box-solid box-primary collapsed-box",
        style = "border: 1px solid #ddd; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          class = "box-header with-border",
          style = "cursor: pointer; padding: 12px 15px; background-color: #f8f9fa; border-radius: 5px;",
          `data-widget` = "collapse",
          tags$h3("7. Field Reference Measurements (DO & Conductivity)", class = "custom-h2",
                   style = "display: inline; margin: 0;"),
          tags$div(class = "box-tools pull-right",
            tags$button(class = "btn btn-box-tool", `data-widget` = "collapse",
              tags$i(class = "fa fa-plus")
            )
          )
        ),
        tags$div(
          class = "box-body",
          style = "padding: 15px;",

          fluidRow(
            # Left column - explanatory text
            column(
              width = 4,
              tags$div(
                style = "background-color: #fce4ec; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
                tags$h4("What are field references?", style = "margin-top: 0;"),
                tags$p(
                  "Field reference measurements are readings taken with a calibrated handheld
                  meter (e.g. YSI) during field visits. These are used for drift correction
                  of dissolved oxygen and conductivity loggers."
                ),
                tags$p(
                  "By comparing logger readings to a known-accurate handheld meter at specific
                  times, we can detect and correct sensor drift over the deployment period."
                )
              ),

              tags$div(
                style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                tags$h4("Required columns:", style = "margin-top: 0;"),
                tags$ul(
                  style = "padding-left: 20px; font-size: 13px;",
                  tags$li(tags$strong("metric"), " - measurement type (dissolved oxygen or conductivity)"),
                  tags$li(tags$strong("site_station_code"), " - station identifier"),
                  tags$li(tags$strong("timestamp"), " - when the measurement was taken"),
                  tags$li(tags$strong("watertemp_C"), " - water temperature from handheld meter"),
                  tags$li(tags$strong("value"), " - the reference measurement value"),
                  tags$li(tags$strong("unit"), " - measurement unit (mg/L, uS/cm, etc.)"),
                  tags$li(tags$strong("event_type"), " - calibration, check, or ignore")
                )
              )
            ),
            # Right column - upload and preview
            column(
              width = 8,
              tags$div(
                style = "max-width: 400px; margin-bottom: 20px;",
                fileInput(ns("fieldref_upload"), "Upload Field References CSV",
                  accept = c(".csv"),
                  multiple = FALSE,
                  placeholder = "No file selected"
                )
              ),

              tags$h4("Field References Preview", style = "margin-bottom: 10px;"),
              tags$p(
                "The table below shows handheld meter readings used for drift correction
                of dissolved oxygen and conductivity data.",
                style = "font-size: 13px; color: #666; margin-bottom: 15px;"
              ),
              DT::DTOutput(ns("fieldref_preview")) %>% withSpinner()
            )
          )
        )
      ),

      # ============================================================
      # Section 8: Level Run Benchmarks (Water Level)
      # ============================================================
      tags$div(
        class = "box box-solid box-primary collapsed-box",
        style = "border: 1px solid #ddd; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          class = "box-header with-border",
          style = "cursor: pointer; padding: 12px 15px; background-color: #f8f9fa; border-radius: 5px;",
          `data-widget` = "collapse",
          tags$h3("8. Level Run Benchmarks (Water Level)", class = "custom-h2",
                   style = "display: inline; margin: 0;"),
          tags$div(class = "box-tools pull-right",
            tags$button(class = "btn btn-box-tool", `data-widget` = "collapse",
              tags$i(class = "fa fa-plus")
            )
          )
        ),
        tags$div(
          class = "box-body",
          style = "padding: 15px;",

          fluidRow(
            # Left column - explanatory text
            column(
              width = 4,
              tags$div(
                style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
                tags$h4("What are level run benchmarks?", style = "margin-top: 0;"),
                tags$p(
                  "Level run benchmarks are survey-grade elevation measurements taken at
                  fixed points near a monitoring station. These are used to confirm that
                  the logger position has not shifted between visits."
                ),
                tags$p(
                  "By comparing benchmark elevations across visits, you can detect whether
                  the logger has been disturbed (e.g. by flooding, ice, or animal activity)
                  and correct water level data accordingly."
                )
              ),

              tags$div(
                style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                tags$h4("Required columns:", style = "margin-top: 0;"),
                tags$ul(
                  style = "padding-left: 20px; font-size: 13px;",
                  tags$li(tags$strong("site_station_code"), " - station identifier"),
                  tags$li(tags$strong("visit_date"), " - date of the survey"),
                  tags$li(tags$strong("BM1"), ", ", tags$strong("BM2"), " - benchmark elevation readings"),
                  tags$li(tags$strong("water_elev_1"), ", ", tags$strong("water_elev_2"), " - water surface elevations")
                )
              )
            ),
            # Right column - upload and preview
            column(
              width = 8,
              tags$div(
                style = "max-width: 400px; margin-bottom: 20px;",
                fileInput(ns("benchmarks_upload"), "Upload Benchmarks CSV",
                  accept = c(".csv"),
                  multiple = FALSE,
                  placeholder = "No file selected"
                )
              ),

              tags$h4("Benchmarks Preview", style = "margin-bottom: 10px;"),
              tags$p(
                "The table below shows survey-grade benchmark data for confirming
                logger position stability between visits.",
                style = "font-size: 13px; color: #666; margin-bottom: 15px;"
              ),
              DT::DTOutput(ns("benchmarks_preview")) %>% withSpinner()
            )
          )
        )
      ),

      # Next step callout
      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next Step:"),
          " After uploading your data files, proceed to Step 1: Raw Series to inspect
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

    # ============================================================
    # Dynamic station selector observers
    # ============================================================
    observe({
      baro <- session$userData$rv_raw_data$baro_data
      req(baro)
      stations <- names(baro)
      updateSelectInput(session, "baro_station_select",
        choices = stations, selected = stations[1]
      )
    })

    observe({
      wl <- session$userData$rv_raw_data$waterlevel_data
      req(wl)
      stations <- names(wl)
      updateSelectInput(session, "wl_station_select",
        choices = stations, selected = stations[1]
      )
    })

    observe({
      do_list <- session$userData$rv_raw_data$do_data
      req(do_list)
      stations <- names(do_list)
      updateSelectInput(session, "do_station_select",
        choices = stations, selected = stations[1]
      )
    })

    observe({
      cond <- session$userData$rv_raw_data$conductivity_data
      req(cond)
      stations <- names(cond)
      updateSelectInput(session, "cond_station_select",
        choices = stations, selected = stations[1]
      )
    })

    # ============================================================
    # Upload handlers
    # ============================================================

    # Metadata upload (single CSV)
    observeEvent(input$metadata_upload, {
      file <- input$metadata_upload
      req(file)
      dat <- read.csv(file$datapath)
      session$userData$rv_raw_data$metadata <- dat
    })

    # Barometric data upload (multi-file)
    observeEvent(input$baro_upload, {
      files <- input$baro_upload
      req(files)
      metadata <- session$userData$rv_raw_data$metadata
      req(metadata)

      tmp_dir <- tempdir()
      baro_dir <- file.path(tmp_dir, "baro_upload")
      dir.create(baro_dir, showWarnings = FALSE, recursive = TRUE)
      file.copy(files$datapath, file.path(baro_dir, files$name))
      meta_path <- file.path(tmp_dir, "metadata_upload.csv")
      write.csv(metadata, meta_path, row.names = FALSE)

      tryCatch({
        result <- bind_hobo_files(baro_dir, metadata_path = meta_path)
        bound_df <- result[[1]]
        bound_df$timestamp <- .parse_timestamp(bound_df$timestamp)
        station_list <- split(bound_df, bound_df$site_station_code)
        session$userData$rv_raw_data$baro_data <- as.list(station_list)
      }, error = function(e) {
        showNotification(paste("Error processing baro files:", e$message), type = "error")
      })
      unlink(baro_dir, recursive = TRUE)
      unlink(meta_path)
    })

    # Water level data upload (multi-file)
    observeEvent(input$wl_upload, {
      files <- input$wl_upload
      req(files)
      metadata <- session$userData$rv_raw_data$metadata
      req(metadata)

      tmp_dir <- tempdir()
      wl_dir <- file.path(tmp_dir, "wl_upload")
      dir.create(wl_dir, showWarnings = FALSE, recursive = TRUE)
      file.copy(files$datapath, file.path(wl_dir, files$name))
      meta_path <- file.path(tmp_dir, "metadata_upload.csv")
      write.csv(metadata, meta_path, row.names = FALSE)

      tryCatch({
        result <- bind_hobo_files(wl_dir, metadata_path = meta_path)
        bound_df <- result[[1]]
        bound_df$timestamp <- .parse_timestamp(bound_df$timestamp)
        station_list <- split(bound_df, bound_df$site_station_code)
        session$userData$rv_raw_data$waterlevel_data <- as.list(station_list)
      }, error = function(e) {
        showNotification(paste("Error processing water level files:", e$message), type = "error")
      })
      unlink(wl_dir, recursive = TRUE)
      unlink(meta_path)
    })

    # Dissolved oxygen data upload (multi-file)
    observeEvent(input$do_upload, {
      files <- input$do_upload
      req(files)
      metadata <- session$userData$rv_raw_data$metadata
      req(metadata)

      tmp_dir <- tempdir()
      do_dir <- file.path(tmp_dir, "do_upload")
      dir.create(do_dir, showWarnings = FALSE, recursive = TRUE)
      file.copy(files$datapath, file.path(do_dir, files$name))
      meta_path <- file.path(tmp_dir, "metadata_upload.csv")
      write.csv(metadata, meta_path, row.names = FALSE)

      tryCatch({
        result <- bind_hobo_files(do_dir, metadata_path = meta_path)
        bound_df <- result[[1]]
        bound_df$timestamp <- .parse_timestamp(bound_df$timestamp)
        station_list <- split(bound_df, bound_df$site_station_code)
        session$userData$rv_raw_data$do_data <- as.list(station_list)
      }, error = function(e) {
        showNotification(paste("Error processing DO files:", e$message), type = "error")
      })
      unlink(do_dir, recursive = TRUE)
      unlink(meta_path)
    })

    # Conductivity data upload (multi-file)
    observeEvent(input$cond_upload, {
      files <- input$cond_upload
      req(files)
      metadata <- session$userData$rv_raw_data$metadata
      req(metadata)

      tmp_dir <- tempdir()
      cond_dir <- file.path(tmp_dir, "cond_upload")
      dir.create(cond_dir, showWarnings = FALSE, recursive = TRUE)
      file.copy(files$datapath, file.path(cond_dir, files$name))
      meta_path <- file.path(tmp_dir, "metadata_upload.csv")
      write.csv(metadata, meta_path, row.names = FALSE)

      tryCatch({
        result <- bind_hobo_files(cond_dir, metadata_path = meta_path)
        bound_df <- result[[1]]
        bound_df$timestamp <- .parse_timestamp(bound_df$timestamp)
        station_list <- split(bound_df, bound_df$site_station_code)
        session$userData$rv_raw_data$conductivity_data <- as.list(station_list)
      }, error = function(e) {
        showNotification(paste("Error processing conductivity files:", e$message), type = "error")
      })
      unlink(cond_dir, recursive = TRUE)
      unlink(meta_path)
    })

    # Stage readings upload (single CSV)
    observeEvent(input$stage_upload, {
      req(input$stage_upload)
      session$userData$rv_raw_data$stage_readings <- read.csv(input$stage_upload$datapath)
    })

    # Field references upload (single CSV)
    observeEvent(input$fieldref_upload, {
      req(input$fieldref_upload)
      session$userData$rv_raw_data$field_references <- read.csv(input$fieldref_upload$datapath)
    })

    # Benchmarks upload (single CSV)
    observeEvent(input$benchmarks_upload, {
      req(input$benchmarks_upload)
      session$userData$rv_raw_data$benchmarks <- read.csv(input$benchmarks_upload$datapath)
    })

    # ============================================================
    # Metadata DataTable
    # ============================================================
    output$metadata_table <- DT::renderDT({
      meta <- session$userData$rv_raw_data$metadata
      req(meta)
      DT::datatable(
        meta,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 25, 50),
          scrollX = TRUE,
          dom = "ltip"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # Leaflet Map with basemap switching
    output$site_map <- renderLeaflet({
      meta <- session$userData$rv_raw_data$metadata
      req(meta)
      sites <- unique(meta[, c("site_station_code", "latitude", "longitude")])

      leaflet(sites) %>%
        addTiles(group = "OpenStreetMap") %>%
        addTiles(
          urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
          attribution = "Esri",
          group = "Esri Topo"
        ) %>%
        addTiles(
          urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
          attribution = "Esri",
          group = "Esri Imagery"
        ) %>%
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
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Esri Topo", "Esri Imagery"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(
          lng = mean(sites$longitude),
          lat = mean(sites$latitude),
          zoom = 6
        )
    })

    # ============================================================
    # Barometric data - reactive based on station selection
    # ============================================================
    baro_data_selected <- reactive({
      req(input$baro_station_select)
      req(session$userData$rv_raw_data$baro_data)
      session$userData$rv_raw_data$baro_data[[input$baro_station_select]]
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
      req(session$userData$rv_raw_data$waterlevel_data)
      session$userData$rv_raw_data$waterlevel_data[[input$wl_station_select]]
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
      req(session$userData$rv_raw_data$do_data)
      session$userData$rv_raw_data$do_data[[input$do_station_select]]
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
      req(session$userData$rv_raw_data$conductivity_data)
      session$userData$rv_raw_data$conductivity_data[[input$cond_station_select]]
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

    # ============================================================
    # Stage Readings preview
    # ============================================================
    output$stage_preview <- DT::renderDT({
      stage <- session$userData$rv_raw_data$stage_readings
      req(stage)
      DT::datatable(
        stage,
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
    # Field References preview
    # ============================================================
    output$fieldref_preview <- DT::renderDT({
      fieldref <- session$userData$rv_raw_data$field_references
      req(fieldref)
      DT::datatable(
        fieldref,
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
    # Benchmarks preview
    # ============================================================
    output$benchmarks_preview <- DT::renderDT({
      benchmarks <- session$userData$rv_raw_data$benchmarks
      req(benchmarks)
      DT::datatable(
        benchmarks,
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
