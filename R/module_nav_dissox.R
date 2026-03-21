# =============================================================================
# Dissolved Oxygen Workflow Modules
# =============================================================================
#
# Pipeline for HOBO U26 dissolved oxygen loggers:
#   1. Import Data          — bind_hobo_files() + resample_timestamps()
#   2. Convert to % Sat.    — add_nearest_baro() + convert_do_mgl_percsat()
#   3. QC Dissolved Oxygen  — dissox_qaqc() + manual adjustments + export
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Import Data
# -----------------------------------------------------------------------------
module_do_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # CSS keyframes for processing spinner animation
    tags$style(HTML("
      @keyframes do-spin {
        0%   { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    ")),
    shinydashboard::box(
      width = 12,
      tags$h1("Import Dissolved Oxygen Data", class = "custom-h1"),
      tags$h2("Upload raw HOBO U26 DO logger files and align timestamps",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Import raw HOBO CSV files from dissolved oxygen loggers ",
        "(HOBO U26). These loggers record DO concentration (mg/L) and water ",
        "temperature in \u00b0C. The raw data columns are ",
        tags$code("do_mgl"), " and ", tags$code("watertemp_C"), ". ",
        "Barometric pressure is needed for the subsequent % saturation ",
        "conversion step."
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(style = "margin: 0;",
          tags$strong("Important:"), " Barometric pressure data must be processed ",
          "and QC'd ", tags$strong("before"), " dissolved oxygen conversion. If you ",
          "haven't done so yet, complete the ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_baro_upload']\").tab('show'); return false;",
            style = "color: #856404; font-weight: bold;",
            "Barometric Pressure"
          ),
          " workflow first."
        )
      ),

      hr(),

      # ---- Data status ----
      uiOutput(ns("data_status")),

      hr(),

      # ================================================================
      # Row 1: Metadata & Station Locations
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Metadata & Station Locations"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Metadata instructions ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px; flex: 1;",
            tags$h4("Metadata File", style = "margin-top: 0;"),
            tags$p(
              "The metadata file (", tags$code("logger_manager.csv"), ") describes ",
              "each logger deployment. It links logger serial numbers to station ",
              "codes, coordinates, and deployment periods."
            ),
            tags$p(style = "font-weight: bold; margin-bottom: 5px;",
              "Required columns:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$code("site_station_code"), " \u2014 station identifier"),
              tags$li(tags$code("timestamp_deploy"), " / ", tags$code("timestamp_remove"),
                      " \u2014 date-time in ", tags$code("MM/DD/YYYY HH:MM"), " format",
                      " (e.g. ", tags$code("05/15/2025 14:30"), ").",
                      " Also accepts ", tags$code("YYYY-MM-DD HH:MM"), ".",
                      " ", tags$code("timestamp_remove"), " may be ", tags$code("NA"),
                      " for loggers still deployed."),
              tags$li(tags$code("model"), " \u2014 logger model (e.g. HOBO_U26)"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 ",
                      tags$strong("must"), " be ", tags$code("\"dissolvedoxygen\""),
                      " or ", tags$code("\"barometric\""),
                      " \u2014 this determines how the logger data is classified and processed"),
              tags$li(tags$code("status"), " \u2014 deployment status"),
              tags$li(tags$code("latitude"), " / ", tags$code("longitude"),
                      " \u2014 decimal degrees",
                      " (e.g. ", tags$code("49.2827"), ", ", tags$code("-123.1207"), ")")
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload your own metadata CSV, or use the pre-loaded sample. ",
              "The file must contain one row per deployment period per logger. ",
              "Include metadata for barometric stations as well, so they can be ",
              "matched to DO stations."
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              tags$a(
                href = "sample_metadata.csv",
                download = "sample_metadata.csv",
                icon("download"),
                " Download sample metadata (pre-loaded demo)",
                style = "color: #1565c0;"
              )
            )
          )
        ),

        # ---- Right: Metadata upload + Station locations ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px;",

          # Metadata upload
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0;",
            tags$h4("Upload Metadata", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload a ", tags$code("logger_manager.csv"), " with deployment ",
              "metadata, or use the pre-loaded sample."
            ),
            fileInput(ns("meta_file"), NULL,
              accept = ".csv",
              placeholder = "Choose metadata CSV...",
              width = "100%"
            )
          ),

          # Station Locations — tabbed map/table
          tags$div(
            style = "margin-bottom: 15px;",
            tags$h4("Station Locations", style = "margin-top: 0;"),
            tabsetPanel(
              id = ns("station_tabs"),
              tabPanel("Map",
                tags$div(
                  style = "padding-top: 10px;",
                  leafletOutput(ns("station_map"), height = "560px")
                )
              ),
              tabPanel("Table",
                tags$div(
                  style = "padding-top: 10px;",
                  DT::DTOutput(ns("station_table"))
                )
              ),
              tabPanel("Timeline",
                tags$div(
                  style = "padding-top: 10px;",
                  uiOutput(ns("deployment_timeline_ui"))
                )
              )
            )
          )
        )
      ),

      # ================================================================
      # Row 2: Raw DO Logger Data
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Raw Dissolved Oxygen Logger Data"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Instructions (wide) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("How to Import", style = "margin-top: 0;"),
            tags$p(
              "Upload the raw CSV files exported from HOBOware for your ",
              "dissolved oxygen loggers. You can select multiple files at once. ",
              "The CSV must contain a ", tags$code("Date Time"), " column in ",
              "HOBOware's default export format: ",
              tags$code("MM/DD/YYYY HH:MM:SS AM/PM"),
              " (e.g. ", tags$code("06/01/2025 02:30:00 PM"), ")."
            ),
            tags$ol(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Ensure metadata is loaded (above) so logger serial ",
                      "numbers can be matched to stations."),
              tags$li("Select one or more raw HOBO U26 CSV files."),
              tags$li("Click ", tags$strong("Bind & Import Files"), " to parse ",
                      "and combine the data."),
              tags$li("Optionally use ", tags$strong("Snap Timestamps"), " to ",
                      "align readings to a regular interval (e.g. 15 min).")
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "If no files are uploaded, the app will use pre-loaded sample ",
              "data for demonstration."
            ),
            tags$p(style = "font-size: 13px; margin-top: 10px;",
              tags$a(
                href = "#", `data-toggle` = "modal",
                `data-target` = "#hoboware_export_modal",
                style = "color: #1565c0;",
                icon("question-circle"),
                " How to export CSV files from HOBOware"
              )
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              tags$a(
                href = "sample_do_raw.csv",
                download = "sample_do_raw.csv",
                icon("download"),
                " Download sample DO logger CSV (pre-loaded demo)",
                style = "color: #1565c0;"
              )
            )
          ),

          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666;",
              "This step calls the following function from the ",
              tags$code("waterlogged"), " package:"
            ),
            tags$code("bind_hobo_files()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Parse raw HOBO CSVs, match serial numbers to metadata")
          ),

          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Columns Produced", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px; margin-bottom: 0;",
              tags$li(tags$code("site_station_code"), " \u2014 station ID",
                      " (e.g. ", tags$code("COOK_WE_40"), ")"),
              tags$li(tags$code("timestamp"), " \u2014 date-time in ",
                      tags$code("YYYY-MM-DD HH:MM:SS"), " format",
                      " (e.g. ", tags$code("2025-06-01 14:30:00"), ")"),
              tags$li(tags$code("do_mgl"), " \u2014 dissolved oxygen in mg/L",
                      " (e.g. ", tags$code("8.42"), ")"),
              tags$li(tags$code("watertemp_C"), " \u2014 water temperature in \u00b0C",
                      " (e.g. ", tags$code("14.7"), ")"),
              tags$li(tags$code("sn"), " \u2014 logger serial number",
                      " (e.g. ", tags$code("21473863"), ")"),
              tags$li(tags$code("metric"), " \u2014 ", tags$code("\"dissolvedoxygen\""))
            )
          )
        ),

        # ---- Right: File upload (narrow) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",

          # Raw HOBO files upload
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0; flex: 1;",
            tags$h4("Raw DO Logger Files", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload one or more raw HOBO U26 CSV files, or use the ",
              "pre-loaded sample data."
            ),
            fileInput(ns("raw_files"), NULL,
              accept = ".csv",
              multiple = TRUE,
              placeholder = "Choose HOBO U26 CSV files...",
              width = "100%"
            ),
            actionButton(ns("bind_btn"), "Bind & Import Files",
              class = "btn-primary",
              icon = icon("file-import"),
              width = "100%",
              style = "color: #ffffff;"
            ),
            # Processing spinner (hidden by default, shown while binding)
            tags$div(
              id = ns("bind_spinner"),
              style = "display: none; text-align: center; padding: 15px; margin-top: 10px; background-color: #e3f2fd; border-radius: 5px;",
              tags$div(
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #90caf9; border-top: 3px solid #1565c0; border-radius: 50%; animation: do-spin 0.8s linear infinite; vertical-align: middle;"
              ),
              tags$span(style = "margin-left: 10px; color: #1565c0; font-weight: bold;",
                "Processing... binding HOBO files")
            ),
            uiOutput(ns("bind_summary"))
          )
        )
      ),

      # ================================================================
      # Row 3: Processed Barometric Data
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Processed Barometric Data"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Baro info ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px; flex: 1;",
            tags$h4("Processed Baro File", style = "margin-top: 0;"),
            tags$p(
              "This file is the output of the Barometric Pressure QAQC workflow. ",
              "You must run through the ",
              tags$a(
                href = "#",
                onclick = "$(\"a[href='#shiny-tab-t_baro_upload']\").tab('show'); return false;",
                style = "color: #1565c0; font-weight: bold;",
                "Barometric Pressure"
              ),
              " section first, or upload a previously exported processed baro CSV."
            ),
            tags$p(style = "font-weight: bold; margin-bottom: 5px;",
              "Required columns:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$code("site_station_code"),
                      " (e.g. ", tags$code("ALBR_ST_30"), ")"),
              tags$li(tags$code("timestamp"), " \u2014 ",
                      tags$code("YYYY-MM-DD HH:MM:SS"), " format",
                      " (e.g. ", tags$code("2025-06-01 14:30:00"), ")"),
              tags$li(tags$code("airpress_kPa"),
                      " \u2014 raw pressure in kPa"),
              tags$li(tags$code("airpress_kPa_adj"),
                      " \u2014 QC'd pressure in kPa"),
              tags$li(tags$code("airtemp_C"),
                      " \u2014 raw air temp in \u00b0C"),
              tags$li(tags$code("airtemp_C_adj"),
                      " \u2014 QC'd air temp in \u00b0C"),
              tags$li("Plus any ", tags$code("edit_*"), " / ", tags$code("flag_*"),
                      " columns (", tags$code("TRUE"), "/", tags$code("FALSE"), ")")
            ),
            tags$div(
              style = "background-color: #e8e8e8; padding: 10px; border-radius: 4px; margin-top: 10px;",
              tags$h5("Follow Along in the R package", style = "margin-top: 0; margin-bottom: 5px;"),
              tags$code("barometric_qaqc_all()"),
              tags$p(style = "font-size: 12px; color: #666; margin-top: 5px; margin-bottom: 0;",
                     "Run automated QAQC on baro data (produces the required columns)")
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              tags$a(
                href = "demo_baro_processed.csv",
                download = "demo_baro_processed.csv",
                style = "color: #1565c0;",
                icon("download"),
                " Download sample processed baro CSV"
              )
            )
          )
        ),

        # ---- Right: Baro upload ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px;",

          # Status message
          uiOutput(ns("baro_status_message")),

          # File upload
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0;",
            tags$h4("Upload Processed Barometric CSV", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload a processed baro CSV file (output of baro QAQC), or use ",
              "session data from the Barometric Pressure tab if available."
            ),
            fileInput(ns("baro_qc_file"), NULL,
              accept = ".csv",
              placeholder = "Choose processed baro CSV...",
              width = "100%"
            ),
            actionButton(ns("load_baro_btn"), "Load Barometric Data (wait until above section says Upload Complete before pressing this button)",
              class = "btn-primary",
              icon = icon("cloud-download-alt"),
              width = "100%",
              style = "color: #ffffff;"
            ),
            # Processing spinner (hidden by default, shown while loading)
            tags$div(
              id = ns("baro_spinner"),
              style = "display: none; text-align: center; padding: 15px; margin-top: 10px; background-color: #e3f2fd; border-radius: 5px;",
              tags$div(
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #90caf9; border-top: 3px solid #1565c0; border-radius: 50%; animation: do-spin 0.8s linear infinite; vertical-align: middle;"
              ),
              tags$span(style = "margin-left: 10px; color: #1565c0; font-weight: bold;",
                "Processing... loading barometric data")
            )
          ),

          # Validation results
          uiOutput(ns("baro_validation"))
        )
      ),

      # ================================================================
      # Row 4: Snap Timestamps
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Snap Timestamps: Dissolved Oxygen and Air Pressure"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Snap explanation ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Timestamp Alignment", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "Round timestamps to a regular interval so dissolved oxygen and ",
              "barometric stations align. Original timestamps are preserved in ",
              tags$code("timestamp_raw"), "."
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "This is optional but recommended when combining data from ",
              "multiple loggers that may have been launched at slightly ",
              "different times."
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("resample_timestamps()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Snap timestamps to regular grid (optional)")
          )
        ),

        # ---- Right: Snap controls ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex;",
          tags$div(
            style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin-bottom: 15px; flex: 1;",
            tags$h4("Snap Logger Timestamps (Optional)", style = "margin-top: 0;"),
            fluidRow(
              column(4,
                selectInput(ns("snap_station"), "Station",
                  choices = c("All Stations" = "all"),
                  width = "100%"
                )
              ),
              column(4,
                selectInput(ns("snap_interval"), "Interval",
                  choices = c("5 min", "10 min", "15 min", "30 min", "1 hour"),
                  selected = "15 min",
                  width = "100%"
                )
              ),
              column(4,
                selectInput(ns("snap_align"), "Alignment",
                  choices = c("round", "floor", "ceiling"),
                  selected = "round",
                  width = "100%"
                )
              )
            ),
            actionButton(ns("snap_btn"), "Snap Timestamps",
              class = "btn-secondary",
              icon = icon("clock"),
              width = "100%"
            )
          )
        )
      ),

      # ================================================================
      # Row 5: Review Raw Series
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Review Raw Series"
        )
      ),

      tags$p(
        class = "clean-print",
        "Visually inspect the time series plots and summary statistics below ",
        "to look for anomalies such as spikes, flatlines, or gaps in the data. ",
        "The top two panels show dissolved oxygen data; the bottom two show barometric ",
        "data (if loaded)."
      ),

      # ---- Import Summary (full width) ----
      tags$h4("Import Summary"),
      DT::DTOutput(ns("import_summary")),

      tags$div(style = "margin-top: 15px;"),

      # ---- Preview Plot (full width, 4 panels) ----
      tags$h4("Data Preview"),
      plotlyOutput(ns("preview_plot"), height = "1065px") %>% withSpinner(),

      # ---- (Optional) Filter Data by Date Range ----
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "(Optional) Filter Data by Date Range"
        )
      ),

      tags$p(
        class = "clean-print",
        "If your data contains records outside the period of interest (e.g., pre-deployment ",
        "or post-retrieval), you can filter to a specific date range. This will remove records ",
        "outside the selected range from all time series loaded in this section."
      ),

      fluidRow(
        column(4, dateInput(ns("filter_from"), "From Date:", value = NULL)),
        column(4, dateInput(ns("filter_to"),   "To Date:",   value = NULL)),
        column(4, tags$div(
          style = "margin-top: 25px;",
          actionButton(ns("apply_filter"), "Apply Filter",
                       class = "btn-primary", icon = icon("filter"),
                       style = "color: #fff; background-color: #1565c0; border-color: #1565c0;")
        ))
      ),

      uiOutput(ns("filter_status")),

      # ---- Next step link ----
      tags$div(
        style = "margin-top: 25px; padding: 15px; background-color: #e3f2fd; border-radius: 5px; text-align: center;",
        tags$p(style = "margin: 0; font-size: 14px;",
          icon("arrow-right"),
          " Ready to proceed? Continue to ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_do_convert']\").tab('show'); window.scrollTo(0,0); return false;",
            style = "color: #1565c0; font-weight: bold;",
            "Convert to % Saturation"
          ),
          " to add barometric pressure and convert DO units."
        )
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 1. Import Data — Server
# =============================================================================
module_do_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data   <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data
    rv_qaqc_data  <- session$userData$rv_qaqc_data

    # ------ Initialize from demo data ------
    observe({
      data_list <- rv_raw_data$do_data
      if (!is.null(data_list) && length(data_list) > 0 &&
          is.null(rv_bound_data$do_bound)) {
        combined <- dplyr::bind_rows(data_list)
        if ("timestamp" %in% names(combined) && is.character(combined$timestamp)) {
          combined$timestamp <- .parse_timestamp(combined$timestamp)
        }
        rv_bound_data$do_bound <- combined
      }
    })

    # ------ Convenience reactives ------
    do_current <- reactive({
      rv_bound_data$do_bound
    })

    baro_qc_current <- reactive({
      rv_qaqc_data$baro_qaqc
    })

    # ------ Data status display ------
    output$data_status <- renderUI({
      do_df <- do_current()
      baro_df <- baro_qc_current()

      do_msg <- if (is.null(do_df) || nrow(do_df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 8px;",
          icon("info-circle"),
          " No dissolved oxygen data loaded. Upload files below or use pre-loaded sample data."
        )
      } else {
        n_stations <- length(unique(do_df$site_station_code))
        n_records <- format(nrow(do_df), big.mark = ",")
        date_min <- format(min(do_df$timestamp, na.rm = TRUE), "%Y-%m-%d")
        date_max <- format(max(do_df$timestamp, na.rm = TRUE), "%Y-%m-%d")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 8px;",
          icon("check-circle"),
          sprintf(" DO data loaded: %d station(s), %s records (%s to %s)",
                  n_stations, n_records, date_min, date_max)
        )
      }

      baro_msg <- if (is.null(baro_df) || nrow(baro_df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px;",
          icon("info-circle"),
          " No processed barometric data available. Load baro data below."
        )
      } else {
        n_baro <- length(unique(baro_df$site_station_code))
        n_baro_rec <- format(nrow(baro_df), big.mark = ",")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px;",
          icon("check-circle"),
          sprintf(" Processed baro data: %d station(s), %s records",
                  n_baro, n_baro_rec)
        )
      }

      tagList(do_msg, baro_msg)
    })

    # ------ Station map (both DO + baro, colored by metric) ------
    output$station_map <- renderLeaflet({
      metadata <- rv_raw_data$metadata
      req(metadata)

      # Filter to dissolvedoxygen + barometric stations
      do_baro_meta <- metadata[metadata$metric %in% c("dissolvedoxygen", "barometric"), ]
      sites <- unique(do_baro_meta[, c("site_station_code", "latitude", "longitude", "metric")])
      sites <- sites[!is.na(sites$latitude) & !is.na(sites$longitude), ]

      req(nrow(sites) > 0)

      # Split by metric for toggleable overlay groups
      do_sites   <- sites[sites$metric == "dissolvedoxygen", ]
      baro_sites <- sites[sites$metric == "barometric", ]

      map <- leaflet(sites) %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo")

      if (nrow(do_sites) > 0) {
        map <- map %>% addCircleMarkers(
          data = do_sites,
          lng = ~longitude, lat = ~latitude,
          radius = 7, weight = 2,
          color = "#fff", fillColor = "#1565C0", fillOpacity = 0.9,
          label = ~site_station_code,
          popup = ~paste0(
            "<strong>", site_station_code, "</strong><br>",
            "Metric: ", metric, "<br>",
            "Lat: ", round(latitude, 4), "<br>",
            "Lon: ", round(longitude, 4)
          ),
          group = "Dissolved Oxygen",
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 20,
            spiderfyOnMaxZoom = TRUE,
            disableClusteringAtZoom = 14
          )
        )
      }

      if (nrow(baro_sites) > 0) {
        map <- map %>% addCircleMarkers(
          data = baro_sites,
          lng = ~longitude, lat = ~latitude,
          radius = 7, weight = 2,
          color = "#fff", fillColor = "#FF8F00", fillOpacity = 0.9,
          label = ~site_station_code,
          popup = ~paste0(
            "<strong>", site_station_code, "</strong><br>",
            "Metric: ", metric, "<br>",
            "Lat: ", round(latitude, 4), "<br>",
            "Lon: ", round(longitude, 4)
          ),
          group = "Barometric",
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 20,
            spiderfyOnMaxZoom = TRUE,
            disableClusteringAtZoom = 14
          )
        )
      }

      map %>%
        addLegend(
          position = "bottomright",
          colors = c("#1565C0", "#FF8F00"),
          labels = c("Dissolved Oxygen", "Barometric"),
          title = "Metric"
        ) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Topo"),
          overlayGroups = c("Dissolved Oxygen", "Barometric"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # ------ Station metadata table (both metrics) ------
    output$station_table <- DT::renderDT({
      metadata <- rv_raw_data$metadata
      req(metadata)

      do_baro_meta <- metadata[metadata$metric %in% c("dissolvedoxygen", "barometric"), ]
      req(nrow(do_baro_meta) > 0)

      display_cols <- intersect(
        c("site_station_code", "timestamp_deploy", "timestamp_remove",
          "model", "sn", "metric", "status", "latitude", "longitude"),
        names(do_baro_meta)
      )

      DT::datatable(do_baro_meta[, display_cols],
        options = list(
          dom = "tip",
          pageLength = 20,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # ------ Deployment timeline (dynamic height, both metrics) ------
    output$deployment_timeline_ui <- renderUI({
      metadata <- rv_raw_data$metadata
      req(metadata)
      n_rows <- sum(metadata$metric %in% c("dissolvedoxygen", "barometric"))
      height_px <- max(200, n_rows * 50 + 80)
      plotlyOutput(session$ns("deployment_timeline"),
        height = paste0(height_px, "px")) %>% withSpinner()
    })

    # ------ Deployment timeline (Gantt chart, colored by SN) ------
    output$deployment_timeline <- renderPlotly({
      metadata <- rv_raw_data$metadata
      req(metadata)

      do_baro_meta <- metadata[metadata$metric %in% c("dissolvedoxygen", "barometric"), ]
      req(nrow(do_baro_meta) > 0)

      # Parse deploy/remove timestamps
      do_baro_meta$deploy <- as.POSIXct(do_baro_meta$timestamp_deploy,
        format = "%m/%d/%Y %H:%M", tz = "UTC")
      do_baro_meta$remove <- as.POSIXct(do_baro_meta$timestamp_remove,
        format = "%m/%d/%Y %H:%M", tz = "UTC")

      # Order by metric then station then deploy time
      do_baro_meta <- do_baro_meta[order(do_baro_meta$metric,
                                          do_baro_meta$site_station_code,
                                          do_baro_meta$deploy), ]

      # Create a unique label per row for the y-axis
      do_baro_meta$row_label <- paste0(
        do_baro_meta$site_station_code, " (SN: ", do_baro_meta$sn, ") [",
        seq_len(nrow(do_baro_meta)), "]"
      )
      # Preserve order as factor
      do_baro_meta$row_label <- factor(do_baro_meta$row_label,
        levels = rev(do_baro_meta$row_label))

      # Colour by unique serial number
      do_baro_meta$sn_factor <- as.character(do_baro_meta$sn)

      # Build colour palette
      unique_sns <- unique(do_baro_meta$sn_factor)
      palette <- RColorBrewer::brewer.pal(max(3, length(unique_sns)), "Set2")
      sn_colors <- setNames(palette[seq_along(unique_sns)], unique_sns)

      # Build plot with one trace per SN (for legend grouping)
      p <- plot_ly()
      legend_shown <- character(0)

      for (i in seq_len(nrow(do_baro_meta))) {
        row <- do_baro_meta[i, ]
        show_legend <- !(row$sn_factor %in% legend_shown)
        if (show_legend) legend_shown <- c(legend_shown, row$sn_factor)

        metric_label <- ifelse(row$metric == "dissolvedoxygen", "Dissolved Oxygen", "Barometric")

        p <- p %>% add_trace(
          x = c(row$deploy, row$remove),
          y = c(as.character(row$row_label), as.character(row$row_label)),
          type = "scatter", mode = "lines",
          line = list(width = 16, color = sn_colors[row$sn_factor]),
          name = paste("SN:", row$sn_factor),
          legendgroup = row$sn_factor,
          showlegend = show_legend,
          hoverinfo = "text",
          text = paste0(
            "<b>", row$site_station_code, "</b><br>",
            "Metric: ", metric_label, "<br>",
            "SN: ", row$sn, "<br>",
            "Deploy: ", format(row$deploy, "%Y-%m-%d %H:%M"), "<br>",
            "Remove: ", format(row$remove, "%Y-%m-%d %H:%M")
          )
        )
      }

      p %>% layout(
        title = list(text = "Deployment Timeline", font = list(size = 14)),
        xaxis = list(title = "Date", type = "date"),
        yaxis = list(title = "", categoryorder = "array",
                     categoryarray = rev(levels(do_baro_meta$row_label))),
        legend = list(title = list(text = "Serial Number"),
                      orientation = "h", y = -0.2),
        margin = list(l = 180, b = 60)
      )
    })

    # ------ Update metadata immediately on upload ------
    observeEvent(input$meta_file, {
      req(input$meta_file)
      tryCatch({
        metadata <- read.csv(input$meta_file$datapath, stringsAsFactors = FALSE)

        # Check required columns
        required_cols <- c("site_station_code", "timestamp_deploy", "timestamp_remove",
                           "model", "sn", "metric", "status", "latitude", "longitude")
        missing_cols <- setdiff(required_cols, names(metadata))
        if (length(missing_cols) > 0) {
          showNotification(
            paste0("Metadata upload rejected. Missing required column(s): ",
                   paste(missing_cols, collapse = ", ")),
            type = "error", duration = 10
          )
          return()
        }

        # Normalize metric column: lowercase, strip spaces
        metadata$metric <- tolower(trimws(gsub("\\s+", "", metadata$metric)))

        # Validate: allow dissolvedoxygen, barometric, and waterlevel
        valid_metrics <- c("dissolvedoxygen", "barometric", "waterlevel")
        invalid <- unique(metadata$metric[!metadata$metric %in% valid_metrics])
        if (length(invalid) > 0) {
          showNotification(
            paste0("Invalid metric values removed: ", paste(invalid, collapse = ", "),
                   ". Allowed values: ", paste(valid_metrics, collapse = ", "), "."),
            type = "warning", duration = 8
          )
          metadata <- metadata[metadata$metric %in% valid_metrics, ]
        }

        # Warn if no dissolvedoxygen rows found after filtering
        n_do <- sum(metadata$metric == "dissolvedoxygen")
        if (n_do == 0) {
          found_metrics <- unique(metadata$metric)
          showNotification(
            paste0("Warning: No rows with metric 'dissolvedoxygen' found in metadata. ",
                   "Found metric values: ",
                   if (length(found_metrics) > 0) paste(found_metrics, collapse = ", ") else "none",
                   "."),
            type = "warning", duration = 10
          )
        }

        rv_raw_data$metadata <- metadata
        showNotification("Metadata updated.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error reading metadata:", conditionMessage(e)),
          type = "error", duration = 5)
      })
    })

    # ------ Update station selector for snap ------
    observe({
      df <- do_current()
      if (!is.null(df)) {
        stations <- unique(df$site_station_code)
        choices <- c("All Stations" = "all", setNames(stations, stations))
        updateSelectInput(session, "snap_station", choices = choices)
      }
    })

    # ------ Bind result tracking ------
    rv_bind_result <- reactiveValues(
      success = FALSE,
      n_stations = 0,
      n_records = 0,
      matched_stations = character(0),
      unmatched_sns = character(0),
      metadata_stations = character(0)
    )

    # ------ Bind & Import button ------
    observeEvent(input$bind_btn, {
      raw_files <- input$raw_files
      metadata <- rv_raw_data$metadata

      if (is.null(metadata)) {
        showNotification("No metadata available. Upload a metadata CSV first.",
          type = "error", duration = 5)
        return()
      }

      # If raw files were uploaded, bind them
      if (!is.null(raw_files)) {
        # Show spinner & disable button
        shinyjs::show("bind_spinner")
        shinyjs::disable("bind_btn")

        tryCatch({
          result <- bind_hobo_files(
            file_paths = raw_files$datapath,
            metadata = metadata
          )

          bound_data <- result[[1]]

          # Update shared reactive
          rv_bound_data$do_bound <- bound_data

          # Track bind results for summary display
          bound_stations <- unique(bound_data$site_station_code)
          meta_do_stations <- unique(
            metadata$site_station_code[metadata$metric %in% c("dissolvedoxygen")]
          )
          matched <- intersect(bound_stations, meta_do_stations)
          unmatched_in_data <- setdiff(bound_stations, meta_do_stations)

          rv_bind_result$success <- TRUE
          rv_bind_result$n_stations <- length(bound_stations)
          rv_bind_result$n_records <- nrow(bound_data)
          rv_bind_result$matched_stations <- matched
          rv_bind_result$unmatched_sns <- unmatched_in_data
          rv_bind_result$metadata_stations <- meta_do_stations

          shinyjs::hide("bind_spinner")
          shinyjs::enable("bind_btn")
          n_rows <- format(nrow(bound_data), big.mark = ",")
          n_stns <- length(bound_stations)
          showNotification(
            sprintf("Import complete: %s records from %d station(s)", n_rows, n_stns),
            type = "message", duration = 5
          )
        }, error = function(e) {
          shinyjs::hide("bind_spinner")
          shinyjs::enable("bind_btn")
          rv_bind_result$success <- FALSE
          showNotification(paste("Import error:", conditionMessage(e)),
            type = "error", duration = 8)
        })
      } else {
        showNotification(
          "No raw files selected. Upload HOBO CSV files first.",
          type = "warning", duration = 3
        )
      }
    })

    # ------ Bind summary display ------
    output$bind_summary <- renderUI({
      if (!rv_bind_result$success) return(NULL)

      n_stns <- rv_bind_result$n_stations
      n_recs <- format(rv_bind_result$n_records, big.mark = ",")
      matched <- rv_bind_result$matched_stations
      unmatched <- rv_bind_result$unmatched_sns
      meta_stns <- rv_bind_result$metadata_stations

      # Stations in metadata but not in bound data
      missing_from_data <- setdiff(meta_stns, matched)

      items <- tagList(
        tags$li(sprintf("Stations loaded: %d", n_stns)),
        tags$li(sprintf("Total records: %s", n_recs))
      )

      if (length(matched) > 0) {
        items <- tagList(items,
          tags$li(
            style = "color: #155724;",
            sprintf("Matched to metadata (%d): ", length(matched)),
            paste(matched, collapse = ", ")
          )
        )
      }

      if (length(unmatched) > 0) {
        items <- tagList(items,
          tags$li(
            style = "color: #856404;",
            icon("exclamation-triangle"),
            sprintf(" Not found in metadata (%d): ", length(unmatched)),
            paste(unmatched, collapse = ", ")
          )
        )
      }

      if (length(missing_from_data) > 0) {
        items <- tagList(items,
          tags$li(
            style = "color: #856404;",
            icon("info-circle"),
            sprintf(" In metadata but no data loaded (%d): ", length(missing_from_data)),
            paste(missing_from_data, collapse = ", ")
          )
        )
      }

      border_color <- if (length(unmatched) > 0) "#ffc107" else "#28a745"

      tags$div(
        style = sprintf("background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px; border-left: 4px solid %s;", border_color),
        tags$h5("Import Summary", style = "margin-top: 0; margin-bottom: 8px;"),
        tags$ul(
          style = "padding-left: 20px; font-size: 13px; margin-bottom: 0;",
          items
        )
      )
    })

    # ------ Snap timestamps button ------
    observeEvent(input$snap_btn, {
      df <- do_current()
      req(df)

      tryCatch({
        showNotification("Snapping timestamps...", type = "message",
          duration = NULL, id = "snap_progress")

        station_arg <- if (input$snap_station == "all") NULL else input$snap_station

        result <- resample_timestamps(
          input_data = df,
          interval = input$snap_interval,
          align = input$snap_align,
          select_station = station_arg,
          metric = "DO"
        )

        rv_bound_data$do_bound <- result

        removeNotification("snap_progress")
        showNotification("Timestamps snapped.", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("snap_progress")
        showNotification(paste("Snap error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Baro status message ------
    output$baro_status_message <- renderUI({
      baro_df <- baro_qc_current()
      if (!is.null(baro_df) && nrow(baro_df) > 0) {
        n_stns <- length(unique(baro_df$site_station_code))
        n_recs <- format(nrow(baro_df), big.mark = ",")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
          icon("check-circle"),
          sprintf(" Processed baro data loaded: %d station(s), %s records.",
                  n_stns, n_recs)
        )
      } else {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
          icon("exclamation-triangle"),
          " No processed baro data loaded. Upload a processed baro CSV below, ",
          "or complete the Barometric Pressure workflow first."
        )
      }
    })

    # ------ Load baro button ------
    observeEvent(input$load_baro_btn, {
      baro_file <- input$baro_qc_file

      if (is.null(baro_file)) {
        showNotification("No baro file selected. Please upload a processed baro CSV.",
          type = "error", duration = 5)
        return()
      }

      # Show spinner & disable button
      shinyjs::show("baro_spinner")
      shinyjs::disable("load_baro_btn")

      tryCatch({
        baro_data <- read.csv(baro_file$datapath, stringsAsFactors = FALSE)

        # Validate required columns
        required_cols <- c("site_station_code", "timestamp",
                           "airpress_kPa", "airpress_kPa_adj",
                           "airtemp_C", "airtemp_C_adj")
        missing_cols <- setdiff(required_cols, names(baro_data))

        if (length(missing_cols) > 0) {
          shinyjs::hide("baro_spinner")
          shinyjs::enable("load_baro_btn")
          showNotification(
            paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
            type = "error", duration = 8
          )
          return()
        }

        # Parse timestamps
        if (is.character(baro_data$timestamp)) {
          baro_data$timestamp <- .parse_timestamp(baro_data$timestamp)
        }

        # Validate station codes against metadata
        metadata <- rv_raw_data$metadata
        if (!is.null(metadata)) {
          meta_stations <- unique(metadata$site_station_code)
          baro_stations <- unique(baro_data$site_station_code)
          missing_stns <- setdiff(baro_stations, meta_stations)
          if (length(missing_stns) > 0) {
            showNotification(
              paste("Warning: baro stations not in metadata:",
                    paste(missing_stns, collapse = ", ")),
              type = "warning", duration = 8
            )
          }
        }

        # Store in session
        rv_qaqc_data$baro_qaqc <- baro_data

        shinyjs::hide("baro_spinner")
        shinyjs::enable("load_baro_btn")
        n_rows <- format(nrow(baro_data), big.mark = ",")
        n_stns <- length(unique(baro_data$site_station_code))
        showNotification(
          sprintf("Baro data loaded: %s records from %d station(s)", n_rows, n_stns),
          type = "message", duration = 5
        )
      }, error = function(e) {
        shinyjs::hide("baro_spinner")
        shinyjs::enable("load_baro_btn")
        showNotification(paste("Error reading baro file:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Baro validation display ------
    output$baro_validation <- renderUI({
      baro_df <- baro_qc_current()
      if (is.null(baro_df) || nrow(baro_df) == 0) return(NULL)

      n_stns <- length(unique(baro_df$site_station_code))
      n_recs <- format(nrow(baro_df), big.mark = ",")
      date_min <- format(min(baro_df$timestamp, na.rm = TRUE), "%Y-%m-%d")
      date_max <- format(max(baro_df$timestamp, na.rm = TRUE), "%Y-%m-%d")

      # Check for edit/flag columns
      edit_cols <- grep("^edit_", names(baro_df), value = TRUE)
      flag_cols <- grep("^flag_", names(baro_df), value = TRUE)

      tags$div(
        style = "background-color: #e8f5e9; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
        tags$h4("Loaded Barometric Data", style = "margin-top: 0;"),
        tags$ul(
          style = "padding-left: 20px; font-size: 13px; margin-bottom: 5px;",
          tags$li(sprintf("Stations: %d", n_stns)),
          tags$li(sprintf("Records: %s", n_recs)),
          tags$li(sprintf("Date range: %s to %s", date_min, date_max)),
          if (length(edit_cols) > 0)
            tags$li(paste("Edit columns:", paste(edit_cols, collapse = ", "))),
          if (length(flag_cols) > 0)
            tags$li(paste("Flag columns:", paste(flag_cols, collapse = ", ")))
        )
      )
    })

    # ------ Import summary table ------
    output$import_summary <- DT::renderDT({
      df <- do_current()
      req(df)

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `Start Date` = format(min(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
          `End Date` = format(max(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
          `DO Min (mg/L)` = round(min(do_mgl, na.rm = TRUE), 2),
          `DO Max (mg/L)` = round(max(do_mgl, na.rm = TRUE), 2),
          `Temp Min (C)` = round(min(watertemp_C, na.rm = TRUE), 1),
          `Temp Max (C)` = round(max(watertemp_C, na.rm = TRUE), 1),
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ------ Preview plot (4 panels: DO mg/L, water temp, baro press, baro temp) ------
    output$preview_plot <- renderPlotly({
      do_df <- do_current()
      req(do_df, nrow(do_df) > 0)

      baro_df <- baro_qc_current()

      do_stations <- unique(do_df$site_station_code)

      # Panel 1: DO concentration (mg/L)
      p1 <- plot_ly()
      for (stn in do_stations) {
        stn_data <- dplyr::filter(do_df, site_station_code == stn)
        p1 <- p1 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~do_mgl,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- DO"),
          legendgroup = stn,
          hovertemplate = paste0(stn, ": %{y:.2f} mg/L<extra></extra>")
        )
      }

      # Panel 2: Water temperature (C)
      p2 <- plot_ly()
      for (stn in do_stations) {
        stn_data <- dplyr::filter(do_df, site_station_code == stn)
        p2 <- p2 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~watertemp_C,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- Water Temp"),
          legendgroup = stn,
          showlegend = FALSE,
          hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
        )
      }

      # Panels 3-4: Barometric data (if available)
      if (!is.null(baro_df) && nrow(baro_df) > 0) {
        baro_stations <- unique(baro_df$site_station_code)

        # Use adjusted columns if available, fall back to raw
        press_col <- if ("airpress_kPa_adj" %in% names(baro_df)) "airpress_kPa_adj" else "airpress_kPa"
        temp_col <- if ("airtemp_C_adj" %in% names(baro_df)) "airtemp_C_adj" else "airtemp_C"

        # Panel 3: Air pressure
        p3 <- plot_ly()
        for (stn in baro_stations) {
          stn_data <- dplyr::filter(baro_df, site_station_code == stn)
          p3 <- p3 %>% add_trace(
            data = stn_data, x = ~timestamp, y = as.formula(paste0("~", press_col)),
            type = "scattergl", mode = "lines",
            line = list(width = 1),
            name = paste(stn, "- Air Press"),
            legendgroup = paste0("baro_", stn),
            hovertemplate = paste0(stn, ": %{y:.2f} kPa<extra></extra>")
          )
        }

        # Panel 4: Air temperature
        p4 <- plot_ly()
        for (stn in baro_stations) {
          stn_data <- dplyr::filter(baro_df, site_station_code == stn)
          p4 <- p4 %>% add_trace(
            data = stn_data, x = ~timestamp, y = as.formula(paste0("~", temp_col)),
            type = "scattergl", mode = "lines",
            line = list(width = 1),
            name = paste(stn, "- Air Temp"),
            legendgroup = paste0("baro_", stn),
            showlegend = FALSE,
            hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
          )
        }
      } else {
        # Empty baro panels with annotation
        p3 <- plot_ly() %>%
          layout(
            annotations = list(list(
              text = "No barometric data loaded",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE,
              font = list(size = 14, color = "#999")
            ))
          )
        p4 <- plot_ly() %>%
          layout(
            annotations = list(list(
              text = "No barometric data loaded",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE,
              font = list(size = 14, color = "#999")
            ))
          )
      }

      subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "Dissolved Oxygen & Barometric Data Preview",
                       font = list(size = 14)),
          xaxis4 = list(title = "Date"),
          yaxis = list(title = "DO (mg/L)"),
          yaxis2 = list(title = "Water Temp (\u00b0C)"),
          yaxis3 = list(title = "Air Press (kPa)"),
          yaxis4 = list(title = "Air Temp (\u00b0C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.08),
          margin = list(b = 80)
        )
    })

    # ---- Date Range Filter: auto-populate ----
    observe({
      do_data <- rv_bound_data$do_bound
      req(do_data, nrow(do_data) > 0)
      ts_range <- range(do_data$timestamp, na.rm = TRUE)
      updateDateInput(session, "filter_from", value = as.Date(ts_range[1]),
                      min = as.Date(ts_range[1]), max = as.Date(ts_range[2]))
      updateDateInput(session, "filter_to",   value = as.Date(ts_range[2]),
                      min = as.Date(ts_range[1]), max = as.Date(ts_range[2]))
    })

    # ---- Date Range Filter: apply ----
    observeEvent(input$apply_filter, {
      from_date <- input$filter_from
      to_date   <- input$filter_to
      req(from_date, to_date)

      from_ts <- as.POSIXct(from_date, tz = "UTC")
      to_ts   <- as.POSIXct(paste(to_date, "23:59:59"), tz = "UTC")

      do_data <- rv_bound_data$do_bound
      if (!is.null(do_data) && nrow(do_data) > 0)
        rv_bound_data$do_bound <- do_data[do_data$timestamp >= from_ts & do_data$timestamp <= to_ts, ]

      showNotification(
        sprintf("Filtered to %s \u2013 %s", format(from_date), format(to_date)),
        type = "message", duration = 5
      )
    })

    # ---- Date Range Filter: status ----
    output$filter_status <- renderUI({
      req(input$apply_filter > 0)
      isolate({
        from_date <- input$filter_from
        to_date   <- input$filter_to
        n <- nrow(rv_bound_data$do_bound)
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;",
          icon("check-circle"),
          sprintf(" Filter applied: %s to %s. %s records remaining.",
                  format(from_date), format(to_date), format(n, big.mark = ","))
        )
      })
    })

  })
}


# -----------------------------------------------------------------------------
# 2. Convert to % Sat.
# -----------------------------------------------------------------------------
module_do_convert_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Convert to % Saturation", class = "custom-h1"),
      tags$h2("Add nearest barometric station and convert DO units",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Dissolved oxygen loggers record concentration in mg/L, but percent ",
        "air saturation (%sat) is needed for cross-site comparison. %sat depends ",
        "on water temperature AND barometric pressure, so this step first joins ",
        "baro data, then applies the Weiss (1970/1974) solubility equations to ",
        "compute %sat."
      ),

      tags$div(
        class = "clean-print",
        tags$h4("Why Barometric Pressure?", style = "margin-top: 10px;"),
        tags$p(
          "DO saturation depends on atmospheric pressure. At higher altitudes, ",
          "lower air pressure means lower O\u2082 partial pressure, so the same ",
          "mg/L value represents a higher % saturation. Without pressure ",
          "correction, cross-site comparisons are biased."
        ),
        tags$h4("The Weiss Equation", style = "margin-top: 15px;"),
        tags$p(
          "The Weiss (1970/1974) equations compute the theoretical DO saturation ",
          "at 1 atm from the water temperature (in Kelvin), then apply a pressure ",
          "correction factor using barometric pressure. Percent saturation is ",
          "then: ", tags$code("%sat = 100 \u00d7 (measured mg/L / corrected saturation)"), "."
        )
      ),

      hr(),

      # ===== Part A: Add Nearest Baro =====
      tags$h3("Part A: Add Nearest Barometric Station",
              style = "color: #1565c0; margin-bottom: 15px;"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",

            tags$h4("How Station Matching Works", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "The function uses ", tags$code("geosphere::distHaversine()"), " to ",
              "rank all barometric stations by horizontal distance (lat/lon) from ",
              "each DO site. It keeps the nearest ", tags$code("max_stations"),
              " (default 5) within an optional ", tags$code("max_km"), " radius."
            ),
            tags$p(style = "font-size: 13px;",
              "Barometric data is joined to DO data by an exact ",
              tags$strong("timestamp match"), " (left join) \u2014 no interpolation is ",
              "performed. If timestamps don't align, the function warns about the ",
              "offset. Use ", tags$strong("Snap Timestamps"), " in the Import step ",
              "to ensure both series are on the same grid."
            ),

            tags$h4("Multi-Station Gap-Filling", style = "margin-top: 15px;"),
            tags$p(style = "font-size: 13px;",
              "When multiple baro stations are available, the function builds a ",
              "composite series. Secondary stations are calibrated to the nearest ",
              "(reference) station via linear regression over their overlapping ",
              "timestamps (minimum 10 points). Values are then coalesced ",
              "nearest-first: the closest station fills first, then calibrated ",
              "alternates fill any remaining gaps."
            ),
            tags$p(style = "font-size: 13px;",
              "A ", tags$code("baro_site_stn_code"), " column is added to track ",
              "which baro station provided each row's value."
            )
          ),

          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("add_nearest_baro()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Matches each DO station to the nearest barometric station(s) ",
                   "by GPS distance and joins baro pressure/temperature by timestamp. ",
                   "Called with ", tags$code('metric = "DO"'), ". Outputs the input ",
                   "data frame with added ",
                   tags$code("airpress_kPa"), ", ", tags$code("airtemp_C"), ", and ",
                   tags$code("baro_site_stn_code"), " columns.")
          )
        ),
        column(
          width = 8,

          # ---- Data readiness banner ----
          uiOutput(ns("baro_readiness")),

          # ---- Run controls ----
          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; margin-bottom: 15px;",

            # Station selector
            tags$h4("Stations to Process", style = "margin-top: 0;"),
            selectizeInput(ns("baro_do_stations"), NULL,
              choices = NULL, multiple = TRUE,
              width = "100%",
              options = list(
                placeholder = "All stations (default)",
                plugins = list("remove_button")
              )
            ),

            # Parameter row
            fluidRow(
              column(4,
                selectInput(ns("baro_selection"), "Baro Selection Mode:",
                  choices = c("Auto (nearest)" = "auto"),
                  width = "100%"
                )
              ),
              column(4,
                numericInput(ns("max_km"), "Max Distance (km):",
                  value = NA, min = 1, max = 500)
              ),
              column(4,
                numericInput(ns("max_stations"), "Max Baro Stations:",
                  value = 5, min = 1, max = 20)
              )
            ),

            tags$p(style = "font-size: 12px; color: #666; margin-top: -5px; margin-bottom: 15px;",
              "Leave station selector empty to process all DO stations. Select ",
              "specific stations to re-run with different settings \u2014 existing ",
              "results for other stations are preserved."
            ),

            actionButton(ns("run_add_baro"), "Add Baro Data",
              class = "btn-primary btn-lg", width = "100%",
              icon = icon("cloud"),
              style = "color: #ffffff;"
            )
          ),

          # ---- Results (conditional) ----
          conditionalPanel(
            condition = "output.show_baro_results",
            ns = ns,

            # Section divider: Baro Assignment Results
            tags$div(
              style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
              tags$span(
                style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
                "Baro Assignment Results"
              )
            ),

            tags$h4("Summary by Station"),
            DT::DTOutput(ns("baro_summary_table"))
          )
        )
      ),

      # ---- Station Detail (full-width, conditional on baro results) ----
      conditionalPanel(
        condition = "output.show_baro_results",
        ns = ns,

        # Section divider: Station Detail
        tags$div(
          style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Station Detail"
          )
        ),

        fluidRow(
          column(4,
            tags$div(
              style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; border: 2px solid #1565c0;",
              selectInput(ns("detail_station"), "Select Station:",
                choices = NULL, width = "100%")
            )
          ),
          column(8,
            uiOutput(ns("station_detail_info"))
          )
        ),

        uiOutput(ns("calibration_info")),

        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("baro_assign_plot"), height = "650px")
        )
      ),

      hr(),

      # ===== Part B: Convert DO to Percent Saturation =====
      tags$h3("Part B: Convert DO to Percent Saturation",
              style = "color: #2e7d32; margin-bottom: 15px;"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("The Conversion", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "The Weiss (1970/1974) equations convert dissolved oxygen from ",
              "mg/L to percent air saturation in three steps:"
            ),
            tags$ol(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Compute theoretical DO saturation at 1 atm from water temperature (Kelvin)"),
              tags$li("Apply pressure correction factor using barometric pressure"),
              tags$li("Calculate: ", tags$code("%sat = 100 \u00d7 (measured mg/L / corrected saturation)"))
            ),
            tags$p(style = "font-size: 13px; margin-top: 10px; color: #666;",
              "Values of ", tags$code("-888.88"), " in ", tags$code("do_mgl"),
              " are treated as logger errors and produce NA."
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "Rows missing barometric pressure (gaps in baro coverage) will ",
              "also produce NA."
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("convert_do_mgl_percsat()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Applies Weiss (1970/1974) equations. Adds a ",
                   tags$code("do_percsat"), " column (numeric, percent air saturation).")
          )
        ),
        column(
          width = 8,

          # ---- Station Readiness ----
          tags$div(
            style = "border-top: 2px solid #2e7d32; margin: 0 0 20px 0; position: relative;",
            tags$span(
              style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
              "Station Readiness"
            )
          ),

          uiOutput(ns("convert_readiness")),

          # ---- Convert Dissolved Oxygen ----
          tags$div(
            style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
            tags$span(
              style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
              "Convert Dissolved Oxygen"
            )
          ),

          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; margin-bottom: 15px;",

            selectizeInput(ns("convert_do_stations"), "Stations to Convert:",
              choices = NULL, multiple = TRUE, width = "100%",
              options = list(
                placeholder = "All ready stations (default)",
                plugins = list("remove_button")
              )
            ),

            actionButton(ns("run_convert"), "Convert to % Saturation",
              class = "btn-success btn-lg", width = "100%",
              icon = icon("flask"),
              style = "color: #ffffff;"
            )
          )
        )
      ),

      # ---- Conversion Results (full-width, conditional) ----
      conditionalPanel(
        condition = "output.show_convert_results",
        ns = ns,

        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Conversion Results"
          )
        ),

        tags$h4("Summary by Station"),
        DT::DTOutput(ns("convert_summary_table")),

        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Station Detail"
          )
        ),

        fluidRow(
          column(4,
            tags$div(
              style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px; border: 2px solid #2e7d32;",
              selectInput(ns("convert_detail_station"), "Select Station:",
                choices = NULL, width = "100%")
            )
          ),
          column(8,
            uiOutput(ns("convert_station_info"))
          )
        ),

        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("convert_review_plot"), height = "850px")
        ),

        # ---- Next step link ----
        tags$div(
          style = "margin-top: 25px; padding: 15px; background-color: #e8f5e9; border-radius: 5px; text-align: center;",
          tags$p(style = "margin: 0; font-size: 14px;",
            icon("arrow-right"),
            " Ready to proceed? Continue to ",
            tags$a(
              href = "#",
              onclick = "$(\"a[href='#shiny-tab-t_do_qc']\").tab('show'); window.scrollTo(0,0); return false;",
              style = "color: #2e7d32; font-weight: bold;",
              "QC Dissolved Oxygen"
            ),
            " to run automated quality checks and review flagged data."
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_do_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data       <- session$userData$rv_raw_data
    rv_bound_data     <- session$userData$rv_bound_data
    rv_qaqc_data      <- session$userData$rv_qaqc_data
    rv_converted_data <- session$userData$rv_converted_data

    # Module-local reactives
    baro_result   <- reactiveVal(NULL)    # DO+baro joined data frame
    baro_messages <- reactiveVal(NULL)    # captured message() output

    convert_result <- reactiveVal(NULL)   # DO with do_percsat added

    # Conditional panel visibility
    output$show_baro_results <- reactive({ !is.null(baro_result()) })
    outputOptions(output, "show_baro_results", suspendWhenHidden = FALSE)

    output$show_convert_results <- reactive({ !is.null(convert_result()) })
    outputOptions(output, "show_convert_results", suspendWhenHidden = FALSE)

    # =======================================================================
    # Part A: Add Nearest Baro — Server Logic
    # =======================================================================

    # ---- Data readiness banner ----
    output$baro_readiness <- renderUI({
      do_data <- rv_bound_data$do_bound
      baro    <- rv_qaqc_data$baro_qaqc
      if (is.null(do_data) || nrow(do_data) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No dissolved oxygen data available. Import data in the Import tab first."
        )
      } else if (is.null(baro) || nrow(baro) == 0) {
        n_stns <- length(unique(do_data$site_station_code))
        n_recs <- format(nrow(do_data), big.mark = ",")
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          sprintf(" DO data ready (%d station(s), %s records) but no QC'd barometric data found. Process baro data first.",
                  n_stns, n_recs)
        )
      } else {
        n_stns <- length(unique(do_data$site_station_code))
        n_recs <- format(nrow(do_data), big.mark = ",")
        n_baro <- length(unique(baro$site_station_code))
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Ready: %d DO station(s), %s records | %d baro station(s) available",
                  n_stns, n_recs, n_baro)
        )
      }
    })

    # ---- Update DO station selector ----
    observe({
      do_data <- rv_bound_data$do_bound
      if (!is.null(do_data) && nrow(do_data) > 0) {
        stns <- sort(unique(do_data$site_station_code))
        updateSelectizeInput(session, "baro_do_stations", choices = stns,
                             selected = character(0))
      }
    })

    # ---- Update baro selection dropdown ----
    observe({
      baro <- rv_qaqc_data$baro_qaqc
      if (!is.null(baro)) {
        baro_stns <- sort(unique(baro$site_station_code))
        choices <- c("Auto (nearest)" = "auto", setNames(baro_stns, baro_stns))
        updateSelectInput(session, "baro_selection", choices = choices)
      }
    })

    # ---- Run add_nearest_baro ----
    observeEvent(input$run_add_baro, {
      do_data   <- rv_bound_data$do_bound
      baro_data <- rv_qaqc_data$baro_qaqc
      metadata  <- rv_raw_data$metadata

      if (is.null(do_data) || nrow(do_data) == 0) {
        showNotification("No DO data. Import data first.", type = "error", duration = 5)
        return()
      }
      if (is.null(baro_data) || nrow(baro_data) == 0) {
        showNotification("No QC'd barometric data. Process baro data first.", type = "error", duration = 5)
        return()
      }
      if (is.null(metadata)) {
        showNotification("No metadata available.", type = "error", duration = 5)
        return()
      }

      tryCatch({
        showNotification("Adding barometric data to DO stations...",
                         type = "message", duration = NULL, id = "do_baro_progress")

        # Determine station subset
        selected_stns <- input$baro_do_stations
        run_subset <- length(selected_stns) > 0
        input_data <- if (run_subset) {
          dplyr::filter(do_data, site_station_code %in% selected_stns)
        } else {
          do_data
        }

        # Deduplicate baro and DO data by (site_station_code, timestamp)
        n_baro_before <- nrow(baro_data)
        baro_data <- baro_data %>%
          dplyr::arrange(site_station_code, timestamp) %>%
          dplyr::filter(!duplicated(paste(site_station_code, timestamp), fromLast = TRUE))
        n_baro_dupes <- n_baro_before - nrow(baro_data)

        n_do_before <- nrow(input_data)
        input_data <- input_data %>%
          dplyr::arrange(site_station_code, timestamp) %>%
          dplyr::filter(!duplicated(paste(site_station_code, timestamp), fromLast = TRUE))
        n_do_dupes <- n_do_before - nrow(input_data)

        if (n_baro_dupes > 0) cat("[add_baro_do] Removed", n_baro_dupes, "duplicate baro rows\n")
        if (n_do_dupes > 0) cat("[add_baro_do] Removed", n_do_dupes, "duplicate DO rows\n")

        cat("[add_baro_do] Input data:", nrow(input_data), "rows,",
            length(unique(input_data$site_station_code)), "station(s)\n")
        cat("[add_baro_do] Baro data:", nrow(baro_data), "rows,",
            length(unique(baro_data$site_station_code)), "station(s)\n")
        cat("[add_baro_do] Metadata:", nrow(metadata), "rows\n")

        # Build baro selection arg
        baro_sel <- if (input$baro_selection == "auto") "auto" else input$baro_selection
        cat("[add_baro_do] baro_selection:", baro_sel, "\n")

        # Build max_km (NA -> NULL)
        max_km_val <- input$max_km
        if (is.na(max_km_val)) max_km_val <- NULL
        cat("[add_baro_do] max_km:", if (is.null(max_km_val)) "NULL" else max_km_val,
            "| max_stations:", input$max_stations, "\n")

        if (run_subset) {
          cat("[add_baro_do] Running subset for station(s):", paste(selected_stns, collapse = ", "), "\n")
        } else {
          cat("[add_baro_do] Running for ALL stations\n")
        }

        cat("[add_baro_do] Calling add_nearest_baro() ...\n")
        t_start <- Sys.time()

        # Capture message() output for calibration diagnostics
        msgs <- character(0)
        result <- withCallingHandlers(
          add_nearest_baro(
            input_data       = input_data,
            metric           = "DO",
            baro_data        = baro_data,
            metadata         = metadata,
            max_km           = max_km_val,
            max_stations     = input$max_stations,
            baro_site_selection = baro_sel
          ),
          message = function(m) {
            cat("[add_baro_do][msg]", conditionMessage(m))
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            cat("[add_baro_do][warn]", conditionMessage(w), "\n")
            invokeRestart("muffleWarning")
          }
        )

        t_elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1)
        cat("[add_baro_do] add_nearest_baro() returned in", t_elapsed, "sec\n")
        cat("[add_baro_do] Result class:", paste(class(result), collapse = "/"), "\n")

        # add_nearest_baro returns a list; extract the data frame
        if (is.list(result) && !is.data.frame(result)) {
          cat("[add_baro_do] Extracting data frame from list (length", length(result), ")\n")
          result <- result[[1]]
        }

        cat("[add_baro_do] Result:", nrow(result), "rows,", ncol(result), "cols\n")
        cat("[add_baro_do] Columns:", paste(names(result), collapse = ", "), "\n")

        # Merge-back: preserve existing results for non-selected stations
        if (run_subset && !is.null(baro_result())) {
          existing <- dplyr::filter(baro_result(),
                                    !(site_station_code %in% selected_stns))
          cat("[add_baro_do] Merge-back: keeping", nrow(existing), "existing rows for other stations\n")
          result <- dplyr::bind_rows(existing, result) %>%
            dplyr::arrange(site_station_code, timestamp)
        }

        cat("[add_baro_do] Storing result in baro_result() and rv_bound_data$do_with_baro\n")
        baro_result(result)
        baro_messages(msgs)
        rv_bound_data$do_with_baro <- result

        removeNotification("do_baro_progress")
        n_stns <- length(unique(result$site_station_code))
        cat("[add_baro_do] Done. Baro data added for", n_stns, "station(s)\n")
        showNotification(
          sprintf("Baro data added for %d DO station(s).", n_stns),
          type = "message", duration = 4
        )

      }, error = function(e) {
        cat("[add_baro_do][ERROR]", conditionMessage(e), "\n")
        removeNotification("do_baro_progress")
        showNotification(paste("Baro join error:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })

    # ---- Baro summary table ----
    output$baro_summary_table <- DT::renderDT({
      df <- baro_result()
      req(df, "airpress_kPa" %in% names(df), "baro_site_stn_code" %in% names(df))

      metadata <- rv_raw_data$metadata

      # Build a lookup of lat/lon per station from metadata
      coords <- metadata %>%
        dplyr::select(site_station_code, latitude, longitude) %>%
        dplyr::distinct(site_station_code, .keep_all = TRUE)

      # One row per DO station x baro station
      summary_df <- df %>%
        dplyr::filter(!is.na(baro_site_stn_code)) %>%
        dplyr::group_by(site_station_code, baro_site_stn_code) %>%
        dplyr::summarise(
          Records    = dplyr::n(),
          `Coverage` = paste0(round(100 * sum(!is.na(airpress_kPa)) / dplyr::n(), 1), "%"),
          `Overlap Period` = paste(
            format(min(timestamp), "%Y-%m-%d"),
            "\u2013",
            format(max(timestamp), "%Y-%m-%d")
          ),
          .groups = "drop"
        )

      # Compute Haversine distance between each DO and baro station
      summary_df <- summary_df %>%
        dplyr::left_join(coords, by = "site_station_code") %>%
        dplyr::rename(do_lat = latitude, do_lon = longitude) %>%
        dplyr::left_join(coords, by = c("baro_site_stn_code" = "site_station_code")) %>%
        dplyr::rename(baro_lat = latitude, baro_lon = longitude) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          `Distance (km)` = if (!is.na(do_lat) && !is.na(do_lon) &&
                                !is.na(baro_lat) && !is.na(baro_lon)) {
            round(geosphere::distHaversine(
              c(do_lon, do_lat), c(baro_lon, baro_lat)
            ) / 1000, 1)
          } else {
            NA_real_
          }
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(
          `DO Station`   = site_station_code,
          `Baro Station` = baro_site_stn_code,
          `Distance (km)`,
          Records,
          Coverage,
          `Overlap Period`
        )

      DT::datatable(summary_df,
        selection = "single",
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ---- Detail station selector update ----
    prev_detail_stations <- reactiveVal(character(0))
    observe({
      df <- baro_result()
      req(df)
      stns <- sort(unique(df$site_station_code))
      if (!identical(stns, prev_detail_stations())) {
        prev_detail_stations(stns)
        current <- isolate(input$detail_station)
        sel <- if (!is.null(current) && current %in% stns) current else stns[1]
        updateSelectInput(session, "detail_station", choices = stns, selected = sel)
      }
    })

    # Sync detail station from summary table click
    observeEvent(input$baro_summary_table_rows_selected, {
      row_idx <- input$baro_summary_table_rows_selected
      if (length(row_idx) == 1) {
        df <- baro_result()
        req(df, "baro_site_stn_code" %in% names(df))
        summary_stns <- df %>%
          dplyr::filter(!is.na(baro_site_stn_code)) %>%
          dplyr::distinct(site_station_code, baro_site_stn_code) %>%
          dplyr::arrange(site_station_code, baro_site_stn_code)
        if (row_idx <= nrow(summary_stns)) {
          updateSelectInput(session, "detail_station",
                            selected = summary_stns$site_station_code[row_idx])
        }
      }
    })

    # ---- Selected station data ----
    selected_station_data <- reactive({
      df <- baro_result()
      stn <- input$detail_station
      req(df, stn)
      dplyr::filter(df, site_station_code == stn)
    })

    # ---- Station detail info ----
    output$station_detail_info <- renderUI({
      df <- selected_station_data()
      req(nrow(df) > 0)

      n_total    <- nrow(df)
      n_baro     <- sum(!is.na(df$airpress_kPa))
      pct        <- round(100 * n_baro / n_total, 1)
      date_range <- paste(format(min(df$timestamp), "%Y-%m-%d"),
                          "to", format(max(df$timestamp), "%Y-%m-%d"))
      baro_stns  <- if ("baro_site_stn_code" %in% names(df)) {
        paste(unique(na.omit(df$baro_site_stn_code)), collapse = ", ")
      } else {
        "unknown"
      }

      tags$div(
        style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px;",
        tags$p(style = "margin: 0;",
          sprintf("%s records | Baro coverage: %.1f%% | %s", format(n_total, big.mark = ","), pct, date_range)
        ),
        tags$p(style = "margin: 5px 0 0 0; color: #555;",
          "Matched baro: ", tags$strong(baro_stns)
        )
      )
    })

    # ---- Calibration info ----
    output$calibration_info <- renderUI({
      msgs <- baro_messages()
      stn  <- input$detail_station
      req(msgs, stn)

      stn_msgs <- msgs[grepl(stn, msgs, fixed = TRUE)]
      cal_lines <- stn_msgs[grepl("Calibrat|calibrat|R2|r2|overlap", stn_msgs)]

      if (length(cal_lines) == 0) {
        df <- selected_station_data()
        if (!is.null(df) && "baro_site_stn_code" %in% names(df)) {
          n_baro <- length(unique(na.omit(df$baro_site_stn_code)))
          if (n_baro <= 1) {
            return(tags$div(
              style = "color: #555; font-size: 13px; margin: 10px 0;",
              icon("info-circle"),
              " Single baro station match \u2014 no calibration needed."
            ))
          }
        }
        return(NULL)
      }

      tags$div(
        style = "background-color: #f0f4c3; padding: 12px; border-radius: 5px; margin: 10px 0; font-size: 13px;",
        tags$strong("Calibration Diagnostics:"),
        tags$ul(
          style = "margin: 5px 0 0 0; padding-left: 20px;",
          lapply(cal_lines, function(line) tags$li(line))
        )
      )
    })

    # ---- Baro assignment review plot (2-panel: DO mg/L + baro kPa) ----
    output$baro_assign_plot <- plotly::renderPlotly({
      df <- selected_station_data()
      req(nrow(df) > 0, "airpress_kPa" %in% names(df))

      baro_colors <- c("#E53935", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1",
                       "#6D4C41", "#D81B60", "#1E88E5", "#FDD835", "#546E7A")

      # Panel 1: DO mg/L
      p1 <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = df, x = ~timestamp, y = ~do_mgl,
          type = "scatter", mode = "lines",
          name = "DO (mg/L)",
          line = list(color = "#1565c0", width = 1.5)
        ) %>%
        plotly::layout(yaxis = list(title = "DO (mg/L)"))

      # Panel 2: Baro pressure, colored by contributing station
      p2 <- plotly::plot_ly()

      if ("baro_site_stn_code" %in% names(df)) {
        baro_stns <- unique(na.omit(df$baro_site_stn_code))
        for (i in seq_along(baro_stns)) {
          stn_data <- dplyr::filter(df, baro_site_stn_code == baro_stns[i])
          if (nrow(stn_data) > 0) {
            p2 <- p2 %>% plotly::add_trace(
              data = stn_data, x = ~timestamp, y = ~airpress_kPa,
              type = "scatter", mode = "lines",
              name = paste0("Baro: ", baro_stns[i]),
              line = list(color = baro_colors[((i - 1) %% length(baro_colors)) + 1], width = 1)
            )
          }
        }
      } else {
        p2 <- p2 %>% plotly::add_trace(
          data = df, x = ~timestamp, y = ~airpress_kPa,
          type = "scatter", mode = "lines",
          name = "Baro Pressure",
          line = list(color = "#E53935", width = 1)
        )
      }

      p2 <- p2 %>% plotly::layout(yaxis = list(title = "Baro (kPa)"))

      plotly::subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.08),
          margin = list(b = 80)
        )
    })

    # =======================================================================
    # Part B: Convert DO to % Saturation — Server Logic
    # =======================================================================

    # ---- Station readiness ----
    output$convert_readiness <- renderUI({
      do_with_baro <- rv_bound_data$do_with_baro
      do_bound     <- rv_bound_data$do_bound

      # Get all DO stations
      do_stns <- character(0)
      if (!is.null(do_bound) && nrow(do_bound) > 0) {
        do_stns <- unique(do_bound$site_station_code)
      } else if (!is.null(do_with_baro) && nrow(do_with_baro) > 0) {
        do_stns <- unique(do_with_baro$site_station_code)
      }

      if (length(do_stns) == 0) {
        return(tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No dissolved oxygen stations available."
        ))
      }

      baro_stns <- if (!is.null(do_with_baro)) unique(do_with_baro$site_station_code) else character(0)

      status_items <- lapply(do_stns, function(stn) {
        has_baro <- stn %in% baro_stns
        has_airpress <- has_baro && !is.null(do_with_baro) && "airpress_kPa" %in% names(do_with_baro)
        missing <- character(0)
        if (!has_baro) missing <- c(missing, "no baro data joined")
        if (has_baro && !has_airpress) missing <- c(missing, "airpress_kPa column missing")

        if (length(missing) == 0) {
          tags$div(style = "margin-bottom: 5px;",
            icon("check-circle", style = "color: #2e7d32;"),
            tags$strong(stn), " \u2014 Ready"
          )
        } else {
          tags$div(style = "margin-bottom: 5px;",
            icon("exclamation-triangle", style = "color: #f9a825;"),
            tags$strong(stn), paste0(" \u2014 Missing: ", paste(missing, collapse = ", "))
          )
        }
      })

      tags$div(
        style = "background-color: #fafafa; padding: 15px; border-radius: 5px; border: 1px solid #e0e0e0; margin-bottom: 15px;",
        do.call(tagList, status_items)
      )
    })

    # ---- Update convert station selector ----
    observe({
      df <- baro_result()
      if (!is.null(df)) {
        stns <- sort(unique(df$site_station_code))
        updateSelectizeInput(session, "convert_do_stations", choices = stns,
                             selected = character(0))
      }
    })

    # ---- Run conversion ----
    observeEvent(input$run_convert, {
      do_with_baro <- rv_bound_data$do_with_baro

      if (is.null(do_with_baro) || nrow(do_with_baro) == 0) {
        showNotification("No DO + baro data. Run Part A first.", type = "error", duration = 5)
        return()
      }

      # Determine station subset
      selected_stns <- input$convert_do_stations
      run_subset <- length(selected_stns) > 0
      input_data <- if (run_subset) {
        dplyr::filter(do_with_baro, site_station_code %in% selected_stns)
      } else {
        do_with_baro
      }

      tryCatch({
        showNotification("Converting DO to % saturation...",
                         type = "message", duration = NULL, id = "do_convert_progress")

        cat("[convert_do] Input data:", nrow(input_data), "rows,",
            length(unique(input_data$site_station_code)), "station(s)\n")

        result <- convert_do_mgl_percsat(
          input_data = input_data,
          path_to_output_folder = NULL  # Shiny mode
        )

        cat("[convert_do] Result:", nrow(result), "rows,", ncol(result), "cols\n")

        # Merge-back for partial runs
        if (run_subset && !is.null(convert_result())) {
          existing <- dplyr::filter(convert_result(),
            !(site_station_code %in% selected_stns))
          cat("[convert_do] Merge-back: keeping", nrow(existing), "existing rows for other stations\n")
          result <- dplyr::bind_rows(existing, result) %>%
            dplyr::arrange(site_station_code, timestamp)
        }

        convert_result(result)
        rv_converted_data$do_converted <- result

        removeNotification("do_convert_progress")
        n_stns <- length(unique(result$site_station_code))
        n_valid <- sum(!is.na(result$do_percsat))
        cat("[convert_do] Done.", n_stns, "station(s),", n_valid, "valid %sat values\n")
        showNotification(
          sprintf("DO converted for %d station(s). %s valid %%sat values.",
                  n_stns, format(n_valid, big.mark = ",")),
          type = "message", duration = 4
        )

      }, error = function(e) {
        cat("[convert_do][ERROR]", conditionMessage(e), "\n")
        removeNotification("do_convert_progress")
        showNotification(paste("Conversion error:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })

    # ---- Conversion summary table ----
    output$convert_summary_table <- DT::renderDT({
      df <- convert_result()
      req(df, "do_percsat" %in% names(df))

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records       = dplyr::n(),
          `Valid %Sat`  = sum(!is.na(do_percsat)),
          `%Sat Min`    = if (all(is.na(do_percsat))) NA_real_ else round(min(do_percsat, na.rm = TRUE), 1),
          `%Sat Max`    = if (all(is.na(do_percsat))) NA_real_ else round(max(do_percsat, na.rm = TRUE), 1),
          `%Sat Mean`   = if (all(is.na(do_percsat))) NA_real_ else round(mean(do_percsat, na.rm = TRUE), 1),
          `DO Min (mg/L)` = if (all(is.na(do_mgl))) NA_real_ else round(min(do_mgl, na.rm = TRUE), 2),
          `DO Max (mg/L)` = if (all(is.na(do_mgl))) NA_real_ else round(max(do_mgl, na.rm = TRUE), 2),
          `Date Range`  = paste(
            format(min(timestamp), "%Y-%m-%d"),
            "\u2013",
            format(max(timestamp), "%Y-%m-%d")
          ),
          .groups = "drop"
        ) %>%
        dplyr::rename(Station = site_station_code)

      DT::datatable(summary_df,
        selection = "single",
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ---- Convert detail station selector update ----
    prev_convert_detail_stns <- reactiveVal(character(0))
    observe({
      df <- convert_result()
      req(df)
      stns <- sort(unique(df$site_station_code))
      if (!identical(stns, prev_convert_detail_stns())) {
        prev_convert_detail_stns(stns)
        current <- isolate(input$convert_detail_station)
        sel <- if (!is.null(current) && current %in% stns) current else stns[1]
        updateSelectInput(session, "convert_detail_station", choices = stns, selected = sel)
      }
    })

    # Sync detail station from summary table click
    observeEvent(input$convert_summary_table_rows_selected, {
      row_idx <- input$convert_summary_table_rows_selected
      if (length(row_idx) == 1) {
        df <- convert_result()
        req(df)
        stns <- sort(unique(df$site_station_code))
        if (row_idx <= length(stns)) {
          updateSelectInput(session, "convert_detail_station", selected = stns[row_idx])
        }
      }
    })

    # ---- Selected converted station data ----
    selected_convert_data <- reactive({
      df <- convert_result()
      stn <- input$convert_detail_station
      req(df, stn)
      dplyr::filter(df, site_station_code == stn)
    })

    # ---- Station detail info ----
    output$convert_station_info <- renderUI({
      df <- selected_convert_data()
      req(nrow(df) > 0, "do_percsat" %in% names(df))

      n_total    <- nrow(df)
      n_valid    <- sum(!is.na(df$do_percsat))
      n_baro     <- sum(!is.na(df$airpress_kPa))
      baro_pct   <- round(100 * n_baro / n_total, 1)
      date_range <- paste(format(min(df$timestamp), "%Y-%m-%d"),
                          "to", format(max(df$timestamp), "%Y-%m-%d"))

      if (n_valid == 0) {
        sat_range <- "no valid values"
        info_style <- "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px;"
        sat_note <- tags$p(style = "margin: 5px 0 0 0; color: #856404;",
          icon("exclamation-triangle"),
          " All %sat values are NA. This usually means barometric pressure data did not match ",
          "the DO timestamps. Use Snap Timestamps in the Import step to align them."
        )
      } else {
        sat_range <- sprintf("%.1f \u2013 %.1f %%",
                             min(df$do_percsat, na.rm = TRUE),
                             max(df$do_percsat, na.rm = TRUE))
        info_style <- "background-color: #f8f9fa; padding: 12px; border-radius: 5px;"
        sat_note <- NULL
      }

      tags$div(
        style = info_style,
        tags$p(style = "margin: 0;",
          sprintf("%s records (%s valid) | %%Sat range: %s | Baro coverage: %.1f%% | %s",
                  format(n_total, big.mark = ","), format(n_valid, big.mark = ","),
                  sat_range, baro_pct, date_range)
        ),
        sat_note
      )
    })

    # ---- Conversion review plot (3-panel) ----
    output$convert_review_plot <- plotly::renderPlotly({
      df <- selected_convert_data()
      req(nrow(df) > 0, "do_percsat" %in% names(df))

      baro_colors <- c("#E53935", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1",
                       "#6D4C41", "#D81B60", "#1E88E5", "#FDD835", "#546E7A")

      # Panel 1: DO mg/L (raw)
      p1 <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = df, x = ~timestamp, y = ~do_mgl,
          type = "scatter", mode = "lines",
          name = "DO (mg/L)",
          line = list(color = "#1565c0", width = 1.5)
        ) %>%
        plotly::layout(yaxis = list(title = "DO (mg/L)"))

      # Panel 2: DO % saturation (newly computed)
      p2 <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = df, x = ~timestamp, y = ~do_percsat,
          type = "scatter", mode = "lines",
          name = "DO (%sat)",
          line = list(color = "#2e7d32", width = 1.5)
        ) %>%
        plotly::layout(yaxis = list(title = "DO (%sat)"))

      # Panel 3: Baro pressure colored by baro_site_stn_code
      p3 <- plotly::plot_ly()

      if ("baro_site_stn_code" %in% names(df)) {
        baro_stns <- unique(na.omit(df$baro_site_stn_code))
        for (i in seq_along(baro_stns)) {
          stn_data <- dplyr::filter(df, baro_site_stn_code == baro_stns[i])
          if (nrow(stn_data) > 0) {
            p3 <- p3 %>% plotly::add_trace(
              data = stn_data, x = ~timestamp, y = ~airpress_kPa,
              type = "scatter", mode = "lines",
              name = paste0("Baro: ", baro_stns[i]),
              line = list(color = baro_colors[((i - 1) %% length(baro_colors)) + 1], width = 1)
            )
          }
        }
      } else {
        p3 <- p3 %>% plotly::add_trace(
          data = df, x = ~timestamp, y = ~airpress_kPa,
          type = "scatter", mode = "lines",
          name = "Baro Pressure",
          line = list(color = "#E53935", width = 1)
        )
      }

      p3 <- p3 %>% plotly::layout(yaxis = list(title = "Baro (kPa)"))

      plotly::subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.05),
          margin = list(b = 80)
        )
    })

  })
}


# -----------------------------------------------------------------------------
# 3. QC Dissolved Oxygen
# -----------------------------------------------------------------------------
module_do_qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,

      # ---- Header + Description ----
      tags$h1("QC Dissolved Oxygen", class = "custom-h1"),
      tags$h2("Automated quality checks, drift correction, and visual review",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Run automated quality checks on converted dissolved oxygen data. The
        function creates ", tags$code("do_mgl_adj"), ", ",
        tags$code("do_percsat_adj"), ", and ",
        tags$code("watertemp_C_do_adj"), " columns, applying corrections for
        error codes, negative values, and outliers. Flags are added for ice and
        dry conditions for your review. Use drift correction with field reference
        data, then manually remove bad periods as needed."
      ),

      uiOutput(ns("qaqc_status")),

      hr(),

      # ================================================================
      # Section 1: Run Automated QAQC
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Run Automated QAQC"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: QAQC explanation (33%) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex; flex-direction: column;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("QAQC Rules", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("DO_ERROR"), " \u2014 -888.88 values \u2192 NA"),
              tags$li(tags$strong("NEGATIVE_DO"), " \u2014 -1 to 0 \u2192 0; < -1 \u2192 NA"),
              tags$li(tags$strong("OUTLIER_DO"), " \u2014 above high limit \u2192 NA"),
              tags$li(tags$strong("NEGATIVE_TEMP"), " \u2014 -1 to 0 \u2192 0; \u2264 -1 \u2192 NA"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 temp below ice threshold \u2192 flagged"),
              tags$li(tags$strong("FLAG_DO_DRY"), " \u2014 12-point window (%sat \u2265 100, air-water temp diff \u2264 2\u00b0C) \u2192 flagged")
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Flags vs Edits", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              tags$code("flag_*"), " = suspicious, review needed (data NOT changed)", tags$br(),
              tags$code("edit_*"), " = data WAS changed (set to NA or corrected)"
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; flex: 1;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("qc_dissox()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Automated QAQC with flag and edit columns"),
            tags$code("wq_drift_correction()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Correct sensor drift using field reference data"),
            tags$code("adjust_logger_NA()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Remove data for ice / dry / disturbance periods")
          )
        ),

        # ---- Right: Station selector & run button (67%) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex; min-width: 0;",
          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1; min-width: 0; overflow-x: auto;",
            tags$div(
              style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; border: 2px solid #1565c0; margin-bottom: 15px;",
              selectInput(ns("station_select"), "Select Station:",
                choices = NULL, width = "100%")
            ),
            tags$h4("QAQC Thresholds", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666; margin-bottom: 10px;",
              icon("sliders-h"),
              " Adjust the thresholds used by ", tags$code("qc_dissox()"),
              ". Default values are shown."
            ),
            fluidRow(
              column(4,
                numericInput(ns("threshold_do_high"), "DO high limit (mg/L)",
                  value = 30, min = 0, max = 100, step = 1)
              ),
              column(4,
                numericInput(ns("threshold_do_low"), "DO low limit (mg/L)",
                  value = 0, min = -10, max = 10, step = 0.5)
              ),
              column(4,
                numericInput(ns("threshold_ice"), "Ice threshold (\u00b0C)",
                  value = 0.3, min = 0, max = 5, step = 0.1)
              )
            ),
            actionButton(ns("run_qaqc"), "Run DO QAQC",
              class = "btn-primary btn-lg", width = "100%",
              style = "color: #fff;",
              icon = icon("play")
            ),

            # Flag summary (appears after QAQC runs)
            conditionalPanel(
              condition = "output.show_results",
              ns = ns,
              tags$div(
                style = "margin-top: 20px; overflow-x: auto;",
                tags$h4("Flag Summary"),
                DT::DTOutput(ns("flag_summary"))
              )
            )
          )
        )
      ),

      # ======== Conditional: Results appear after QAQC runs ========
      conditionalPanel(
        condition = "output.show_results",
        ns = ns,

        # ================================================================
        # Section 2: Drift Correction (collapsible)
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "(Optional) Drift Correction: Warning Experimental"
          )
        ),

        tags$a(
          `data-toggle` = "collapse", href = paste0("#", ns("drift_collapse")),
          style = "display: block; margin-bottom: 10px; color: #1565c0; font-weight: bold; cursor: pointer;",
          icon("chevron-right"), " Show / Hide Drift Correction"
        ),

        tags$div(
          id = ns("drift_collapse"), class = "collapse",

        tags$div(
          style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
          # ---- Left: Explanation (33%) ----
          tags$div(
            style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex; flex-direction: column;",
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("Why Drift Correction?", style = "margin-top: 0;"),
              tags$p(style = "font-size: 13px;",
                "DO sensors drift over time due to membrane degradation, biofouling, and ",
                "aging electrolyte. Field reference measurements taken during site visits ",
                "let you quantify and correct this drift. The correction ramps linearly ",
                "from zero at the start of each segment to the full correction at the ",
                "visit endpoint, then restarts at the next segment. No correction is ",
                "applied after the last visit."
              ),

              tags$h4("Event Type", style = "margin-top: 12px; margin-bottom: 6px;"),
              tags$p(style = "font-size: 13px;",
                "Each reference measurement has an ", tags$code("event_type"),
                " that controls how the drift segment boundaries are determined:"
              ),
              tags$ul(
                style = "padding-left: 20px; font-size: 13px;",
                tags$li(
                  tags$strong("check"),
                  " \u2014 a comparison measurement only (the sensor was not touched). ",
                  "The nearest logger reading to the ", tags$code("timestamp"),
                  " is used as the segment endpoint. The next segment restarts from ",
                  "that same reading."
                ),
                tags$li(
                  tags$strong("calibration"),
                  " \u2014 the sensor was recalibrated during the visit. The last logger ",
                  "reading ", tags$em("before"), " the ", tags$code("calibration_datetime"),
                  " is the segment endpoint (pre-cal drift target). The next segment ",
                  "restarts at the first reading ", tags$em("at or after"), " the ",
                  tags$code("calibration_datetime"),
                  ", because post-calibration values are trusted."
                ),
                tags$li(
                  tags$strong("ignore"),
                  " \u2014 this measurement is skipped entirely and not used in drift correction."
                )
              ),

              tags$h4("Timestamp vs. Calibration Datetime", style = "margin-top: 12px; margin-bottom: 6px;"),
              tags$p(style = "font-size: 13px;",
                "The ", tags$code("timestamp"), " column records when the field reference ",
                "measurement was taken (e.g., a handheld meter reading). For ",
                tags$strong("check"), " events, this is the time matched to the nearest ",
                "logger reading. For ", tags$strong("calibration"), " events, the ",
                tags$code("calibration_datetime"), " column records the moment the sensor ",
                "was actually recalibrated \u2014 this is the time used to split pre-cal ",
                "from post-cal data. Leave ", tags$code("calibration_datetime"),
                " blank for check and ignore events."
              ),

              tags$h4("Method", style = "margin-top: 12px; margin-bottom: 6px;"),
              tags$ul(
                style = "padding-left: 20px; font-size: 13px;",
                tags$li(
                  tags$strong("Ratio"), " (multiplicative, default) \u2014 each logger value is ",
                  "scaled by a factor that ramps linearly from 1.0 at the segment start to ",
                  "the reference/logger ratio at the segment end. Best for %sat where drift ",
                  "is proportional to the reading magnitude. Both ", tags$code("do_percsat_adj"),
                  " and ", tags$code("do_mgl_adj"), " are corrected using the same ratio."
                ),
                tags$li(
                  tags$strong("Offset"), " (additive) \u2014 each value is shifted by an amount ",
                  "that ramps from 0 at the segment start to the reference-minus-logger ",
                  "difference at the segment end. Better when drift is a constant bias ",
                  "independent of reading magnitude."
                )
              ),

              tags$h4("Max Gap (hours)", style = "margin-top: 12px; margin-bottom: 6px;"),
              tags$p(style = "font-size: 13px;",
                "The maximum allowable time difference (in hours) between a reference ",
                "measurement timestamp and the nearest logger reading. If no logger ",
                "reading falls within this window, the reference event is skipped with ",
                "a warning. This prevents incorrect corrections when the reference time ",
                "doesn't align with available logger data (e.g., the logger was out of ",
                "water or timestamps are in different timezones). Default: 1 hour."
              )
            ),
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; flex: 1;",
              tags$h4("Reference Data Format", style = "margin-top: 0;"),
              tags$p(style = "font-size: 13px;", "Required CSV columns:"),
              tags$ul(
                style = "padding-left: 20px; font-size: 12px;",
                tags$li(tags$code("metric"), " \u2014 'do' or 'dissolvedoxygen'"),
                tags$li(tags$code("site_station_code"), " \u2014 station ID"),
                tags$li(tags$code("timestamp"), " \u2014 measurement time (see formats below)"),
                tags$li(tags$code("watertemp_C"), " \u2014 reference temperature"),
                tags$li(tags$code("value"), " \u2014 reference reading (in %sat for DO)"),
                tags$li(tags$code("unit"), " \u2014 '%', '%sat', 'percsat', 'percent', or 'percent saturation'"),
                tags$li(tags$code("event_type"), " \u2014 'check', 'calibration', or 'ignore'"),
                tags$li(tags$code("calibration_datetime"), " \u2014 when sensor was last calibrated (same formats as timestamp; leave blank for check/ignore)")
              ),
              tags$p(style = "font-size: 12px; margin-top: 8px; color: #555;",
                tags$strong("Accepted datetime formats"), " for ",
                tags$code("timestamp"), " and ", tags$code("calibration_datetime"), ":"
              ),
              tags$ul(
                style = "padding-left: 20px; font-size: 12px; color: #555;",
                tags$li(tags$code("YYYY-MM-DD HH:MM:SS"), " \u2014 e.g. 2025-07-20 10:45:00"),
                tags$li(tags$code("YYYY-MM-DD HH:MM"), " \u2014 e.g. 2025-07-20 10:45"),
                tags$li(tags$code("M/D/YYYY HH:MM:SS"), " \u2014 e.g. 7/20/2025 10:45:00"),
                tags$li(tags$code("M/D/YYYY HH:MM"), " \u2014 e.g. 7/20/2025 10:45"),
                tags$li(tags$code("DD-MM-YYYY HH:MM:SS"), " \u2014 e.g. 20-07-2025 10:45:00"),
                tags$li(tags$code("DD-MM-YYYY HH:MM"), " \u2014 e.g. 20-07-2025 10:45")
              )
            )
          ),

          # ---- Right: Upload & controls (67%) ----
          tags$div(
            style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex; min-width: 0;",
            tags$div(
              style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1; min-width: 0; overflow-x: auto;",
              fileInput(ns("ref_upload"), "Upload Reference Data CSV:",
                accept = ".csv", width = "100%"),
              tags$p(style = "font-size: 12px; margin-top: -5px;",
                tags$a(
                  href = "sample_field_references.csv",
                  download = "sample_field_references.csv",
                  icon("download"),
                  " Download sample field references template",
                  style = "color: #1565c0;"
                )
              ),
              uiOutput(ns("ref_upload_status")),

              conditionalPanel(
                condition = "output.show_ref_data",
                ns = ns,
                tags$div(
                  style = "margin-top: 15px;",
                  DT::DTOutput(ns("ref_preview")),
                  tags$div(
                    style = "margin-top: 15px; display: flex; gap: 15px; flex-wrap: wrap; align-items: flex-end;",
                    tags$div(
                      style = "flex: 1; min-width: 150px;",
                      selectInput(ns("drift_method"), "Method:",
                        choices = c("Ratio" = "ratio", "Offset" = "offset"),
                        selected = "ratio", width = "100%")
                    ),
                    tags$div(
                      style = "flex: 1; min-width: 150px;",
                      numericInput(ns("max_ref_gap"), "Max Gap (hours):",
                        value = 1, min = 0.5, max = 24, step = 0.5, width = "100%")
                    ),
                    tags$div(
                      style = "flex: 1; min-width: 150px;",
                      selectInput(ns("drift_station"), "Station:",
                        choices = NULL, width = "100%")
                    )
                  ),
                  actionButton(ns("run_drift"), "Apply Drift Correction",
                    class = "btn-primary", width = "100%",
                    icon = icon("chart-line"),
                    style = "color: #ffffff;"),
                  uiOutput(ns("drift_result_summary"))
                )
              )
            )
          )
        ),

        ), # end drift_collapse div

        # ================================================================
        # Section 3: Manual Adjustments
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Manual Adjustments"
          )
        ),

        tags$p(
          class = "clean-print",
          "Use the plot below to review flagged data. Switch to ",
          tags$strong("Select Range"), " mode and drag across the time series ",
          "to highlight periods that need removal. Use ", tags$strong("Zoom"),
          " mode to navigate and inspect the data more closely."
        ),

        tags$div(
          style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #e0e0e0;",

          tags$div(
            style = "display: flex; flex-direction: column; align-items: center; gap: 10px;",

            # Row 1: Mode toggle
            shinyWidgets::radioGroupButtons(
              inputId = ns("plot_mode"),
              label = NULL,
              choices = c("Zoom" = "zoom", "Select Range" = "select"),
              selected = "zoom",
              size = "lg",
              status = "primary"
            ),

            # Row 2: Range management + Remove button
            tags$div(
              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; justify-content: center;",
              actionButton(ns("undo_range"), "Undo",
                class = "btn-default btn-sm", icon = icon("undo")),
              actionButton(ns("clear_ranges"), "Clear Ranges",
                class = "btn-default btn-sm", icon = icon("times")),
              actionButton(ns("btn_remove"), "Remove Selected",
                class = "btn-warning btn-sm", icon = icon("eraser"),
                style = "color: #fff;")
            ),

            # Row 3: Range status
            uiOutput(ns("range_status"))
          ),

          # Adjustment explanation panel
          tags$div(
            style = "margin-top: 20px; padding: 15px; background-color: #e3f2fd; border-radius: 5px;",
            fluidRow(
              column(4,
                tags$h5("Ice", style = "margin-top: 0; color: #1565c0;"),
                tags$p(style = "font-size: 12px;",
                  "Sensor frozen or encased in ice. Sets DO and optionally ",
                  "temperature to NA."
                )
              ),
              column(4,
                tags$h5("Dry", style = "margin-top: 0; color: #1565c0;"),
                tags$p(style = "font-size: 12px;",
                  "Sensor out of water. Sets DO values to NA; temperature ",
                  "may still be valid."
                )
              ),
              column(4,
                tags$h5("Disturbance", style = "margin-top: 0; color: #1565c0;"),
                tags$p(style = "font-size: 12px;",
                  "Logger moved, disturbed, or otherwise unreliable. ",
                  "Use ", tags$code("keep_temp"), " to retain temperature."
                )
              )
            )
          )
        ),

        # ================================================================
        # Section 4: Visual Review
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Visual Review"
          )
        ),

        tags$div(
          style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #e0e0e0;",
          uiOutput(ns("station_info")),
          tags$div(style = "margin-top: 15px;",
            plotlyOutput(ns("qaqc_plot"), height = "850px") %>% withSpinner()
          ),
          tags$div(style = "margin-top: 15px;",
            plotlyOutput(ns("flag_plot"), height = "250px") %>% withSpinner()
          )
        ),

        # ================================================================
        # Section 5: Station Summary Statistics
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Station Summary Statistics"
          )
        ),

        uiOutput(ns("summary_station_label")),

        fluidRow(
          column(
            width = 4,
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("DO mg/L Summary (raw vs adjusted)", style = "margin-top: 0;"),
              verbatimTextOutput(ns("do_mgl_summary"))
            )
          ),
          column(
            width = 4,
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("DO %sat Summary (raw vs adjusted)", style = "margin-top: 0;"),
              verbatimTextOutput(ns("do_percsat_summary"))
            )
          ),
          column(
            width = 4,
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("Temperature Summary (raw vs adjusted)", style = "margin-top: 0;"),
              verbatimTextOutput(ns("temp_summary"))
            )
          )
        ),

        # ================================================================
        # Section 6: Export Clean Data
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Export Clean Data"
          )
        ),

        tags$div(
          style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
          # ---- Left: Explanation ----
          tags$div(
            style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; flex: 1;",
              tags$h4("Download Results", style = "margin-top: 0;"),
              tags$p(style = "font-size: 13px;",
                "Download the QAQC'd dissolved oxygen data as CSV for the ",
                tags$strong("selected station"), ". The exported file ",
                "contains raw columns, adjusted columns, and all edit/flag ",
                "columns. The QAQC log records every automated and manual ",
                "adjustment applied."
              ),
              tags$p(style = "font-size: 13px; color: #666;",
                "Note: this is the final step in the DO pipeline. Select the ",
                "station you want to export before downloading."
              )
            )
          ),

          # ---- Right: Download buttons ----
          tags$div(
            style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex;",
            tags$div(
              style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1;",
              fluidRow(
                column(4,
                  downloadButton(ns("download_csv"), "Download Clean CSV",
                    class = "btn-primary",
                    style = "width: 100%; color: #ffffff;")
                ),
                column(4,
                  downloadButton(ns("download_log"), "Download QAQC Log",
                    class = "btn-info",
                    style = "width: 100%; color: #ffffff;")
                ),
                column(4,
                  downloadButton(ns("download_metadata"), "Download Metadata",
                    class = "btn-default",
                    style = "width: 100%;")
                )
              )
            )
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_do_qc_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_converted_data <- session$userData$rv_converted_data
    rv_qaqc_data      <- session$userData$rv_qaqc_data
    rv_raw_data        <- session$userData$rv_raw_data

    # Module-local state
    do_qc_result    <- reactiveVal(NULL)
    qaqc_log        <- reactiveVal(NULL)
    drift_ref_data  <- reactiveVal(NULL)

    # Range selection
    selected_ranges <- reactiveVal(list())
    range_history   <- reactiveVal(list())

    # Conditional panel visibility
    output$show_results <- reactive({ !is.null(do_qc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    output$show_ref_data <- reactive({ !is.null(drift_ref_data()) })
    outputOptions(output, "show_ref_data", suspendWhenHidden = FALSE)

    # ------ Populate station selectors from converted data ------
    observe({
      df <- rv_converted_data$do_converted
      req(df, nrow(df) > 0)
      stations <- sort(unique(df$site_station_code))
      stations <- stations[sapply(stations, function(s) {
        any(!is.na(df$do_percsat[df$site_station_code == s]))
      })]
      updateSelectInput(session, "station_select",
        choices = stations, selected = stations[1])
      updateSelectInput(session, "drift_station",
        choices = stations, selected = stations[1])
    })

    # ------ Data readiness status ------
    output$qaqc_status <- renderUI({
      df <- rv_converted_data$do_converted
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No converted DO data available. Complete Part B (Convert DO to %sat) first."
        )
      } else {
        n_stns <- length(unique(df$site_station_code))
        n_recs <- format(nrow(df), big.mark = ",")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Ready: %d station(s), %s records available for QAQC", n_stns, n_recs)
        )
      }
    })

    # ------ Run QAQC ------
    observeEvent(input$run_qaqc, {
      station <- input$station_select
      req(station)
      converted_data <- rv_converted_data$do_converted
      req(converted_data)

      tryCatch({
        showNotification("Running DO QAQC...",
          type = "message", duration = NULL, id = "do_qaqc_progress")

        result <- qc_dissox(
          input_data           = converted_data,
          select_station       = station,
          do_high_limit_mgl    = input$threshold_do_high,
          do_low_limit_mgl     = input$threshold_do_low,
          temp_ice_threshold_C = input$threshold_ice,
          log_root             = NULL,
          user                 = NULL
        )

        qaqc_log(attr(result, "qaqc_log"))

        # Merge: replace station in full dataset
        other_data <- dplyr::filter(converted_data, site_station_code != station)
        full_data <- dplyr::bind_rows(other_data, result) %>%
          dplyr::arrange(site_station_code, timestamp)

        do_qc_result(full_data)
        rv_qaqc_data$do_qaqc <- full_data

        # Reset range state
        selected_ranges(list())
        range_history(list())

        removeNotification("do_qaqc_progress")
        showNotification(sprintf("QAQC complete for %s", station),
          type = "message", duration = 5)

      }, error = function(e) {
        removeNotification("do_qaqc_progress")
        showNotification(paste("QAQC error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Selected station data ------
    selected_data <- reactive({
      req(input$station_select, do_qc_result())
      dplyr::filter(do_qc_result(), site_station_code == input$station_select)
    })

    # ------ Station info ------
    output$station_info <- renderUI({
      data <- selected_data()
      req(nrow(data) > 0)

      flag_cols <- c("ERROR_DO", "OUTLIER_DO", "OUTLIER_WT", "FLAG_ICE", "FLAG_DRY", "FLAG_RANGE")
      flag_cols <- intersect(flag_cols, names(data))
      n_flags <- sum(vapply(flag_cols, function(col) sum(as.logical(data[[col]]), na.rm = TRUE), integer(1)))

      date_range <- paste(
        format(min(data$timestamp), "%Y-%m-%d"),
        "to",
        format(max(data$timestamp), "%Y-%m-%d")
      )

      tags$div(
        style = "padding-top: 8px;",
        tags$p(style = "margin: 0; font-size: 14px;",
          tags$strong("Station: "), input$station_select, " | ",
          tags$strong("Records: "), format(nrow(data), big.mark = ","), " | ",
          tags$strong("Flags: "), format(n_flags, big.mark = ","), " | ",
          tags$strong("Range: "), date_range
        )
      )
    })

    # ------ Flag summary table ------
    output$flag_summary <- DT::renderDT({
      req(do_qc_result())
      data <- selected_data()
      req(nrow(data) > 0)

      safe_sum <- function(col) {
        if (col %in% names(data)) sum(as.logical(data[[col]]), na.rm = TRUE) else 0L
      }

      summary_df <- data.frame(
        Station = data$site_station_code[1],
        Records = nrow(data),
        ERROR_DO = safe_sum("ERROR_DO"),
        OUTLIER_DO = safe_sum("OUTLIER_DO"),
        OUTLIER_WT = safe_sum("OUTLIER_WT"),
        FLAG_ICE = safe_sum("FLAG_ICE"),
        FLAG_DRY = safe_sum("FLAG_DRY"),
        FLAG_RANGE = safe_sum("FLAG_RANGE"),
        DO_mgl_NA = sum(is.na(data$do_mgl_adj)),
        DO_percsat_NA = sum(is.na(data$do_percsat_adj)),
        stringsAsFactors = FALSE
      )

      # Add edit columns if they exist
      edit_cols <- grep("^edit_", names(data), value = TRUE)
      for (ec in edit_cols) {
        summary_df[[ec]] <- sum(data[[ec]], na.rm = TRUE)
      }

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE,
        selection = "none"
      )
    })

    # ------ Reference data upload ------
    observeEvent(input$ref_upload, {
      req(input$ref_upload)
      tryCatch({
        ref <- read.csv(input$ref_upload$datapath, stringsAsFactors = FALSE)

        required_ref_cols <- c("metric", "site_station_code", "timestamp",
                               "watertemp_C", "value", "unit", "event_type",
                               "calibration_datetime")
        missing <- setdiff(required_ref_cols, names(ref))
        if (length(missing) > 0) {
          output$ref_upload_status <- renderUI({
            tags$div(
              style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-top: 10px;",
              icon("exclamation-triangle"),
              sprintf(" Missing required columns: %s", paste(missing, collapse = ", "))
            )
          })
          drift_ref_data(NULL)
          return()
        }

        # Filter to DO rows
        ref <- ref[grepl("^(do|dissolvedoxygen|dissolved oxygen)$",
                         trimws(tolower(ref$metric))), ]
        if (nrow(ref) == 0) {
          output$ref_upload_status <- renderUI({
            tags$div(
              style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-top: 10px;",
              icon("exclamation-triangle"),
              " No DO rows found in reference data. Check the 'metric' column."
            )
          })
          drift_ref_data(NULL)
          return()
        }

        # Validate event_type
        valid_events <- c("check", "calibration", "ignore")
        bad_events <- setdiff(tolower(trimws(ref$event_type)), valid_events)
        if (length(bad_events) > 0) {
          output$ref_upload_status <- renderUI({
            tags$div(
              style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-top: 10px;",
              icon("exclamation-triangle"),
              sprintf(" Invalid event_type values: %s. Must be: check, calibration, or ignore.",
                      paste(bad_events, collapse = ", "))
            )
          })
          drift_ref_data(NULL)
          return()
        }

        drift_ref_data(ref)
        output$ref_upload_status <- renderUI({
          tags$div(
            style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-top: 10px;",
            icon("check-circle"),
            sprintf(" Reference data loaded: %d DO measurements across %d station(s)",
                    nrow(ref), length(unique(ref$site_station_code)))
          )
        })

      }, error = function(e) {
        output$ref_upload_status <- renderUI({
          tags$div(
            style = "color: #721c24; background: #f8d7da; padding: 12px; border-radius: 5px; margin-top: 10px;",
            icon("times-circle"),
            paste(" Error reading CSV:", conditionMessage(e))
          )
        })
        drift_ref_data(NULL)
      })
    })

    # ------ Reference data preview ------
    output$ref_preview <- DT::renderDT({
      req(drift_ref_data())
      DT::datatable(drift_ref_data(),
        options = list(dom = "t", pageLength = 20, scrollX = TRUE),
        rownames = FALSE,
        selection = "none"
      )
    })

    # ------ Run drift correction ------
    observeEvent(input$run_drift, {
      station <- input$drift_station
      req(station)
      req(do_qc_result())
      req(drift_ref_data())

      tryCatch({
        showNotification("Applying drift correction...",
          type = "message", duration = NULL, id = "do_drift_progress")

        drift_messages <- character()
        result <- withCallingHandlers(
          wq_drift_correction(
            input_data     = do_qc_result(),
            ref_data_path  = NULL,
            select_station = station,
            metric         = "do",
            method         = input$drift_method,
            max_ref_gap_h  = input$max_ref_gap,
            log_root       = NULL,
            user           = NULL,
            ref_data       = drift_ref_data()
          ),
          message = function(m) {
            drift_messages <<- c(drift_messages, conditionMessage(m))
            invokeRestart("muffleMessage")
          }
        )

        drift_log <- attr(result, "qaqc_log")

        # Merge station data back
        other_data <- dplyr::filter(do_qc_result(), site_station_code != station)
        full_data <- dplyr::bind_rows(other_data, result) %>%
          dplyr::arrange(site_station_code, timestamp)

        do_qc_result(full_data)
        rv_qaqc_data$do_qaqc <- full_data

        # Append drift log (coerce ts columns to avoid type mismatch)
        if (!is.null(drift_log)) {
          existing <- qaqc_log()
          if (!is.null(existing) && nrow(existing) > 0) {
            existing$ts_start <- as.character(existing$ts_start)
            existing$ts_end   <- as.character(existing$ts_end)
          }
          drift_log$ts_start <- as.character(drift_log$ts_start)
          drift_log$ts_end   <- as.character(drift_log$ts_end)
          qaqc_log(dplyr::bind_rows(existing, drift_log))
        }

        removeNotification("do_drift_progress")
        showNotification(sprintf("Drift correction applied for %s", station),
          type = "message", duration = 5)

        # Show summary
        n_drift <- if ("edit_do_drift" %in% names(result)) {
          sum(result$edit_do_drift, na.rm = TRUE)
        } else { 0L }

        output$drift_result_summary <- renderUI({
          tags$div(
            style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-top: 15px;",
            icon("check-circle"),
            sprintf(" Drift correction complete for %s: %s records corrected.",
                    station, format(n_drift, big.mark = ",")),
            if (length(drift_messages) > 0) {
              tags$div(style = "margin-top: 8px; font-size: 12px; color: #666;",
                tags$em(paste(drift_messages, collapse = " "))
              )
            }
          )
        })

      }, error = function(e) {
        removeNotification("do_drift_progress")
        showNotification(paste("Drift correction error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ QAQC Plot (4-panel time series) ------
    drag_range_js <- sprintf("
      function(el, x) {
        var _resetting = false;
        var _savedRange = null;

        el.on('plotly_relayout', function(evt) {
          if (_resetting) return;

          var keys = Object.keys(evt);
          var isShapeOnly = keys.every(function(k) {
            return k === 'shapes' || k.indexOf('shapes[') === 0;
          });
          if (isShapeOnly) return;

          if (evt['xaxis.autorange'] || evt['yaxis.autorange']) {
            _savedRange = null;
            return;
          }

          var x0 = evt['xaxis.range[0]'];
          var x1 = evt['xaxis.range[1]'];
          if (!x0 || !x1) return;

          var mode = Shiny.shinyapp.$inputValues['%s'];
          if (mode !== 'select') {
            _savedRange = { x0: x0, x1: x1 };
            return;
          }

          Shiny.setInputValue('%s', {
            x0: String(x0), x1: String(x1), nonce: Math.random()
          });

          _resetting = true;
          var restore = _savedRange
            ? { 'xaxis.range[0]': _savedRange.x0, 'xaxis.range[1]': _savedRange.x1 }
            : { 'xaxis.autorange': true };
          Plotly.relayout(el, restore).then(function() { _resetting = false; });
        });

        el.on('plotly_doubleclick', function() { _savedRange = null; });
      }
    ", session$ns("plot_mode"), session$ns("drag_range"))

    # ------ Parsed reference data for plotting (station-filtered) ------
    plot_ref_data <- reactive({
      ref <- drift_ref_data()
      if (is.null(ref)) return(NULL)
      station <- input$station_select
      if (is.null(station)) return(NULL)

      stn_ref <- ref[ref$site_station_code == station, ]
      if (nrow(stn_ref) == 0) return(NULL)

      # Parse timestamps
      parse_orders <- c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "dmy HMS", "dmy HM")
      stn_ref$ts_parsed <- lubridate::parse_date_time(
        stn_ref$timestamp, orders = parse_orders, quiet = TRUE)
      stn_ref <- stn_ref[!is.na(stn_ref$ts_parsed), ]
      if (nrow(stn_ref) == 0) return(NULL)

      stn_ref
    })

    output$qaqc_plot <- renderPlotly({
      data <- selected_data()
      req(nrow(data) > 0)
      station <- input$station_select

      # Reference data for overlay (may be NULL)
      ref <- plot_ref_data()

      # Panel 1: DO mg/L
      p1 <- plot_ly(data, x = ~timestamp, y = ~do_mgl,
              type = "scattergl", mode = "lines",
              line = list(color = "rgba(21,101,192,0.4)", width = 1),
              name = "Raw DO mg/L",
              hovertemplate = "Raw: %{y:.2f} mg/L<extra></extra>") %>%
        add_trace(y = ~do_mgl_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#1565c0", width = 1),
              name = "Adj DO mg/L",
              hovertemplate = "Adj: %{y:.2f} mg/L<extra></extra>")

      # Panel 2: DO %sat
      p2 <- plot_ly(data, x = ~timestamp, y = ~do_percsat,
              type = "scattergl", mode = "lines",
              line = list(color = "rgba(46,125,50,0.4)", width = 1),
              name = "Raw DO %sat",
              hovertemplate = "Raw: %{y:.1f} %%sat<extra></extra>") %>%
        add_trace(y = ~do_percsat_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#2e7d32", width = 1),
              name = "Adj DO %sat",
              hovertemplate = "Adj: %{y:.1f} %%sat<extra></extra>")

      # Overlay %sat reference points on Panel 2
      if (!is.null(ref)) {
        percsat_units <- c("%", "percent", "percent saturation", "%sat", "percsat")
        ref_percsat <- ref[tolower(trimws(ref$unit)) %in% percsat_units, ]
        if (nrow(ref_percsat) > 0) {
          p2 <- p2 %>% add_trace(
            data = ref_percsat, x = ~ts_parsed, y = ~value,
            type = "scatter", mode = "markers",
            inherit = FALSE,
            marker = list(color = "#d32f2f", size = 10, symbol = "diamond",
                         line = list(color = "#ffffff", width = 1.5)),
            name = "Field Ref (%sat)",
            text = ~paste0(event_type, ": ", value, " %sat\n", timestamp),
            hovertemplate = "%{text}<extra></extra>"
          )
        }
      }

      # Panel 3: Water Temperature
      p3 <- plot_ly(data, x = ~timestamp, y = ~watertemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "rgba(97,155,138,0.4)", width = 1),
              name = "Raw Water Temp",
              hovertemplate = "Raw: %{y:.1f} \u00b0C<extra></extra>") %>%
        add_trace(y = ~watertemp_C_do_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#619b8a", width = 1),
              name = "Adj Water Temp",
              hovertemplate = "Adj: %{y:.1f} \u00b0C<extra></extra>")

      # Overlay temperature reference points on Panel 3
      if (!is.null(ref) && nrow(ref) > 0) {
        p3 <- p3 %>% add_trace(
          data = ref, x = ~ts_parsed, y = ~watertemp_C,
          type = "scatter", mode = "markers",
          inherit = FALSE,
          marker = list(color = "#d32f2f", size = 10, symbol = "diamond",
                       line = list(color = "#ffffff", width = 1.5)),
          name = "Field Ref (temp)",
          text = ~paste0(event_type, ": ", watertemp_C, "\u00b0C\n", timestamp),
          hovertemplate = "%{text}<extra></extra>"
        )
      }

      # Panel 4: Air Temperature
      p4 <- plot_ly(data, x = ~timestamp, y = ~airtemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "#FDB840", width = 1),
              name = "Air Temp",
              hovertemplate = "Air: %{y:.1f} \u00b0C<extra></extra>")

      subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = paste("DO QAQC \u2014", station), font = list(size = 14)),
          xaxis = list(title = ""),
          xaxis2 = list(title = ""),
          xaxis3 = list(title = ""),
          xaxis4 = list(title = "Date"),
          yaxis  = list(title = "DO (mg/L)", fixedrange = TRUE),
          yaxis2 = list(title = "DO (%sat)", fixedrange = TRUE),
          yaxis3 = list(title = "Water Temp (\u00b0C)", fixedrange = TRUE),
          yaxis4 = list(title = "Air Temp (\u00b0C)", fixedrange = TRUE),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.08),
          margin = list(b = 80)
        ) %>%
        htmlwidgets::onRender(drag_range_js) %>%
        htmlwidgets::onRender(sprintf("
          function(el, x) {
            var flagId = '%s';
            var modeId = '%s';
            if (!window._doSyncGuard) window._doSyncGuard = false;
            el.on('plotly_relayout', function(evt) {
              if (window._doSyncGuard) return;
              var mode = Shiny.shinyapp.$inputValues[modeId];
              if (mode === 'select') return;
              var keys = Object.keys(evt);
              var isShapeOnly = keys.every(function(k) {
                return k === 'shapes' || k.indexOf('shapes[') === 0;
              });
              if (isShapeOnly) return;
              var flagEl = document.getElementById(flagId);
              if (!flagEl || !flagEl.layout) return;
              window._doSyncGuard = true;
              if (evt['xaxis.autorange']) {
                Plotly.relayout(flagEl, {'xaxis.autorange': true}).then(function() { window._doSyncGuard = false; });
              } else {
                var x0 = evt['xaxis.range[0]'];
                var x1 = evt['xaxis.range[1]'];
                if (x0 && x1) {
                  Plotly.relayout(flagEl, {'xaxis.range[0]': x0, 'xaxis.range[1]': x1}).then(function() { window._doSyncGuard = false; });
                } else { window._doSyncGuard = false; }
              }
            });
            el.on('plotly_doubleclick', function() {
              var flagEl = document.getElementById(flagId);
              if (flagEl) {
                window._doSyncGuard = true;
                Plotly.relayout(flagEl, {'xaxis.autorange': true}).then(function() { window._doSyncGuard = false; });
              }
            });
          }
        ", session$ns("flag_plot"), session$ns("plot_mode")))
    })

    # ------ Flag review plot ------
    output$flag_plot <- renderPlotly({
      data <- selected_data()
      req(nrow(data) > 0)

      flag_cols <- c("ERROR_DO", "OUTLIER_DO", "OUTLIER_WT", "FLAG_ICE", "FLAG_DRY", "FLAG_RANGE")
      edit_cols <- grep("^edit_", names(data), value = TRUE)
      all_flag_cols <- intersect(c(flag_cols, edit_cols), names(data))

      symbols <- c("circle", "triangle-up", "square", "cross", "x",
                   "diamond", "star-triangle-up", "hexagram")

      p <- plot_ly()
      for (i in seq_along(all_flag_cols)) {
        col <- all_flag_cols[i]
        flagged <- data[which(data[[col]] == TRUE), ]
        if (nrow(flagged) > 0) {
          p <- p %>% add_trace(
            data = flagged, x = ~timestamp,
            y = col, type = "scatter", mode = "markers",
            marker = list(color = "#000000", size = 6,
                         symbol = symbols[((i - 1) %% length(symbols)) + 1]),
            name = col
          )
        }
      }

      p %>% layout(
        title = list(text = "QA/QC Flags", font = list(size = 13)),
        xaxis = list(title = "Date"),
        yaxis = list(title = "", categoryorder = "trace"),
        legend = list(orientation = "h", y = -0.3),
        margin = list(b = 80, l = 150)
      ) %>%
        htmlwidgets::onRender(sprintf("
          function(el, x) {
            var mainId = '%s';
            if (!window._doSyncGuard) window._doSyncGuard = false;
            el.on('plotly_relayout', function(evt) {
              if (window._doSyncGuard) return;
              var keys = Object.keys(evt);
              var isShapeOnly = keys.every(function(k) {
                return k === 'shapes' || k.indexOf('shapes[') === 0;
              });
              if (isShapeOnly) return;
              var mainEl = document.getElementById(mainId);
              if (!mainEl || !mainEl.layout) return;
              window._doSyncGuard = true;
              if (evt['xaxis.autorange']) {
                Plotly.relayout(mainEl, {'xaxis.autorange': true}).then(function() { window._doSyncGuard = false; });
              } else {
                var x0 = evt['xaxis.range[0]'];
                var x1 = evt['xaxis.range[1]'];
                if (x0 && x1) {
                  Plotly.relayout(mainEl, {'xaxis.range[0]': x0, 'xaxis.range[1]': x1}).then(function() { window._doSyncGuard = false; });
                } else { window._doSyncGuard = false; }
              }
            });
            el.on('plotly_doubleclick', function() {
              var mainEl = document.getElementById(mainId);
              if (mainEl) {
                window._doSyncGuard = true;
                Plotly.relayout(mainEl, {'xaxis.autorange': true}).then(function() { window._doSyncGuard = false; });
              }
            });
          }
        ", session$ns("qaqc_plot")))
    })

    # ------ Multi-range selection via drag ------
    merge_ranges <- function(ranges) {
      if (length(ranges) <= 1) return(ranges)
      sorted <- ranges[order(sapply(ranges, `[[`, 1))]
      merged <- list(sorted[[1]])
      for (i in seq(2, length(sorted))) {
        last <- merged[[length(merged)]]
        curr <- sorted[[i]]
        if (curr[1] <= last[2]) {
          merged[[length(merged)]] <- c(min(last[1], curr[1]), max(last[2], curr[2]))
        } else {
          merged <- c(merged, list(curr))
        }
      }
      merged
    }

    observeEvent(input$drag_range, {
      info <- input$drag_range
      req(info$x0, info$x1)

      t0 <- as.character(info$x0)
      t1 <- as.character(info$x1)
      if (t0 > t1) { tmp <- t0; t0 <- t1; t1 <- tmp }

      range_history(c(range_history(), list(selected_ranges())))
      new_ranges <- merge_ranges(c(selected_ranges(), list(c(t0, t1))))
      selected_ranges(new_ranges)
    })

    observe({
      ranges <- selected_ranges()
      if (length(ranges) > 0) {
        plot_id <- session$ns("qaqc_plot")
        shapes_json <- jsonlite::toJSON(
          lapply(ranges, function(r) {
            list(type = "rect", xref = "x", yref = "paper",
                 x0 = r[1], x1 = r[2], y0 = 0, y1 = 1,
                 fillcolor = "rgba(255, 165, 0, 0.20)",
                 line = list(color = "rgba(255, 140, 0, 0.8)", width = 1))
          }),
          auto_unbox = TRUE
        )
        shinyjs::runjs(sprintf(
          "var el = document.getElementById('%s');
           if (el && el.layout) { el.layout.shapes = %s; Plotly.redraw(el); }",
          plot_id, shapes_json
        ))
      }
    })

    observeEvent(input$station_select, {
      selected_ranges(list())
      range_history(list())
    })

    observeEvent(input$undo_range, {
      hist <- range_history()
      if (length(hist) > 0) {
        selected_ranges(hist[[length(hist)]])
        range_history(hist[-length(hist)])
      }
    })

    observeEvent(input$clear_ranges, {
      range_history(c(range_history(), list(selected_ranges())))
      selected_ranges(list())
      df <- do_qc_result()
      if (!is.null(df)) {
        attr(df, ".redraw") <- Sys.time()
        do_qc_result(df)
      }
    })

    output$range_status <- renderUI({
      n <- length(selected_ranges())
      if (input$plot_mode == "select") {
        if (n == 0) {
          tags$span(style = "font-size: 13px; color: #666;",
                    "Drag across the plot to select a period")
        } else {
          tags$span(style = "font-size: 13px; color: #e65100; font-weight: bold;",
                    sprintf("%d range%s selected", n, if (n > 1) "s" else ""))
        }
      }
    })

    # ------ Helpers: timestamp parsing, snapping, merge ------
    parse_plotly_ts <- function(ts_char) {
      ts <- lubridate::ymd_hms(ts_char, quiet = TRUE)
      if (!is.na(ts)) return(ts)
      ts <- lubridate::ymd_hm(ts_char, quiet = TRUE)
      if (!is.na(ts)) return(ts)
      ts <- lubridate::ymd(ts_char, quiet = TRUE)
      if (!is.na(ts)) return(ts)
      stripped <- sub("\\.\\d+$", "", ts_char)
      ts <- lubridate::ymd_hms(stripped, quiet = TRUE)
      if (!is.na(ts)) return(ts)
      ts <- tryCatch(as.POSIXct(ts_char), error = function(e) NA)
      ts
    }

    snap_to_data <- function(ts_char, timestamps) {
      ts <- parse_plotly_ts(ts_char)
      if (is.na(ts)) return(ts_char)
      idx <- which.min(abs(as.numeric(difftime(timestamps, ts, units = "secs"))))
      format(timestamps[idx], "%Y-%m-%d %H:%M:%S")
    }

    snap_ranges <- function(ranges, station_timestamps) {
      lapply(ranges, function(r) {
        c(snap_to_data(r[1], station_timestamps),
          snap_to_data(r[2], station_timestamps))
      })
    }

    merge_station_back <- function(full_data, station_data, station) {
      other_data <- dplyr::filter(full_data, site_station_code != station)
      dplyr::bind_rows(other_data, station_data) %>%
        dplyr::arrange(site_station_code, timestamp)
    }

    apply_adjustment_result <- function(station_data, station) {
      updated <- merge_station_back(do_qc_result(), station_data, station)
      selected_ranges(list())
      range_history(list())
      do_qc_result(updated)
      rv_qaqc_data$do_qaqc <- updated

      adj_log <- attr(station_data, "qaqc_log")
      if (!is.null(adj_log)) {
        # Coerce ts_start/ts_end to character before binding — qc_dissox()
        # uses NA_character_ in its no-flag fallback while adjust_logger_NA()
        # returns POSIXct from make_qaqc_log_row().
        existing <- qaqc_log()
        if (!is.null(existing) && nrow(existing) > 0) {
          existing$ts_start <- as.character(existing$ts_start)
          existing$ts_end   <- as.character(existing$ts_end)
        }
        adj_log$ts_start <- as.character(adj_log$ts_start)
        adj_log$ts_end   <- as.character(adj_log$ts_end)
        qaqc_log(dplyr::bind_rows(existing, adj_log))
      }
    }

    # ------ Remove Selected Periods (adjust_logger_NA) ------
    observeEvent(input$btn_remove, {
      ranges <- selected_ranges()
      if (length(ranges) == 0) {
        showNotification("Select at least one range on the plot first.",
          type = "warning", duration = 4)
        return()
      }

      n <- length(ranges)
      showModal(modalDialog(
        title = sprintf("Remove %d Selected Period%s", n, if (n > 1) "s" else ""),
        tags$p(sprintf("Station: %s", input$station_select)),
        tags$ul(
          lapply(ranges, function(r) {
            tags$li(paste(r[1], "to", r[2]))
          })
        ),
        selectInput(session$ns("modal_reason"), "Reason:",
          choices = c("ice", "dry", "disturbance"),
          selected = "disturbance"),
        checkboxInput(session$ns("modal_keep_temp"),
          "Keep water temperature (keep_temp = TRUE)", value = FALSE),
        textInput(session$ns("modal_note"), "Note (required):", value = ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("modal_remove_confirm"), "Confirm Removal",
            class = "btn-warning", icon = icon("eraser"),
            style = "color: #ffffff;")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$modal_remove_confirm, {
      ranges <- selected_ranges()
      req(length(ranges) > 0)
      note <- input$modal_note
      if (is.null(note) || nchar(trimws(note)) == 0) {
        showNotification("Enter a note describing the reason for removal.",
          type = "error", duration = 3)
        return()
      }

      removeModal()
      station <- input$station_select
      req(station)

      tryCatch({
        station_data <- dplyr::filter(do_qc_result(), site_station_code == station)
        snapped <- snap_ranges(ranges, station_data$timestamp)

        for (r in snapped) {
          station_data <- adjust_logger_NA(
            input_data       = station_data,
            select_station   = station,
            metric           = "dissolvedoxygen",
            reason_to_adjust = input$modal_reason,
            timestamp_start  = r[1],
            timestamp_end    = r[2],
            keep_temp        = input$modal_keep_temp,
            manual_note      = paste0(note, " (", r[1], " to ", r[2], ")"),
            log_root         = NULL
          )
        }

        apply_adjustment_result(station_data, station)
        showNotification(
          sprintf("Removed %d period%s for %s", length(ranges),
                  if (length(ranges) > 1) "s" else "", station),
          type = "message", duration = 5)

      }, error = function(e) {
        showNotification(paste("Removal error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Summary stats ------
    output$summary_station_label <- renderUI({
      stn <- input$station_select
      req(stn)
      data <- selected_data()
      req(nrow(data) > 0)
      date_range <- paste(format(min(data$timestamp), "%Y-%m-%d"),
                          "to", format(max(data$timestamp), "%Y-%m-%d"))
      tags$div(
        style = "background-color: #e3f2fd; padding: 12px; border-radius: 5px; border: 2px solid #1565c0; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 14px;",
          "Showing summary statistics for station: ",
          tags$strong(stn, style = "color: #1565c0;"),
          tags$span(style = "color: #555; margin-left: 10px;",
            sprintf("(%s records, %s)", format(nrow(data), big.mark = ","), date_range)
          )
        )
      )
    })

    output$do_mgl_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      n_dup <- sum(duplicated(data$timestamp))
      cat("Records:", nrow(data), " | Duplicate timestamps:", n_dup, "\n\n")
      cat("--- Raw DO (mg/L) ---\n")
      print(round(summary(data$do_mgl), 3))
      cat("\n--- Adjusted DO (mg/L) ---\n")
      if ("do_mgl_adj" %in% names(data)) {
        print(round(summary(data$do_mgl_adj), 3))
      } else {
        cat("(column not yet created)\n")
      }
    })

    output$do_percsat_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      cat("--- Raw DO (%sat) ---\n")
      print(round(summary(data$do_percsat), 1))
      cat("\n--- Adjusted DO (%sat) ---\n")
      if ("do_percsat_adj" %in% names(data)) {
        print(round(summary(data$do_percsat_adj), 1))
      } else {
        cat("(column not yet created)\n")
      }
    })

    output$temp_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      cat("--- Raw Water Temperature (\u00b0C) ---\n")
      print(round(summary(data$watertemp_C), 1))
      cat("\n--- Adjusted Water Temperature (\u00b0C) ---\n")
      if ("watertemp_C_do_adj" %in% names(data)) {
        print(round(summary(data$watertemp_C_do_adj), 1))
      } else {
        cat("(column not yet created)\n")
      }
    })

    # ------ Download: Clean CSV ------
    output$download_csv <- downloadHandler(
      filename = function() {
        station <- input$station_select
        paste0("do_qaqc_", station, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        data <- selected_data()
        req(nrow(data) > 0)

        export_cols <- c(
          "site_station_code", "timestamp",
          "do_mgl", "do_mgl_adj",
          "do_percsat", "do_percsat_adj",
          "watertemp_C", "watertemp_C_do_adj",
          "airpress_kPa", "airtemp_C"
        )
        flag_cols <- grep("^(edit_|flag_|FLAG_|ERROR_|OUTLIER_)", names(data), value = TRUE)
        export_cols <- intersect(c(export_cols, flag_cols), names(data))

        export_df <- data[, export_cols]
        export_df$timestamp <- format(export_df$timestamp, "%Y-%m-%d %H:%M:%S")
        write.csv(export_df, file, row.names = FALSE)
      }
    )

    # ------ Download: QAQC Log ------
    output$download_log <- downloadHandler(
      filename = function() {
        station <- input$station_select
        paste0("do_qaqc_log_", station, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        log <- qaqc_log()
        if (!is.null(log) && nrow(log) > 0) {
          station <- input$station_select
          if ("station" %in% names(log) && !is.null(station)) {
            station_log <- log[log$station == station, ]
            if (nrow(station_log) > 0) {
              write.csv(station_log, file, row.names = FALSE)
              return()
            }
          }
          write.csv(log, file, row.names = FALSE)
        } else {
          write.csv(
            data.frame(
              station = character(), metric = character(),
              field = character(), action = character(),
              n_affected = integer(), note = character(),
              stringsAsFactors = FALSE
            ),
            file, row.names = FALSE
          )
        }
      }
    )

    # ------ Download: Metadata ------
    output$download_metadata <- downloadHandler(
      filename = function() {
        paste0("do_metadata_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        meta <- rv_raw_data$metadata
        if (!is.null(meta) && nrow(meta) > 0) {
          do_meta <- meta[grepl("^(do|dissolvedoxygen|dissolved oxygen)$",
                                tolower(trimws(meta$metric))), ]
          write.csv(do_meta, file, row.names = FALSE)
        } else {
          write.csv(data.frame(), file, row.names = FALSE)
        }
      }
    )

  })
}
