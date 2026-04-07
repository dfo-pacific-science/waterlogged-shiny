# =============================================================================
# Temperature Workflow Modules
# =============================================================================
#
# Workflow for standalone temperature loggers (TidbiT, Pendant) recording
# water or air temperature time series.
#
# This section handles ONLY raw temperature CSV files — not the temperature
# channels embedded in other logger types (those are handled within their
# respective metric workflows: Water Level, Dissolved Oxygen, etc.).
#
# Sub-tabs:
#   1. Import Data          — bind_hobo_files() with metric = "watertemp" or "airtemp"
#   2. Snap Timestamps      — resample_timestamps() to align to regular grid
#   3. Add Nearest Baro     — OVERLAY ONLY: add_nearest_baro() for air temp
#                             comparison — does NOT modify temperature data
#   4. QC Air Temperature   — qc_airtemp() + export
#   5. QC Water Temperature — qc_watertemp() + export
#
# Package functions used:
#   bind_hobo_files()       — parse raw HOBO CSVs (TidbiT, Pendant)
#   resample_timestamps()   — snap timestamps to regular interval grid
#   add_nearest_baro()      — join nearest baro air temp (overlay only)
#   qc_airtemp()            — range-check QAQC for air temperature
#   qc_watertemp()          — ice/dry/negative-temp QAQC for water temperature
#   adjust_logger_NA()      — manual removal of bad data windows
#   write_clean_data()      — export v1.0 CSV (metric = "watertemp" or "airtemp")
#
# Metadata requirements:
#   logger_manager.csv must have metric = "watertemp" or "airtemp" for
#   TidbiT / Pendant deployments.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Import Data
# -----------------------------------------------------------------------------
# nolint start: line_length_linter.
module_temp_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # CSS keyframes for processing spinner animation
    tags$style(HTML("
      @keyframes temp-spin {
        0%   { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    ")),
    shinydashboard::box(
      width = 12,
      tags$div(
        style = "display: flex; flex-wrap: wrap; align-items: flex-start;",
        # ---- Left: header text (2/3 width) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding-right: 20px;",
          tags$h1("Import Temperature Logger Data", class = "custom-h1"),
          tags$h2("Upload raw HOBO TidbiT / Pendant CSV files for water and air temperature",
                  class = "custom-h2"),
          tags$p(
            class = "clean-print",
            "Import raw HOBO CSV files from standalone temperature loggers ",
            "(TidbiT V2, Pendant). These loggers record a single temperature ",
            "channel (\u00b0C). The metadata file determines whether each logger is ",
            "classified as ", tags$code("watertemp"), " (deployed in water) or ",
            tags$code("airtemp"), " (deployed in air). Because ",
            tags$code("bind_hobo_files()"), " processes one metric type per call, ",
            "water and air temperature files are imported separately."
          ),
          tags$div(
            class = "clean-print",
            style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(style = "margin: 0;",
              icon("info-circle"),
              " This tab handles ", tags$strong("standalone temperature loggers only"),
              ". Temperature channels embedded in other logger types (water level, ",
              "dissolved oxygen, conductivity) are processed within their respective ",
              "metric workflows."
            )
          )
        ),
        # ---- Right: logger image (1/3 width) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; text-align: center; padding-top: 10px;",
          tags$img(
            src = "img/toprightpic_temperature.png",
            style = "max-width: 100%; height: auto;",
            alt = "HOBO TidbiT and Pendant temperature loggers"
          )
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
        style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
              tags$li(tags$code("model"), " \u2014 logger model (e.g. TidbiT_V2, Pendant)"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 ",
                      tags$strong("must"), " be ", tags$code("\"watertemp\""), " or ",
                      tags$code("\"airtemp\""),
                      " \u2014 this determines how the logger data is classified and processed"),
              tags$li(tags$code("status"), " \u2014 deployment status"),
              tags$li(tags$code("latitude"), " / ", tags$code("longitude"),
                      " \u2014 decimal degrees",
                      " (e.g. ", tags$code("49.2827"), ", ", tags$code("-123.1207"), ")")
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload your own metadata CSV, or use the pre-loaded sample. ",
              "The file must contain one row per deployment period per logger."
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              tags$a(
                href = "sample_metadata.csv",
                download = "sample_metadata.csv",
                icon("download"),
                " Download sample metadata template",
                style = "color: #00796b;"
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
      # Row 2: Raw Logger Data — Water Temperature
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Raw Logger Data \u2014 Water Temperature"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Instructions (wide) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("How to Import Water Temperature Files", style = "margin-top: 0;"),
            tags$p(
              "Upload the raw CSV files exported from HOBOware for your ",
              "water temperature loggers (TidbiT V2 or Pendant deployed in water). ",
              "You can select multiple files at once across stations and site visits. ",
              "The CSV must contain a ", tags$code("Date Time"), " column in ",
              "HOBOware's default export format: ",
              tags$code("MM/DD/YYYY HH:MM:SS AM/PM"),
              " (e.g. ", tags$code("06/01/2025 02:30:00 PM"), ")."
            ),
            tags$ol(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Ensure metadata is loaded (above) with ", tags$code("metric = \"watertemp\""),
                      " rows so logger serial numbers can be matched."),
              tags$li("Select one or more raw HOBO CSV files from water temp loggers."),
              tags$li("Click ", tags$strong("Bind Water Temp Files"), " to parse ",
                      "and combine the data.")
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              icon("download"), " Download sample raw HOBO water temp files:",
              tags$br(),
              tags$a(
                href = "WT_NT_LEMI_ST_20_20712099_20230414.csv",
                download = "WT_NT_LEMI_ST_20_20712099_20230414.csv",
                style = "color: #00796b; font-size: 12px;",
                "WT_NT_LEMI_ST_20_20712099_20230414.csv"
              ),
              tags$br(),
              tags$a(
                href = "WT_NT_LEMI_ST_20_20712099_20231017.csv",
                download = "WT_NT_LEMI_ST_20_20712099_20231017.csv",
                style = "color: #00796b; font-size: 12px;",
                "WT_NT_LEMI_ST_20_20712099_20231017.csv"
              )
            ),
            tags$p(style = "font-size: 13px; margin-top: 10px;",
              tags$a(
                href = "#", `data-toggle` = "modal",
                `data-target` = "#hoboware_export_modal",
                style = "color: #00796b;",
                icon("question-circle"),
                " How to export CSV files from HOBOware"
              )
            )
          ),

          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
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
                      " (e.g. ", tags$code("LEMI_ST_20"), ")"),
              tags$li(tags$code("timestamp"), " \u2014 date-time in ",
                      tags$code("YYYY-MM-DD HH:MM:SS"), " format",
                      " (e.g. ", tags$code("2025-06-01 14:30:00"), ")"),
              tags$li(tags$code("watertemp_C"), " \u2014 water temperature in \u00b0C",
                      " (e.g. ", tags$code("12.3"), ")"),
              tags$li(tags$code("sn"), " \u2014 logger serial number",
                      " (e.g. ", tags$code("20712099"), ")"),
              tags$li(tags$code("metric"), " \u2014 ", tags$code("\"watertemp\""))
            )
          )
        ),

        # ---- Right: File upload (narrow) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0; flex: 1;",
            tags$h4("Raw Water Temp Logger Files", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload one or more raw HOBO TidbiT / Pendant CSV files ",
              "from loggers deployed in water."
            ),
            fileInput(ns("wt_raw_files"), NULL,
              accept = ".csv",
              multiple = TRUE,
              placeholder = "Choose water temp CSV files...",
              width = "100%"
            ),
            actionButton(ns("wt_bind_btn"), "Bind Water Temp Files",
              class = "btn-primary",
              icon = icon("file-import"),
              width = "100%",
              style = "color: #ffffff;"
            ),
            tags$div(
              id = ns("wt_bind_spinner"),
              style = "display: none; text-align: center; padding: 15px; margin-top: 10px; background-color: #e0f2f1; border-radius: 5px;",
              tags$div(
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #80cbc4; border-top: 3px solid #00796b; border-radius: 50%; animation: temp-spin 0.8s linear infinite; vertical-align: middle;"
              ),
              tags$span(style = "margin-left: 10px; color: #00796b; font-weight: bold;",
                "Processing... binding water temp files")
            ),
            uiOutput(ns("wt_bind_summary"))
          )
        )
      ),

      # ================================================================
      # Row 3: Raw Logger Data — Air Temperature
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Raw Logger Data \u2014 Air Temperature"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Instructions (wide) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("How to Import Air Temperature Files", style = "margin-top: 0;"),
            tags$p(
              "Upload the raw CSV files exported from HOBOware for your ",
              "air temperature loggers (TidbiT V2 or Pendant deployed in air). ",
              "You can select multiple files at once across stations and site visits. ",
              "The CSV must contain a ", tags$code("Date Time"), " column in ",
              "HOBOware's default export format: ",
              tags$code("MM/DD/YYYY HH:MM:SS AM/PM"),
              " (e.g. ", tags$code("06/01/2025 02:30:00 PM"), ")."
            ),
            tags$ol(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Ensure metadata is loaded (above) with ", tags$code("metric = \"airtemp\""),
                      " rows so logger serial numbers can be matched."),
              tags$li("Select one or more raw HOBO CSV files from air temp loggers."),
              tags$li("Click ", tags$strong("Bind Air Temp Files"), " to parse ",
                      "and combine the data.")
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              icon("download"), " Download sample raw HOBO air temp file:",
              tags$br(),
              tags$a(
                href = "WT_HAGG_ST_30_20764236_20230411.csv",
                download = "WT_HAGG_ST_30_20764236_20230411.csv",
                style = "color: #00796b; font-size: 12px;",
                "WT_HAGG_ST_30_20764236_20230411.csv"
              )
            ),
            tags$p(style = "font-size: 13px; margin-top: 10px;",
              tags$a(
                href = "#", `data-toggle` = "modal",
                `data-target` = "#hoboware_export_modal",
                style = "color: #00796b;",
                icon("question-circle"),
                " How to export CSV files from HOBOware"
              )
            )
          ),

          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
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
                      " (e.g. ", tags$code("HAGG_ST_30"), ")"),
              tags$li(tags$code("timestamp"), " \u2014 date-time in ",
                      tags$code("YYYY-MM-DD HH:MM:SS"), " format",
                      " (e.g. ", tags$code("2025-06-01 14:30:00"), ")"),
              tags$li(tags$code("airtemp_C"), " \u2014 air temperature in \u00b0C",
                      " (e.g. ", tags$code("18.4"), ")"),
              tags$li(tags$code("sn"), " \u2014 logger serial number",
                      " (e.g. ", tags$code("20764236"), ")"),
              tags$li(tags$code("metric"), " \u2014 ", tags$code("\"airtemp\""))
            )
          )
        ),

        # ---- Right: File upload (narrow) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0; flex: 1;",
            tags$h4("Raw Air Temp Logger Files", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload one or more raw HOBO TidbiT / Pendant CSV files ",
              "from loggers deployed in air."
            ),
            fileInput(ns("at_raw_files"), NULL,
              accept = ".csv",
              multiple = TRUE,
              placeholder = "Choose air temp CSV files...",
              width = "100%"
            ),
            actionButton(ns("at_bind_btn"), "Bind Air Temp Files",
              class = "btn-primary",
              icon = icon("file-import"),
              width = "100%",
              style = "color: #ffffff;"
            ),
            tags$div(
              id = ns("at_bind_spinner"),
              style = "display: none; text-align: center; padding: 15px; margin-top: 10px; background-color: #e0f2f1; border-radius: 5px;",
              tags$div(
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #80cbc4; border-top: 3px solid #00796b; border-radius: 50%; animation: temp-spin 0.8s linear infinite; vertical-align: middle;"
              ),
              tags$span(style = "margin-left: 10px; color: #00796b; font-weight: bold;",
                "Processing... binding air temp files")
            ),
            uiOutput(ns("at_bind_summary"))
          )
        )
      ),

      # ================================================================
      # Row 4: Snap Timestamps
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Snap Timestamps"
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
              "Round timestamps to a regular interval so multiple temperature ",
              "loggers align. Original timestamps are preserved in ",
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
              column(3,
                selectInput(ns("snap_metric"), "Metric",
                  choices = c("Water Temperature" = "watertemp",
                              "Air Temperature" = "airtemp"),
                  width = "100%"
                )
              ),
              column(3,
                selectInput(ns("snap_station"), "Station",
                  choices = c("All Stations" = "all"),
                  width = "100%"
                )
              ),
              column(3,
                selectInput(ns("snap_interval"), "Interval",
                  choices = c("5 min", "10 min", "15 min", "30 min", "1 hour"),
                  selected = "15 min",
                  width = "100%"
                )
              ),
              column(3,
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
        style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Review Raw Series"
        )
      ),

      tags$p(
        class = "clean-print",
        "Visually inspect the time series plots and summary statistics below ",
        "to look for anomalies such as spikes, flatlines, or gaps in the data. ",
        "Any quality issues identified here will be addressed in the QC steps ",
        "using automated QAQC rules and manual adjustments."
      ),

      # ---- Import Summary (full width) ----
      tags$h4("Import Summary"),
      DT::DTOutput(ns("import_summary")),

      tags$div(style = "margin-top: 15px;"),

      # ---- Preview Plot (full width) ----
      tags$h4("Data Preview"),
      plotlyOutput(ns("preview_plot"), height = "650px") %>% withSpinner(),

      # ---- (Optional) Filter Data by Date Range ----
      tags$div(
        style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
                       style = "color: #fff; background-color: #00796b; border-color: #00796b;")
        ))
      ),

      uiOutput(ns("filter_status")),

      # ---- Next step link ----
      tags$div(
        style = "margin-top: 25px; padding: 15px; background-color: #e0f2f1; border-radius: 5px; text-align: center;",
        tags$p(style = "margin: 0; font-size: 14px;",
          icon("arrow-right"),
          " Ready to proceed? Continue to ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_temp_qc_air']\").tab('show'); return false;",
            style = "color: #00796b; font-weight: bold;",
            "QC Air Temperature"
          ),
          " / ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_temp_qc_water']\").tab('show'); return false;",
            style = "color: #00796b; font-weight: bold;",
            "QC Water Temperature"
          ),
          "."
        )
      )
    ),
    .wireframe_footer()
  )
}
# nolint end


# =============================================================================
# 1. Import Data — Server
# =============================================================================
module_temp_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data   <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data

    # ------ Current data reactives ------
    wt_current <- reactive({ rv_bound_data$watertemp_bound })
    at_current <- reactive({ rv_bound_data$airtemp_bound })

    # ------ Data status display ------
    output$data_status <- renderUI({
      wt_df <- wt_current()
      at_df <- at_current()

      wt_loaded <- !is.null(wt_df) && nrow(wt_df) > 0
      at_loaded <- !is.null(at_df) && nrow(at_df) > 0

      if (!wt_loaded && !at_loaded) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px;",
          icon("info-circle"),
          " No temperature data loaded. Upload water and/or air temperature files below."
        )
      } else {
        parts <- character(0)
        if (wt_loaded) {
          n_wt_stns <- length(unique(wt_df$site_station_code))
          n_wt_recs <- format(nrow(wt_df), big.mark = ",")
          parts <- c(parts, sprintf("Water temp: %d station(s), %s records", n_wt_stns, n_wt_recs))
        }
        if (at_loaded) {
          n_at_stns <- length(unique(at_df$site_station_code))
          n_at_recs <- format(nrow(at_df), big.mark = ",")
          parts <- c(parts, sprintf("Air temp: %d station(s), %s records", n_at_stns, n_at_recs))
        }

        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px;",
          icon("check-circle"),
          paste(" Data loaded.", paste(parts, collapse = ". "))
        )
      }
    })

    # ------ Station map (watertemp + airtemp, colored by metric) ------
    output$station_map <- renderLeaflet({
      metadata <- rv_raw_data$metadata
      req(metadata)

      temp_meta <- metadata[metadata$metric %in% c("watertemp", "airtemp"), ]
      sites <- unique(temp_meta[, c("site_station_code", "latitude", "longitude", "metric")])
      sites <- sites[!is.na(sites$latitude) & !is.na(sites$longitude), ]

      req(nrow(sites) > 0)

      wt_sites <- sites[sites$metric == "watertemp", ]
      at_sites <- sites[sites$metric == "airtemp", ]

      map <- leaflet(sites) %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo")

      if (nrow(wt_sites) > 0) {
        map <- map %>% addCircleMarkers(
          data = wt_sites,
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
          group = "Water Temp"
        )
      }

      if (nrow(at_sites) > 0) {
        map <- map %>% addCircleMarkers(
          data = at_sites,
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
          group = "Air Temp"
        )
      }

      map %>%
        addLegend(
          position = "bottomright",
          colors = c("#1565C0", "#FF8F00"),
          labels = c("Water Temp", "Air Temp"),
          title = "Metric"
        ) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Topo"),
          overlayGroups = c("Water Temp", "Air Temp"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # ------ Station metadata table ------
    output$station_table <- DT::renderDT({
      metadata <- rv_raw_data$metadata
      req(metadata)

      temp_meta <- metadata[metadata$metric %in% c("watertemp", "airtemp"), ]
      req(nrow(temp_meta) > 0)

      display_cols <- intersect(
        c("site_station_code", "timestamp_deploy", "timestamp_remove",
          "model", "sn", "metric", "status", "latitude", "longitude"),
        names(temp_meta)
      )

      DT::datatable(temp_meta[, display_cols],
        options = list(
          dom = "tip",
          pageLength = 20,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # ------ Deployment timeline (dynamic height) ------
    output$deployment_timeline_ui <- renderUI({
      metadata <- rv_raw_data$metadata
      req(metadata)
      n_rows <- sum(metadata$metric %in% c("watertemp", "airtemp"))
      height_px <- max(200, n_rows * 50 + 80)
      plotlyOutput(session$ns("deployment_timeline"),
        height = paste0(height_px, "px")) %>% withSpinner()
    })

    # ------ Deployment timeline (Gantt chart) ------
    output$deployment_timeline <- renderPlotly({
      metadata <- rv_raw_data$metadata
      req(metadata)

      temp_meta <- metadata[metadata$metric %in% c("watertemp", "airtemp"), ]
      req(nrow(temp_meta) > 0)

      # Parse deploy/remove timestamps
      temp_meta$deploy <- as.POSIXct(temp_meta$timestamp_deploy,
        format = "%m/%d/%Y %H:%M", tz = "UTC")
      temp_meta$remove <- as.POSIXct(temp_meta$timestamp_remove,
        format = "%m/%d/%Y %H:%M", tz = "UTC")

      # Order by metric then station then deploy time
      temp_meta <- temp_meta[order(temp_meta$metric,
                                    temp_meta$site_station_code,
                                    temp_meta$deploy), ]

      # Create a unique label per row for the y-axis
      temp_meta$row_label <- paste0(
        temp_meta$site_station_code, " (SN: ", temp_meta$sn, ") [",
        seq_len(nrow(temp_meta)), "]"
      )
      temp_meta$row_label <- factor(temp_meta$row_label,
        levels = rev(temp_meta$row_label))

      # Color by metric
      metric_colors <- c("watertemp" = "#1565C0", "airtemp" = "#FF8F00")

      # Build plot with one trace per metric (for legend grouping)
      p <- plot_ly()
      legend_shown <- character(0)

      for (i in seq_len(nrow(temp_meta))) {
        row <- temp_meta[i, ]
        show_legend <- !(row$metric %in% legend_shown)
        if (show_legend) legend_shown <- c(legend_shown, row$metric)

        label <- if (row$metric == "watertemp") "Water Temp" else "Air Temp"

        p <- p %>% add_trace(
          x = c(row$deploy, row$remove),
          y = c(as.character(row$row_label), as.character(row$row_label)),
          type = "scatter", mode = "lines",
          line = list(width = 16, color = metric_colors[row$metric]),
          name = label,
          legendgroup = row$metric,
          showlegend = show_legend,
          hoverinfo = "text",
          text = paste0(
            "<b>", row$site_station_code, "</b><br>",
            "Metric: ", row$metric, "<br>",
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
                     categoryarray = rev(levels(temp_meta$row_label))),
        legend = list(title = list(text = "Metric"),
                      orientation = "h", y = -0.2),
        margin = list(l = 180, b = 60)
      )
    })

    # ------ Update metadata on upload ------
    observeEvent(input$meta_file, {
      req(input$meta_file)
      tryCatch({
        metadata <- read.csv(input$meta_file$datapath, stringsAsFactors = FALSE)

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

        # Warn if no temperature rows found
        n_wt <- sum(metadata$metric == "watertemp")
        n_at <- sum(metadata$metric == "airtemp")
        if (n_wt == 0 && n_at == 0) {
          found_metrics <- unique(metadata$metric)
          showNotification(
            paste0("Warning: No rows with metric 'watertemp' or 'airtemp' found in metadata. ",
                   "Found metric values: ",
                   if (length(found_metrics) > 0) paste(found_metrics, collapse = ", ") else "none",
                   "."),
            type = "warning", duration = 10
          )
        } else if (n_wt == 0) {
          showNotification(
            "Warning: No rows with metric 'watertemp' found in metadata.",
            type = "warning", duration = 10
          )
        } else if (n_at == 0) {
          showNotification(
            "Warning: No rows with metric 'airtemp' found in metadata.",
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

    # ------ Bind result tracking (water temp) ------
    rv_wt_bind_result <- reactiveValues(
      success = FALSE,
      n_stations = 0,
      n_records = 0,
      matched_stations = character(0),
      unmatched_sns = character(0),
      metadata_stations = character(0)
    )

    # ------ Bind result tracking (air temp) ------
    rv_at_bind_result <- reactiveValues(
      success = FALSE,
      n_stations = 0,
      n_records = 0,
      matched_stations = character(0),
      unmatched_sns = character(0),
      metadata_stations = character(0)
    )

    # ------ Bind Water Temp button ------
    observeEvent(input$wt_bind_btn, {
      raw_files <- input$wt_raw_files
      metadata <- rv_raw_data$metadata

      if (is.null(metadata)) {
        showNotification("No metadata available. Upload a metadata CSV first.",
          type = "error", duration = 5)
        return()
      }

      if (is.null(raw_files)) {
        showNotification("No water temp files selected.",
          type = "warning", duration = 3)
        return()
      }

      shinyjs::show("wt_bind_spinner")
      shinyjs::disable("wt_bind_btn")

      tryCatch({
        result <- bind_hobo_files(
          file_paths = raw_files$datapath,
          metadata = metadata
        )

        bound_data <- result[[1]]

        # Store in reactive values
        rv_bound_data$watertemp_bound <- bound_data

        # Split by station and store in rv_raw_data
        station_list <- split(bound_data, bound_data$site_station_code)
        rv_raw_data$watertemp_data <- as.list(station_list)

        # Track bind results
        bound_stations <- unique(bound_data$site_station_code)
        meta_wt_stations <- unique(
          metadata$site_station_code[metadata$metric == "watertemp"]
        )
        matched <- intersect(bound_stations, meta_wt_stations)
        unmatched_in_data <- setdiff(bound_stations, meta_wt_stations)

        rv_wt_bind_result$success <- TRUE
        rv_wt_bind_result$n_stations <- length(bound_stations)
        rv_wt_bind_result$n_records <- nrow(bound_data)
        rv_wt_bind_result$matched_stations <- matched
        rv_wt_bind_result$unmatched_sns <- unmatched_in_data
        rv_wt_bind_result$metadata_stations <- meta_wt_stations

        shinyjs::hide("wt_bind_spinner")
        shinyjs::enable("wt_bind_btn")

        n_rows <- format(nrow(bound_data), big.mark = ",")
        n_stns <- length(bound_stations)
        showNotification(
          sprintf("Water temp import complete: %s records from %d station(s)", n_rows, n_stns),
          type = "message", duration = 5
        )
      }, error = function(e) {
        shinyjs::hide("wt_bind_spinner")
        shinyjs::enable("wt_bind_btn")
        rv_wt_bind_result$success <- FALSE
        showNotification(paste("Water temp import error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Bind Air Temp button ------
    observeEvent(input$at_bind_btn, {
      raw_files <- input$at_raw_files
      metadata <- rv_raw_data$metadata

      if (is.null(metadata)) {
        showNotification("No metadata available. Upload a metadata CSV first.",
          type = "error", duration = 5)
        return()
      }

      if (is.null(raw_files)) {
        showNotification("No air temp files selected.",
          type = "warning", duration = 3)
        return()
      }

      shinyjs::show("at_bind_spinner")
      shinyjs::disable("at_bind_btn")

      tryCatch({
        result <- bind_hobo_files(
          file_paths = raw_files$datapath,
          metadata = metadata
        )

        bound_data <- result[[1]]

        # Store in reactive values
        rv_bound_data$airtemp_bound <- bound_data

        # Split by station and store in rv_raw_data
        station_list <- split(bound_data, bound_data$site_station_code)
        rv_raw_data$airtemp_data <- as.list(station_list)

        # Track bind results
        bound_stations <- unique(bound_data$site_station_code)
        meta_at_stations <- unique(
          metadata$site_station_code[metadata$metric == "airtemp"]
        )
        matched <- intersect(bound_stations, meta_at_stations)
        unmatched_in_data <- setdiff(bound_stations, meta_at_stations)

        rv_at_bind_result$success <- TRUE
        rv_at_bind_result$n_stations <- length(bound_stations)
        rv_at_bind_result$n_records <- nrow(bound_data)
        rv_at_bind_result$matched_stations <- matched
        rv_at_bind_result$unmatched_sns <- unmatched_in_data
        rv_at_bind_result$metadata_stations <- meta_at_stations

        shinyjs::hide("at_bind_spinner")
        shinyjs::enable("at_bind_btn")

        n_rows <- format(nrow(bound_data), big.mark = ",")
        n_stns <- length(bound_stations)
        showNotification(
          sprintf("Air temp import complete: %s records from %d station(s)", n_rows, n_stns),
          type = "message", duration = 5
        )
      }, error = function(e) {
        shinyjs::hide("at_bind_spinner")
        shinyjs::enable("at_bind_btn")
        rv_at_bind_result$success <- FALSE
        showNotification(paste("Air temp import error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Water temp bind summary display ------
    output$wt_bind_summary <- renderUI({
      if (!rv_wt_bind_result$success) return(NULL)

      n_stns <- rv_wt_bind_result$n_stations
      n_recs <- format(rv_wt_bind_result$n_records, big.mark = ",")
      matched <- rv_wt_bind_result$matched_stations
      unmatched <- rv_wt_bind_result$unmatched_sns
      meta_stns <- rv_wt_bind_result$metadata_stations
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
        tags$h5("Water Temp Import Summary", style = "margin-top: 0; margin-bottom: 8px;"),
        tags$ul(
          style = "padding-left: 20px; font-size: 13px; margin-bottom: 0;",
          items
        )
      )
    })

    # ------ Air temp bind summary display ------
    output$at_bind_summary <- renderUI({
      if (!rv_at_bind_result$success) return(NULL)

      n_stns <- rv_at_bind_result$n_stations
      n_recs <- format(rv_at_bind_result$n_records, big.mark = ",")
      matched <- rv_at_bind_result$matched_stations
      unmatched <- rv_at_bind_result$unmatched_sns
      meta_stns <- rv_at_bind_result$metadata_stations
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
        tags$h5("Air Temp Import Summary", style = "margin-top: 0; margin-bottom: 8px;"),
        tags$ul(
          style = "padding-left: 20px; font-size: 13px; margin-bottom: 0;",
          items
        )
      )
    })

    # ------ Update snap station selector when metric changes ------
    observe({
      metric <- input$snap_metric
      req(metric)

      df <- if (metric == "watertemp") wt_current() else at_current()

      if (!is.null(df) && nrow(df) > 0) {
        stations <- unique(df$site_station_code)
        choices <- c("All Stations" = "all", setNames(stations, stations))
      } else {
        choices <- c("All Stations" = "all")
      }
      updateSelectInput(session, "snap_station", choices = choices)
    })

    # ------ Snap timestamps button ------
    observeEvent(input$snap_btn, {
      metric <- input$snap_metric
      req(metric)

      df <- if (metric == "watertemp") wt_current() else at_current()
      req(df)

      tryCatch({
        showNotification("Snapping timestamps...", type = "message",
          duration = NULL, id = "snap_progress")

        station_arg <- if (input$snap_station == "all") NULL else input$snap_station

        # Use appropriate metric label for resample_timestamps
        metric_label <- if (metric == "watertemp") "WATERTEMP" else "AIRTEMP"

        result <- resample_timestamps(
          input_data = df,
          interval = input$snap_interval,
          align = input$snap_align,
          select_station = station_arg,
          metric = metric_label
        )

        if (metric == "watertemp") {
          rv_bound_data$watertemp_bound <- result
        } else {
          rv_bound_data$airtemp_bound <- result
        }

        removeNotification("snap_progress")
        label <- if (metric == "watertemp") "water temp" else "air temp"
        showNotification(
          sprintf("Timestamps snapped for %s.", label),
          type = "message", duration = 3
        )
      }, error = function(e) {
        removeNotification("snap_progress")
        showNotification(paste("Snap error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Import summary table ------
    output$import_summary <- DT::renderDT({
      wt_df <- wt_current()
      at_df <- at_current()
      req(!is.null(wt_df) || !is.null(at_df))

      summary_parts <- list()

      if (!is.null(wt_df) && nrow(wt_df) > 0) {
        wt_summary <- wt_df %>%
          dplyr::group_by(site_station_code) %>%
          dplyr::summarise(
            Metric = "watertemp",
            Records = dplyr::n(),
            `Start Date` = format(min(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
            `End Date` = format(max(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
            `Temp Min (C)` = round(min(watertemp_C, na.rm = TRUE), 1),
            `Temp Max (C)` = round(max(watertemp_C, na.rm = TRUE), 1),
            .groups = "drop"
          )
        summary_parts <- c(summary_parts, list(wt_summary))
      }

      if (!is.null(at_df) && nrow(at_df) > 0) {
        at_summary <- at_df %>%
          dplyr::group_by(site_station_code) %>%
          dplyr::summarise(
            Metric = "airtemp",
            Records = dplyr::n(),
            `Start Date` = format(min(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
            `End Date` = format(max(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
            `Temp Min (C)` = round(min(airtemp_C, na.rm = TRUE), 1),
            `Temp Max (C)` = round(max(airtemp_C, na.rm = TRUE), 1),
            .groups = "drop"
          )
        summary_parts <- c(summary_parts, list(at_summary))
      }

      summary_df <- dplyr::bind_rows(summary_parts)

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ------ Preview plot ------
    output$preview_plot <- renderPlotly({
      wt_df <- wt_current()
      at_df <- at_current()

      wt_loaded <- !is.null(wt_df) && nrow(wt_df) > 0
      at_loaded <- !is.null(at_df) && nrow(at_df) > 0

      req(wt_loaded || at_loaded)

      if (wt_loaded && at_loaded) {
        # Two-panel subplot
        p1 <- plot_ly()
        for (stn in unique(wt_df$site_station_code)) {
          stn_data <- dplyr::filter(wt_df, site_station_code == stn)
          p1 <- p1 %>% add_trace(
            data = stn_data, x = ~timestamp, y = ~watertemp_C,
            type = "scattergl", mode = "lines",
            line = list(width = 1),
            name = paste(stn, "- Water"),
            hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
          )
        }

        p2 <- plot_ly()
        for (stn in unique(at_df$site_station_code)) {
          stn_data <- dplyr::filter(at_df, site_station_code == stn)
          p2 <- p2 %>% add_trace(
            data = stn_data, x = ~timestamp, y = ~airtemp_C,
            type = "scattergl", mode = "lines",
            line = list(width = 1),
            name = paste(stn, "- Air"),
            hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
          )
        }

        subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
          layout(
            title = list(text = "Temperature Data Preview", font = list(size = 14)),
            xaxis2 = list(title = "Date"),
            yaxis = list(title = "Water Temp (\u00b0C)"),
            yaxis2 = list(title = "Air Temp (\u00b0C)"),
            hovermode = "x unified",
            legend = list(orientation = "h", y = -0.15),
            margin = list(b = 80)
          )
      } else if (wt_loaded) {
        # Single panel: water temp only
        p <- plot_ly()
        for (stn in unique(wt_df$site_station_code)) {
          stn_data <- dplyr::filter(wt_df, site_station_code == stn)
          p <- p %>% add_trace(
            data = stn_data, x = ~timestamp, y = ~watertemp_C,
            type = "scattergl", mode = "lines",
            line = list(width = 1),
            name = stn,
            hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
          )
        }
        p %>% layout(
          title = list(text = "Water Temperature Preview", font = list(size = 14)),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Water Temp (\u00b0C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15),
          margin = list(b = 80)
        )
      } else {
        # Single panel: air temp only
        p <- plot_ly()
        for (stn in unique(at_df$site_station_code)) {
          stn_data <- dplyr::filter(at_df, site_station_code == stn)
          p <- p %>% add_trace(
            data = stn_data, x = ~timestamp, y = ~airtemp_C,
            type = "scattergl", mode = "lines",
            line = list(width = 1),
            name = stn,
            hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
          )
        }
        p %>% layout(
          title = list(text = "Air Temperature Preview", font = list(size = 14)),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Air Temp (\u00b0C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15),
          margin = list(b = 80)
        )
      }
    })

    # ---- Date Range Filter: auto-populate ----
    observe({
      wt <- rv_bound_data$watertemp_bound
      at <- rv_bound_data$airtemp_bound
      timestamps <- c(
        if (!is.null(wt) && nrow(wt) > 0) wt$timestamp,
        if (!is.null(at) && nrow(at) > 0) at$timestamp
      )
      req(length(timestamps) > 0)
      ts_range <- range(timestamps, na.rm = TRUE)
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

      wt <- rv_bound_data$watertemp_bound
      if (!is.null(wt) && nrow(wt) > 0)
        rv_bound_data$watertemp_bound <- wt[wt$timestamp >= from_ts & wt$timestamp <= to_ts, ]

      at <- rv_bound_data$airtemp_bound
      if (!is.null(at) && nrow(at) > 0)
        rv_bound_data$airtemp_bound <- at[at$timestamp >= from_ts & at$timestamp <= to_ts, ]

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
        wt <- rv_bound_data$watertemp_bound
        at <- rv_bound_data$airtemp_bound
        n_wt <- if (!is.null(wt)) nrow(wt) else 0
        n_at <- if (!is.null(at)) nrow(at) else 0
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;",
          icon("check-circle"),
          sprintf(" Filter applied: %s to %s. Water temp: %s records. Air temp: %s records.",
                  format(from_date), format(to_date),
                  format(n_wt, big.mark = ","), format(n_at, big.mark = ","))
        )
      })
    })

  })
}


# =============================================================================
# 2. QC Water Temperature — UI
# =============================================================================
module_temp_qc_water_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,

      # ---- Header + Description ----
      tags$h1("QC Water Temperature", class = "custom-h1"),
      tags$h2("Automated quality checks for standalone water temperature loggers",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Run automated quality checks on water temperature data from standalone ",
        "loggers (TidbiT, Pendant). The function flags possible ice and dry conditions, ",
        "corrects small negative temperatures to 0\u00b0C, and removes clearly impossible ",
        "values (< -1\u00b0C). Raw data is preserved \u2014 corrections are applied to the ",
        tags$code("watertemp_C_adj"), " column only."
      ),

      uiOutput(ns("qaqc_status")),

      hr(),


      # ================================================================
      # Section 1: Run Automated QAQC
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
              tags$li(tags$strong("OUTLIER_WT"), " \u2014 water temp < -1\u00b0C \u2192 set to NA; -1\u00b0C < water temp < 0\u00b0C \u2192 corrected to 0\u00b0C"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 water temp < ice flag threshold (\u00b0C) \u2192 flagged for review"),
              tags$li(tags$strong("FLAG_DRY"), " \u2014 water temp > dry flag threshold (\u00b0C) or air-water temperature difference > flag threshold (\u00b0C) \u2192 flagged for review")
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Flags vs Edits", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              tags$code("flag_*"), " = data flagged for review, no automatic adjustments applied", tags$br(),
              tags$code("edit_*"), " = data adjusted (set to NA or corrected)"
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; flex: 1;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("qc_watertemp()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Applies automatic qc rules for water temperature"),
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
              style = "background-color: #e0f2f1; padding: 10px; border-radius: 5px; border: 2px solid #00796b; margin-bottom: 15px;",
              selectInput(ns("station_select"), "Select Station:",
                choices = NULL, width = "100%")
            ),

            tags$div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; margin-bottom: 15px;",
              tags$div(
                style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("ice_threshold"), "Ice flag threshold (\u00b0C):",
                  value = 0.3, min = 0, max = 5, step = 0.1, width = "100%")
              ),
              tags$div(
                style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("high_dry_C"), "Dry flag threshold (\u00b0C):",
                  value = 19, min = 10, max = 40, step = 1, width = "100%")
              ),
              tags$div(
                style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("air_water_diff"), "Air-water temperature diff threshold (\u00b0C):",
                  value = 2, min = 0.5, max = 10, step = 0.5, width = "100%")
              )
            ),

            actionButton(ns("run_qaqc"), "Run Water Temp QAQC",
              class = "btn-primary btn-lg", width = "100%",
              style = "color: #fff; background-color: #00796b; border-color: #00796b;",
              icon = icon("play")
            ),

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

      conditionalPanel(
        condition = "output.show_results",
        ns = ns,

        tags$div(
          style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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

          uiOutput(ns("station_info")),

          tags$div(style = "margin-top: 15px;",
            plotlyOutput(ns("qaqc_plot"), height = "550px") %>% withSpinner()
          ),

          tags$div(
            style = "display: flex; flex-direction: column; align-items: center; gap: 10px; margin-top: 15px;",

            shinyWidgets::radioGroupButtons(
              inputId = ns("plot_mode"),
              label = NULL,
              choices = c("Zoom" = "zoom", "Select Range" = "select"),
              selected = "zoom",
              size = "lg",
              status = "primary"
            ),

            tags$div(
              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; justify-content: center;",
              actionButton(ns("undo_range"), "Undo",
                class = "btn-default btn-sm", icon = icon("undo")),
              actionButton(ns("clear_ranges"), "Clear Ranges",
                class = "btn-default btn-sm", icon = icon("times")),
              actionButton(ns("apply_ranges"), "Remove Selected",
                class = "btn-warning btn-sm", icon = icon("eraser"),
                style = "color: #fff;")
            ),

            uiOutput(ns("range_status"))
          ),

          tags$div(
            style = "margin-top: 20px; padding: 15px; background-color: #f0f0f0; border-radius: 5px;",
            fluidRow(
              column(4,
                tags$h5("Ice", style = "margin-top: 0; color: #00796b;"),
                tags$p(style = "font-size: 12px;",
                  "Sensor frozen or encased in ice. Sets water temperature to NA."
                )
              ),
              column(4,
                tags$h5("Dry", style = "margin-top: 0; color: #00796b;"),
                tags$p(style = "font-size: 12px;",
                  "Sensor out of water. Temperature readings may be air temperature ",
                  "rather than water temperature."
                )
              ),
              column(4,
                tags$h5("Disturbance", style = "margin-top: 0; color: #00796b;"),
                tags$p(style = "font-size: 12px;",
                  "Logger moved, disturbed, or otherwise unreliable."
                )
              )
            )
          )
        ),

        tags$div(
          style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Station Summary Statistics"
          )
        ),

        uiOutput(ns("summary_station_label")),

        tags$div(
          style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
          tags$h4("Temperature Summary (raw vs adjusted)", style = "margin-top: 0;"),
          verbatimTextOutput(ns("temp_summary"))
        ),

        tags$div(
          style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Export Clean Data"
          )
        ),

        tags$div(
          style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
          tags$div(
            style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; flex: 1;",
              tags$h4("Download Results", style = "margin-top: 0;"),
              tags$p(style = "font-size: 13px;",
                "Download the QAQC'd water temperature data as CSV for the ",
                tags$strong("selected station"), "."
              )
            )
          ),
          tags$div(
            style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex;",
            tags$div(
              style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1;",
              fluidRow(
                column(4,
                  downloadButton(ns("download_csv"), "Download Clean CSV",
                    class = "btn-primary",
                    style = "width: 100%; color: #ffffff; background-color: #00796b; border-color: #00796b;")
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


# =============================================================================
# 2. QC Water Temperature -- Server
# =============================================================================
module_temp_qc_water_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data   <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data
    rv_qaqc_data  <- session$userData$rv_qaqc_data

    wt_qc_result    <- reactiveVal(NULL)
    qaqc_log        <- reactiveVal(NULL)
    selected_ranges <- reactiveVal(list())
    range_history   <- reactiveVal(list())

    output$show_results <- reactive({ !is.null(wt_qc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    wt_input_data <- reactive({
      d <- rv_bound_data$watertemp_bound
      if (!is.null(d) && nrow(d) > 0) return(d)
      NULL
    })

    observe({
      df <- wt_input_data()
      req(df, nrow(df) > 0)
      stations <- sort(unique(df$site_station_code))
      stations <- stations[sapply(stations, function(s) {
        any(!is.na(df$watertemp_C[df$site_station_code == s]))
      })]
      updateSelectInput(session, "station_select",
        choices = stations, selected = stations[1])
    })

    output$qaqc_status <- renderUI({
      df <- wt_input_data()
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No water temperature data available. Import and bind water temperature data first."
        )
      } else {
        n_stns <- length(unique(df$site_station_code))
        n_recs <- format(nrow(df), big.mark = ",")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Ready: %d station(s), %s records", n_stns, n_recs)
        )
      }
    })

    observeEvent(input$run_qaqc, {
      station <- input$station_select
      req(station)
      input_df <- wt_input_data()
      req(input_df)

      tryCatch({
        showNotification("Running water temperature QAQC...",
          type = "message", duration = NULL, id = "wt_qc_progress")

        result <- qc_watertemp(
          input_data            = input_df,
          select_station        = station,
          temp_ice_flag_C       = input$ice_threshold,
          temp_dry_flag_C       = input$high_dry_C,
          temp_air_water_diff_C = input$air_water_diff
        )

        qaqc_log(attr(result, "qaqc_log"))

        other_data <- dplyr::filter(input_df, site_station_code != station)
        full_data <- dplyr::bind_rows(other_data, result) %>%
          dplyr::arrange(site_station_code, timestamp)

        wt_qc_result(full_data)
        rv_qaqc_data$qc_watertemp <- full_data

        selected_ranges(list())
        range_history(list())

        removeNotification("wt_qc_progress")
        showNotification(
          sprintf("Water temp QAQC complete for %s", station),
          type = "message", duration = 5)

      }, error = function(e) {
        removeNotification("wt_qc_progress")
        showNotification(paste("QAQC error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    selected_data <- reactive({
      req(input$station_select, wt_qc_result())
      dplyr::filter(wt_qc_result(), site_station_code == input$station_select)
    })

    output$station_info <- renderUI({
      data <- selected_data()
      req(nrow(data) > 0)
      flag_cols <- intersect(c("FLAG_ICE", "FLAG_DRY", "OUTLIER_WT"), names(data))
      n_flags <- sum(vapply(flag_cols, function(col) sum(as.logical(data[[col]]), na.rm = TRUE), integer(1)))
      date_range <- paste(format(min(data$timestamp), "%Y-%m-%d"), "to", format(max(data$timestamp), "%Y-%m-%d"))
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

    output$flag_summary <- DT::renderDT({
      req(wt_qc_result())
      data <- selected_data()
      req(nrow(data) > 0)
      safe_sum <- function(col) if (col %in% names(data)) sum(as.logical(data[[col]]), na.rm = TRUE) else 0L
      summary_df <- data.frame(
        Station = data$site_station_code[1], Records = nrow(data),
        FLAG_ICE = safe_sum("FLAG_ICE"),
        FLAG_DRY = safe_sum("FLAG_DRY"),
        OUTLIER_WT = safe_sum("OUTLIER_WT"),
        Temp_NA_adj = sum(is.na(data$watertemp_C_adj)),
        stringsAsFactors = FALSE
      )
      DT::datatable(summary_df, options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE, selection = "none")
    })

    drag_range_js <- sprintf("
      function(el, x) {
        var _resetting = false;
        var _savedRange = null;
        el.on('plotly_relayout', function(evt) {
          if (_resetting) return;
          var keys = Object.keys(evt);
          var isShapeOnly = keys.every(function(k) { return k === 'shapes' || k.indexOf('shapes[') === 0; });
          if (isShapeOnly) return;
          if (evt['xaxis.autorange'] || evt['yaxis.autorange']) { _savedRange = null; return; }
          var x0 = evt['xaxis.range[0]']; var x1 = evt['xaxis.range[1]'];
          if (!x0 || !x1) return;
          var mode = Shiny.shinyapp.$inputValues['%s'];
          if (mode !== 'select') { _savedRange = { x0: x0, x1: x1 }; return; }
          Shiny.setInputValue('%s', { x0: String(x0), x1: String(x1), nonce: Math.random() });
          _resetting = true;
          var restore = _savedRange ? { 'xaxis.range[0]': _savedRange.x0, 'xaxis.range[1]': _savedRange.x1 } : { 'xaxis.autorange': true };
          Plotly.relayout(el, restore).then(function() { _resetting = false; });
        });
        el.on('plotly_doubleclick', function() { _savedRange = null; });
      }
    ", session$ns("plot_mode"), session$ns("drag_range"))

    output$qaqc_plot <- renderPlotly({
      data <- selected_data()
      req(nrow(data) > 0)
      station <- input$station_select

      p <- plot_ly(data, x = ~timestamp, y = ~watertemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "#90CAF9", width = 1),
              name = "Raw Water Temp",
              hovertemplate = "Raw: %{y:.1f} \u00b0C<extra></extra>") %>%
        add_trace(y = ~watertemp_C_adj, type = "scattergl", mode = "lines",
              line = list(color = "#1565C0", width = 1),
              name = "Adjusted Water Temp",
              hovertemplate = "Adj: %{y:.1f} \u00b0C<extra></extra>")

      ice_flags <- data[which(as.logical(data$FLAG_ICE)), ]
      dry_flags <- data[which(as.logical(data$FLAG_DRY)), ]
      outlier_wt <- data[which(as.logical(data$OUTLIER_WT)), ]

      if (nrow(ice_flags) > 0)
        p <- p %>% add_trace(data = ice_flags, x = ~timestamp, y = ~watertemp_C,
          type = "scatter", mode = "markers", marker = list(color = "#00BCD4", size = 5, symbol = "diamond"),
          line = list(width = 0), name = "FLAG_ICE", hovertemplate = "ICE: %{y:.1f} \u00b0C<extra></extra>")
      if (nrow(dry_flags) > 0)
        p <- p %>% add_trace(data = dry_flags, x = ~timestamp, y = ~watertemp_C,
          type = "scatter", mode = "markers", marker = list(color = "#FF9800", size = 5, symbol = "triangle-up"),
          line = list(width = 0), name = "FLAG_DRY", hovertemplate = "DRY: %{y:.1f} \u00b0C<extra></extra>")
      if (nrow(outlier_wt) > 0)
        p <- p %>% add_trace(data = outlier_wt, x = ~timestamp, y = ~watertemp_C,
          type = "scatter", mode = "markers", marker = list(color = "#D32F2F", size = 5, symbol = "circle"),
          line = list(width = 0), name = "OUTLIER_WT", hovertemplate = "OUTLIER: %{y:.1f} \u00b0C<extra></extra>")

      p %>% layout(
        title = list(text = paste("Water Temp QAQC \u2014", station), font = list(size = 14)),
        xaxis = list(title = "Date"), yaxis = list(title = "Temperature (\u00b0C)", fixedrange = TRUE),
        hovermode = "x unified", legend = list(orientation = "h", y = -0.15), margin = list(b = 80)
      ) %>% htmlwidgets::onRender(drag_range_js)
    })

    parse_plotly_ts <- function(ts_char) {
      ts <- lubridate::ymd_hms(ts_char, quiet = TRUE); if (!is.na(ts)) return(ts)
      ts <- lubridate::ymd_hm(ts_char, quiet = TRUE);  if (!is.na(ts)) return(ts)
      ts <- lubridate::ymd(ts_char, quiet = TRUE);      if (!is.na(ts)) return(ts)
      stripped <- sub("\\.\\d+$", "", ts_char)
      ts <- lubridate::ymd_hms(stripped, quiet = TRUE);  if (!is.na(ts)) return(ts)
      tryCatch(as.POSIXct(ts_char), error = function(e) NA)
    }
    snap_to_data <- function(ts_char, timestamps) {
      ts <- parse_plotly_ts(ts_char); if (is.na(ts)) return(ts_char)
      idx <- which.min(abs(as.numeric(difftime(timestamps, ts, units = "secs"))))
      format(timestamps[idx], "%Y-%m-%d %H:%M:%S")
    }
    snap_ranges <- function(ranges, station_timestamps) {
      lapply(ranges, function(r) c(snap_to_data(r[1], station_timestamps), snap_to_data(r[2], station_timestamps)))
    }
    merge_ranges <- function(ranges) {
      if (length(ranges) <= 1) return(ranges)
      sorted <- ranges[order(sapply(ranges, `[[`, 1))]
      merged <- list(sorted[[1]])
      for (i in seq(2, length(sorted))) {
        last <- merged[[length(merged)]]; curr <- sorted[[i]]
        if (curr[1] <= last[2]) merged[[length(merged)]] <- c(min(last[1], curr[1]), max(last[2], curr[2]))
        else merged <- c(merged, list(curr))
      }
      merged
    }

    observeEvent(input$drag_range, {
      info <- input$drag_range; req(info$x0, info$x1)
      t0 <- as.character(info$x0); t1 <- as.character(info$x1)
      if (t0 > t1) { tmp <- t0; t0 <- t1; t1 <- tmp }
      range_history(c(range_history(), list(selected_ranges())))
      selected_ranges(merge_ranges(c(selected_ranges(), list(c(t0, t1)))))
    })

    observe({
      ranges <- selected_ranges()
      if (length(ranges) > 0) {
        plot_id <- session$ns("qaqc_plot")
        shapes_json <- jsonlite::toJSON(lapply(ranges, function(r) {
          list(type = "rect", xref = "x", yref = "paper", x0 = r[1], x1 = r[2], y0 = 0, y1 = 1,
               fillcolor = "rgba(255, 165, 0, 0.20)", line = list(color = "rgba(255, 140, 0, 0.8)", width = 1))
        }), auto_unbox = TRUE)
        shinyjs::runjs(sprintf("var el = document.getElementById('%s'); if (el && el.layout) { el.layout.shapes = %s; Plotly.redraw(el); }", plot_id, shapes_json))
      }
    })

    observeEvent(input$station_select, { selected_ranges(list()); range_history(list()) })
    observeEvent(input$undo_range, {
      hist <- range_history()
      if (length(hist) > 0) { selected_ranges(hist[[length(hist)]]); range_history(hist[-length(hist)]) }
    })
    observeEvent(input$clear_ranges, {
      range_history(c(range_history(), list(selected_ranges()))); selected_ranges(list())
      df <- wt_qc_result(); if (!is.null(df)) { attr(df, ".redraw") <- Sys.time(); wt_qc_result(df) }
    })

    output$range_status <- renderUI({
      n <- length(selected_ranges())
      if (input$plot_mode == "select") {
        if (n == 0) tags$span(style = "font-size: 13px; color: #666;", "Drag across the plot to select a period")
        else tags$span(style = "font-size: 13px; color: #00796b; font-weight: bold;", sprintf("%d range%s selected", n, if (n > 1) "s" else ""))
      }
    })

    observeEvent(input$apply_ranges, {
      ranges <- selected_ranges()
      if (length(ranges) == 0) { showNotification("Select at least one range on the plot first.", type = "warning", duration = 4); return() }
      n <- length(ranges)
      showModal(modalDialog(
        title = sprintf("Remove %d Selected Period%s", n, if (n > 1) "s" else ""),
        tags$p(sprintf("Station: %s", input$station_select)),
        tags$ul(lapply(ranges, function(r) tags$li(paste(r[1], "to", r[2])))),
        selectInput(session$ns("modal_reason"), "Reason:", choices = c("ice", "dry", "disturbance"), selected = "disturbance"),
        textInput(session$ns("modal_note"), "Note (required):", value = ""),
        footer = tagList(modalButton("Cancel"),
          actionButton(session$ns("modal_confirm"), "Confirm Removal", class = "btn-warning", icon = icon("eraser"), style = "color: #ffffff;")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$modal_confirm, {
      ranges <- selected_ranges(); req(length(ranges) > 0)
      note <- input$modal_note
      if (is.null(note) || nchar(trimws(note)) == 0) { showNotification("Enter a note.", type = "error", duration = 3); return() }
      removeModal(); station <- input$station_select; req(station)
      tryCatch({
        station_data <- dplyr::filter(wt_qc_result(), site_station_code == station)
        snapped <- snap_ranges(ranges, station_data$timestamp)
        for (r in snapped) {
          station_data <- adjust_logger_NA(
            input_data = station_data, select_station = station, metric = "watertemp",
            reason_to_adjust = input$modal_reason, timestamp_start = r[1], timestamp_end = r[2],
            keep_temp = FALSE, manual_note = paste0(note, " (", r[1], " to ", r[2], ")"), log_root = NULL)
        }
        other_data <- dplyr::filter(wt_qc_result(), site_station_code != station)
        updated <- dplyr::bind_rows(other_data, station_data) %>% dplyr::arrange(site_station_code, timestamp)
        adj_log <- attr(station_data, "qaqc_log")
        if (!is.null(adj_log)) {
          existing <- qaqc_log()
          if (!is.null(existing) && nrow(existing) > 0) { existing$ts_start <- as.character(existing$ts_start); existing$ts_end <- as.character(existing$ts_end) }
          adj_log$ts_start <- as.character(adj_log$ts_start); adj_log$ts_end <- as.character(adj_log$ts_end)
          qaqc_log(dplyr::bind_rows(existing, adj_log))
        }
        selected_ranges(list()); range_history(list()); wt_qc_result(updated); rv_qaqc_data$qc_watertemp <- updated
        showNotification(sprintf("Removed %d period%s for %s", length(ranges), if (length(ranges) > 1) "s" else "", station), type = "message", duration = 5)
      }, error = function(e) { showNotification(paste("Removal error:", conditionMessage(e)), type = "error", duration = 8) })
    })

    output$summary_station_label <- renderUI({
      stn <- input$station_select; req(stn); data <- selected_data(); req(nrow(data) > 0)
      date_range <- paste(format(min(data$timestamp), "%Y-%m-%d"), "to", format(max(data$timestamp), "%Y-%m-%d"))
      tags$div(style = "background-color: #e0f2f1; padding: 12px; border-radius: 5px; border: 2px solid #00796b; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 14px;", "Showing summary statistics for station: ",
          tags$strong(stn, style = "color: #00796b;"),
          tags$span(style = "color: #555; margin-left: 10px;", sprintf("(%s records, %s)", format(nrow(data), big.mark = ","), date_range))))
    })

    output$temp_summary <- renderPrint({
      data <- selected_data(); req(nrow(data) > 0)
      cat("Records:", nrow(data), " | Duplicate timestamps:", sum(duplicated(data$timestamp)), "\n\n")
      cat("--- Raw Water Temperature (\u00b0C) ---\n"); print(round(summary(data$watertemp_C), 1))
      cat("\n--- Adjusted Water Temperature (\u00b0C) ---\n")
      if ("watertemp_C_adj" %in% names(data)) print(round(summary(data$watertemp_C_adj), 1)) else cat("(column not yet created)\n")
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("watertemp_qaqc_", input$station_select, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        data <- selected_data(); req(nrow(data) > 0)
        export_cols <- c("site_station_code", "timestamp", "watertemp_C", "watertemp_C_adj")
        flag_cols <- grep("^(edit_|flag_)", names(data), value = TRUE)
        export_cols <- intersect(c(export_cols, flag_cols), names(data))
        export_df <- data[, export_cols]; export_df$timestamp <- format(export_df$timestamp, "%Y-%m-%d %H:%M:%S")
        write.csv(export_df, file, row.names = FALSE)
      })
    output$download_log <- downloadHandler(
      filename = function() paste0("watertemp_qaqc_log_", input$station_select, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        log <- qaqc_log()
        if (!is.null(log) && nrow(log) > 0) write.csv(log, file, row.names = FALSE)
        else write.csv(data.frame(station = character(), metric = character(), field = character(), action = character(), n_affected = integer(), note = character(), stringsAsFactors = FALSE), file, row.names = FALSE)
      })
    output$download_metadata <- downloadHandler(
      filename = function() paste0("watertemp_metadata_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        meta <- rv_raw_data$metadata
        if (!is.null(meta) && nrow(meta) > 0) write.csv(meta[grepl("^watertemp$", tolower(trimws(meta$metric))), ], file, row.names = FALSE)
        else write.csv(data.frame(), file, row.names = FALSE)
      })
  })
}


# =============================================================================
# 3. QC Air Temperature -- UI
# =============================================================================
module_temp_qc_air_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,

      tags$h1("QC Air Temperature", class = "custom-h1"),
      tags$h2("Range-check quality control for standalone air temperature loggers",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Run automated range-check quality control on air temperature data from ",
        "standalone loggers. Values outside the specified low/high limits are set ",
        "to NA in the ", tags$code("airtemp_C_adj"), " column. Raw data is preserved."
      ),

      uiOutput(ns("qaqc_status")),

      hr(),

      tags$div(
        style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Run Automated QAQC"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex; flex-direction: column;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("QAQC Rules", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("OUTLIER_AT"), " \u2014 low limit \u00b0C < air temp < high limit \u00b0C  \u2192 set to NA"),
              tags$li(tags$strong("FLAG_RANGE"), " \u2014 low limit \u00b0C < air temp < high limit \u00b0C  \u2192 flagged for review")
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; flex: 1;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("qc_airtemp()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;", "Applies automatic qc for air temperature"),
            tags$code("adjust_logger_NA()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;", "Remove data for disturbance periods")
          )
        ),

        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex; min-width: 0;",
          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1; min-width: 0; overflow-x: auto;",

            tags$div(
              style = "background-color: #e0f2f1; padding: 10px; border-radius: 5px; border: 2px solid #00796b; margin-bottom: 15px;",
              selectInput(ns("station_select"), "Select Station:", choices = NULL, width = "100%")
            ),

            tags$div(
              style = "display: grid; grid-template-columns: repeat(2, 2fr); gap: 12px; margin-bottom: 15px;",
              tags$div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("temp_low_flag"), "Low flag (\u00b0C):", value = -40, min = -60, max = 0, step = 1, width = "100%")),
              tags$div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("temp_high_flag"), "High flag (\u00b0C):", value = 50, min = 20, max = 80, step = 1, width = "100%"))
              tags$div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("temp_low"), "Low limit (\u00b0C):", value = -40, min = -60, max = 0, step = 1, width = "100%")),
              tags$div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("temp_high"), "High limit (\u00b0C):", value = 50, min = 20, max = 80, step = 1, width = "100%"))
            ),

            actionButton(ns("run_qaqc"), "Run Air Temp QAQC",
              class = "btn-primary btn-lg", width = "100%",
              style = "color: #fff; background-color: #00796b; border-color: #00796b;",
              icon = icon("play")),

            conditionalPanel(condition = "output.show_results", ns = ns,
              tags$div(style = "margin-top: 20px; overflow-x: auto;",
                tags$h4("Flag Summary"), DT::DTOutput(ns("flag_summary"))))
          )
        )
      ),

      conditionalPanel(
        condition = "output.show_results", ns = ns,

        tags$div(style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
          tags$span(style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;", "Manual Adjustments")),

        tags$p(class = "clean-print", "Use the plot below to review flagged data. Switch to ",
          tags$strong("Select Range"), " mode and drag across the time series to highlight periods that need removal."),

        tags$div(
          style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #e0e0e0;",
          uiOutput(ns("station_info")),
          tags$div(style = "margin-top: 15px;", plotlyOutput(ns("qaqc_plot"), height = "550px") %>% withSpinner()),
          tags$div(style = "display: flex; flex-direction: column; align-items: center; gap: 10px; margin-top: 15px;",
            shinyWidgets::radioGroupButtons(inputId = ns("plot_mode"), label = NULL,
              choices = c("Zoom" = "zoom", "Select Range" = "select"), selected = "zoom", size = "lg", status = "primary"),
            tags$div(style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; justify-content: center;",
              actionButton(ns("undo_range"), "Undo", class = "btn-default btn-sm", icon = icon("undo")),
              actionButton(ns("clear_ranges"), "Clear Ranges", class = "btn-default btn-sm", icon = icon("times")),
              actionButton(ns("apply_ranges"), "Remove Selected", class = "btn-warning btn-sm", icon = icon("eraser"), style = "color: #fff;")),
            uiOutput(ns("range_status"))),
          tags$div(style = "margin-top: 20px; padding: 15px; background-color: #f0f0f0; border-radius: 5px;",
            tags$h5("Disturbance", style = "margin-top: 0; color: #00796b;"),
            tags$p(style = "font-size: 12px;", "Logger moved, disturbed, or otherwise unreliable. Sets air temperature to NA for the selected period."))
        ),

        tags$div(style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
          tags$span(style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;", "Station Summary Statistics")),
        uiOutput(ns("summary_station_label")),
        tags$div(style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
          tags$h4("Temperature Summary (raw vs adjusted)", style = "margin-top: 0;"),
          verbatimTextOutput(ns("temp_summary"))),

        tags$div(style = "border-top: 2px solid #00796b; margin: 25px 0 20px 0; position: relative;",
          tags$span(style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #00796b; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;", "Export Clean Data")),
        tags$div(style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
          tags$div(style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
            tags$div(style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; flex: 1;",
              tags$h4("Download Results", style = "margin-top: 0;"),
              tags$p(style = "font-size: 13px;", "Download the QAQC'd air temperature data as CSV for the ", tags$strong("selected station"), "."))),
          tags$div(style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex;",
            tags$div(style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1;",
              fluidRow(
                column(4, downloadButton(ns("download_csv"), "Download Clean CSV", class = "btn-primary",
                  style = "width: 100%; color: #ffffff; background-color: #00796b; border-color: #00796b;")),
                column(4, downloadButton(ns("download_log"), "Download QAQC Log", class = "btn-info", style = "width: 100%; color: #ffffff;")),
                column(4, downloadButton(ns("download_metadata"), "Download Metadata", class = "btn-default", style = "width: 100%;"))))))
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 3. QC Air Temperature -- Server
# =============================================================================
module_temp_qc_air_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data   <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data
    rv_qaqc_data  <- session$userData$rv_qaqc_data

    at_qc_result    <- reactiveVal(NULL)
    qaqc_log        <- reactiveVal(NULL)
    selected_ranges <- reactiveVal(list())
    range_history   <- reactiveVal(list())

    output$show_results <- reactive({ !is.null(at_qc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    at_input_data <- reactive({
      d <- rv_bound_data$airtemp_bound
      if (!is.null(d) && nrow(d) > 0) return(d)
      NULL
    })

    observe({
      df <- at_input_data()
      req(df, nrow(df) > 0)
      stations <- sort(unique(df$site_station_code))
      stations <- stations[sapply(stations, function(s) any(!is.na(df$airtemp_C[df$site_station_code == s])))]
      updateSelectInput(session, "station_select", choices = stations, selected = stations[1])
    })

    output$qaqc_status <- renderUI({
      df <- at_input_data()
      if (is.null(df) || nrow(df) == 0) {
        tags$div(style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"), " No air temperature data available. Import and bind air temperature data first.")
      } else {
        n_stns <- length(unique(df$site_station_code)); n_recs <- format(nrow(df), big.mark = ",")
        tags$div(style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"), sprintf(" Ready: %d station(s), %s records", n_stns, n_recs))
      }
    })

    observeEvent(input$run_qaqc, {
      station <- input$station_select; req(station); input_df <- at_input_data(); req(input_df)
      tryCatch({
        showNotification("Running air temperature QAQC...", type = "message", duration = NULL, id = "at_qc_progress")
        result <- qc_airtemp(input_data = input_df,
                             select_station = station,
                             temp_low_limit_C = input$temp_low,
                             temp_high_limit_C = input$temp_high,
                             temp_low_flag_C = input$temp_low_flag,
                             temp_high_flag_C = input$temp_high_flag)
        qaqc_log(attr(result, "qaqc_log"))
        other_data <- dplyr::filter(input_df, site_station_code != station)
        full_data <- dplyr::bind_rows(other_data, result) %>% dplyr::arrange(site_station_code, timestamp)
        at_qc_result(full_data); rv_qaqc_data$qc_airtemp <- full_data
        selected_ranges(list()); range_history(list())
        removeNotification("at_qc_progress")
        showNotification(sprintf("Air temp QAQC complete for %s", station), type = "message", duration = 5)
      }, error = function(e) {
        removeNotification("at_qc_progress")
        showNotification(paste("QAQC error:", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    selected_data <- reactive({
      req(input$station_select, at_qc_result())
      dplyr::filter(at_qc_result(), site_station_code == input$station_select)
    })

    output$station_info <- renderUI({
      data <- selected_data(); req(nrow(data) > 0)
      n_flags <- if ("OUTLIER_AT" %in% names(data)) sum(as.logical(data$OUTLIER_AT), na.rm = TRUE) else 0L
      date_range <- paste(format(min(data$timestamp), "%Y-%m-%d"), "to", format(max(data$timestamp), "%Y-%m-%d"))
      tags$div(style = "padding-top: 8px;",
        tags$p(style = "margin: 0; font-size: 14px;",
          tags$strong("Station: "), input$station_select, " | ",
          tags$strong("Records: "), format(nrow(data), big.mark = ","), " | ",
          tags$strong("Flags: "), format(n_flags, big.mark = ","), " | ",
          tags$strong("Range: "), date_range))
    })

    output$flag_summary <- DT::renderDT({
      req(at_qc_result()); data <- selected_data(); req(nrow(data) > 0)
      safe_sum <- function(col) if (col %in% names(data)) sum(as.logical(data[[col]]), na.rm = TRUE) else 0L
      summary_df <- data.frame(Station = data$site_station_code[1], Records = nrow(data),
        OUTLIER_AT = safe_sum("OUTLIER_AT"), Temp_NA_adj = sum(is.na(data$airtemp_C_adj)), stringsAsFactors = FALSE)
      DT::datatable(summary_df, options = list(dom = "t", pageLength = 50, scrollX = TRUE), rownames = FALSE, selection = "none")
    })

    drag_range_js <- sprintf("
      function(el, x) {
        var _resetting = false; var _savedRange = null;
        el.on('plotly_relayout', function(evt) {
          if (_resetting) return;
          var keys = Object.keys(evt);
          var isShapeOnly = keys.every(function(k) { return k === 'shapes' || k.indexOf('shapes[') === 0; });
          if (isShapeOnly) return;
          if (evt['xaxis.autorange'] || evt['yaxis.autorange']) { _savedRange = null; return; }
          var x0 = evt['xaxis.range[0]']; var x1 = evt['xaxis.range[1]'];
          if (!x0 || !x1) return;
          var mode = Shiny.shinyapp.$inputValues['%s'];
          if (mode !== 'select') { _savedRange = { x0: x0, x1: x1 }; return; }
          Shiny.setInputValue('%s', { x0: String(x0), x1: String(x1), nonce: Math.random() });
          _resetting = true;
          var restore = _savedRange ? { 'xaxis.range[0]': _savedRange.x0, 'xaxis.range[1]': _savedRange.x1 } : { 'xaxis.autorange': true };
          Plotly.relayout(el, restore).then(function() { _resetting = false; });
        });
        el.on('plotly_doubleclick', function() { _savedRange = null; });
      }
    ", session$ns("plot_mode"), session$ns("drag_range"))

    output$qaqc_plot <- renderPlotly({
      data <- selected_data(); req(nrow(data) > 0); station <- input$station_select
      p <- plot_ly(data, x = ~timestamp, y = ~airtemp_C, type = "scattergl", mode = "lines",
              line = list(color = "#FFCC80", width = 1), name = "Raw Air Temp",
              hovertemplate = "Raw: %{y:.1f} \u00b0C<extra></extra>") %>%
        add_trace(y = ~airtemp_C_adj, type = "scattergl", mode = "lines",
              line = list(color = "#E65100", width = 1), name = "Adjusted Air Temp",
              hovertemplate = "Adj: %{y:.1f} \u00b0C<extra></extra>")
      temp_flags <- data[which(as.logical(data$OUTLIER_AT)), ]
      if (nrow(temp_flags) > 0)
        p <- p %>% add_trace(data = temp_flags, x = ~timestamp, y = ~airtemp_C,
          type = "scatter", mode = "markers", marker = list(color = "#D32F2F", size = 5, symbol = "circle"),
          line = list(width = 0), name = "OUTLIER_AT", hovertemplate = "RANGE: %{y:.1f} \u00b0C<extra></extra>")
      p %>% layout(title = list(text = paste("Air Temp QAQC \u2014", station), font = list(size = 14)),
        xaxis = list(title = "Date"), yaxis = list(title = "Temperature (\u00b0C)", fixedrange = TRUE),
        hovermode = "x unified", legend = list(orientation = "h", y = -0.15), margin = list(b = 80)) %>%
        htmlwidgets::onRender(drag_range_js)
    })

    parse_plotly_ts <- function(ts_char) {
      ts <- lubridate::ymd_hms(ts_char, quiet = TRUE); if (!is.na(ts)) return(ts)
      ts <- lubridate::ymd_hm(ts_char, quiet = TRUE);  if (!is.na(ts)) return(ts)
      ts <- lubridate::ymd(ts_char, quiet = TRUE);      if (!is.na(ts)) return(ts)
      stripped <- sub("\\.\\d+$", "", ts_char)
      ts <- lubridate::ymd_hms(stripped, quiet = TRUE);  if (!is.na(ts)) return(ts)
      tryCatch(as.POSIXct(ts_char), error = function(e) NA)
    }
    snap_to_data <- function(ts_char, timestamps) {
      ts <- parse_plotly_ts(ts_char); if (is.na(ts)) return(ts_char)
      idx <- which.min(abs(as.numeric(difftime(timestamps, ts, units = "secs"))))
      format(timestamps[idx], "%Y-%m-%d %H:%M:%S")
    }
    snap_ranges <- function(ranges, station_timestamps) {
      lapply(ranges, function(r) c(snap_to_data(r[1], station_timestamps), snap_to_data(r[2], station_timestamps)))
    }
    merge_ranges <- function(ranges) {
      if (length(ranges) <= 1) return(ranges)
      sorted <- ranges[order(sapply(ranges, `[[`, 1))]
      merged <- list(sorted[[1]])
      for (i in seq(2, length(sorted))) {
        last <- merged[[length(merged)]]; curr <- sorted[[i]]
        if (curr[1] <= last[2]) merged[[length(merged)]] <- c(min(last[1], curr[1]), max(last[2], curr[2]))
        else merged <- c(merged, list(curr))
      }
      merged
    }

    observeEvent(input$drag_range, {
      info <- input$drag_range; req(info$x0, info$x1)
      t0 <- as.character(info$x0); t1 <- as.character(info$x1)
      if (t0 > t1) { tmp <- t0; t0 <- t1; t1 <- tmp }
      range_history(c(range_history(), list(selected_ranges())))
      selected_ranges(merge_ranges(c(selected_ranges(), list(c(t0, t1)))))
    })

    observe({
      ranges <- selected_ranges()
      if (length(ranges) > 0) {
        plot_id <- session$ns("qaqc_plot")
        shapes_json <- jsonlite::toJSON(lapply(ranges, function(r) {
          list(type = "rect", xref = "x", yref = "paper", x0 = r[1], x1 = r[2], y0 = 0, y1 = 1,
               fillcolor = "rgba(255, 165, 0, 0.20)", line = list(color = "rgba(255, 140, 0, 0.8)", width = 1))
        }), auto_unbox = TRUE)
        shinyjs::runjs(sprintf("var el = document.getElementById('%s'); if (el && el.layout) { el.layout.shapes = %s; Plotly.redraw(el); }", plot_id, shapes_json))
      }
    })

    observeEvent(input$station_select, { selected_ranges(list()); range_history(list()) })
    observeEvent(input$undo_range, {
      hist <- range_history()
      if (length(hist) > 0) { selected_ranges(hist[[length(hist)]]); range_history(hist[-length(hist)]) }
    })
    observeEvent(input$clear_ranges, {
      range_history(c(range_history(), list(selected_ranges()))); selected_ranges(list())
      df <- at_qc_result(); if (!is.null(df)) { attr(df, ".redraw") <- Sys.time(); at_qc_result(df) }
    })

    output$range_status <- renderUI({
      n <- length(selected_ranges())
      if (input$plot_mode == "select") {
        if (n == 0) tags$span(style = "font-size: 13px; color: #666;", "Drag across the plot to select a period")
        else tags$span(style = "font-size: 13px; color: #00796b; font-weight: bold;", sprintf("%d range%s selected", n, if (n > 1) "s" else ""))
      }
    })

    observeEvent(input$apply_ranges, {
      ranges <- selected_ranges()
      if (length(ranges) == 0) { showNotification("Select at least one range on the plot first.", type = "warning", duration = 4); return() }
      n <- length(ranges)
      showModal(modalDialog(
        title = sprintf("Remove %d Selected Period%s", n, if (n > 1) "s" else ""),
        tags$p(sprintf("Station: %s", input$station_select)),
        tags$ul(lapply(ranges, function(r) tags$li(paste(r[1], "to", r[2])))),
        tags$div(tags$label("Reason:"),
          tags$input(type = "text", value = "disturbance", readonly = "readonly", class = "form-control",
            style = "background-color: #e9ecef; cursor: not-allowed;")),
        textInput(session$ns("modal_note"), "Note (required):", value = ""),
        footer = tagList(modalButton("Cancel"),
          actionButton(session$ns("modal_confirm"), "Confirm Removal", class = "btn-warning", icon = icon("eraser"), style = "color: #ffffff;")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$modal_confirm, {
      ranges <- selected_ranges(); req(length(ranges) > 0)
      note <- input$modal_note
      if (is.null(note) || nchar(trimws(note)) == 0) { showNotification("Enter a note.", type = "error", duration = 3); return() }
      removeModal(); station <- input$station_select; req(station)
      tryCatch({
        station_data <- dplyr::filter(at_qc_result(), site_station_code == station)
        snapped <- snap_ranges(ranges, station_data$timestamp)
        for (r in snapped) {
          station_data <- adjust_logger_NA(
            input_data = station_data, select_station = station, metric = "airtemp",
            reason_to_adjust = "disturbance", timestamp_start = r[1], timestamp_end = r[2],
            keep_temp = FALSE, manual_note = paste0(note, " (", r[1], " to ", r[2], ")"), log_root = NULL)
        }
        other_data <- dplyr::filter(at_qc_result(), site_station_code != station)
        updated <- dplyr::bind_rows(other_data, station_data) %>% dplyr::arrange(site_station_code, timestamp)
        adj_log <- attr(station_data, "qaqc_log")
        if (!is.null(adj_log)) {
          existing <- qaqc_log()
          if (!is.null(existing) && nrow(existing) > 0) { existing$ts_start <- as.character(existing$ts_start); existing$ts_end <- as.character(existing$ts_end) }
          adj_log$ts_start <- as.character(adj_log$ts_start); adj_log$ts_end <- as.character(adj_log$ts_end)
          qaqc_log(dplyr::bind_rows(existing, adj_log))
        }
        selected_ranges(list()); range_history(list()); at_qc_result(updated); rv_qaqc_data$qc_airtemp <- updated
        showNotification(sprintf("Removed %d period%s for %s", length(ranges), if (length(ranges) > 1) "s" else "", station), type = "message", duration = 5)
      }, error = function(e) { showNotification(paste("Removal error:", conditionMessage(e)), type = "error", duration = 8) })
    })

    output$summary_station_label <- renderUI({
      stn <- input$station_select; req(stn); data <- selected_data(); req(nrow(data) > 0)
      date_range <- paste(format(min(data$timestamp), "%Y-%m-%d"), "to", format(max(data$timestamp), "%Y-%m-%d"))
      tags$div(style = "background-color: #e0f2f1; padding: 12px; border-radius: 5px; border: 2px solid #00796b; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 14px;", "Showing summary statistics for station: ",
          tags$strong(stn, style = "color: #00796b;"),
          tags$span(style = "color: #555; margin-left: 10px;", sprintf("(%s records, %s)", format(nrow(data), big.mark = ","), date_range))))
    })

    output$temp_summary <- renderPrint({
      data <- selected_data(); req(nrow(data) > 0)
      cat("Records:", nrow(data), " | Duplicate timestamps:", sum(duplicated(data$timestamp)), "\n\n")
      cat("--- Raw Air Temperature (\u00b0C) ---\n"); print(round(summary(data$airtemp_C), 1))
      cat("\n--- Adjusted Air Temperature (\u00b0C) ---\n")
      if ("airtemp_C_adj" %in% names(data)) print(round(summary(data$airtemp_C_adj), 1)) else cat("(column not yet created)\n")
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("airtemp_qaqc_", input$station_select, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        data <- selected_data(); req(nrow(data) > 0)
        export_cols <- c("site_station_code", "timestamp", "airtemp_C", "airtemp_C_adj")
        flag_cols <- grep("^(edit_|flag_)", names(data), value = TRUE)
        export_cols <- intersect(c(export_cols, flag_cols), names(data))
        export_df <- data[, export_cols]; export_df$timestamp <- format(export_df$timestamp, "%Y-%m-%d %H:%M:%S")
        write.csv(export_df, file, row.names = FALSE)
      })
    output$download_log <- downloadHandler(
      filename = function() paste0("airtemp_qaqc_log_", input$station_select, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        log <- qaqc_log()
        if (!is.null(log) && nrow(log) > 0) write.csv(log, file, row.names = FALSE)
        else write.csv(data.frame(station = character(), metric = character(), field = character(), action = character(), n_affected = integer(), note = character(), stringsAsFactors = FALSE), file, row.names = FALSE)
      })
    output$download_metadata <- downloadHandler(
      filename = function() paste0("airtemp_metadata_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        meta <- rv_raw_data$metadata
        if (!is.null(meta) && nrow(meta) > 0) write.csv(meta[grepl("^airtemp$", tolower(trimws(meta$metric))), ], file, row.names = FALSE)
        else write.csv(data.frame(), file, row.names = FALSE)
      })
  })
}


# -----------------------------------------------------------------------------
# Shared footer helper (not exported)
# -----------------------------------------------------------------------------
.wireframe_footer <- function() {
  fluidRow(column(
    width = 12,
    tags$p(
      "Waterlogged Water Data Processing Application",
      style = "text-align: center; color: #8A939A; margin-top: 10px; font-size: 12px;"
    )
  ))
}

