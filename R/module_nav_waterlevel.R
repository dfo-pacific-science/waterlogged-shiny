# =============================================================================
# Water Level (Pressure) Workflow Modules
# =============================================================================
#
# Full pipeline for HOBO U20 water level loggers:
#   1. Import Data           — bind_hobo_files() + snap timestamps
#   2. Convert Data Metrics  — add_nearest_baro() + convert_waterlevel_kPa_m()
#   3. QA Water Level        — qc_waterlevel() + visual review
#   (Manual adjustments are integrated into QA Water Level)
# =============================================================================



# -----------------------------------------------------------------------------
# 1. Import Data
# -----------------------------------------------------------------------------
module_wl_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # CSS keyframes for processing spinner animation
    tags$style(HTML("
      @keyframes wl-spin {
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
          tags$h1("Import Water Level Data", class = "custom-h1"),
          tags$h2("Upload raw HOBO U20 water level logger files, barometric data, and align timestamps",
                  class = "custom-h2"),
          tags$p(
            class = "clean-print",
            "Import raw HOBO CSV files from submerged water level pressure loggers ",
            "(HOBO U20). These loggers record absolute pressure (water + atmosphere) ",
            "in kPa, plus water temperature in \u00b0C. The raw pressure column is ",
            "named ", tags$code("waterpress_kPa"), ". Because the HOBO U20 is not ",
            "vented, atmospheric pressure must be subtracted before converting to ",
            "water depth \u2014 that conversion happens in the next step."
          ),
          tags$div(
            class = "clean-print",
            style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(style = "margin: 0;",
              tags$strong("Important:"), " Barometric pressure data must be processed ",
              "and QC'd ", tags$strong("before"), " water level conversion. If you ",
              "haven't done so yet, complete the ",
              tags$a(
                href = "#",
                onclick = "$(\"a[href='#shiny-tab-t_baro_upload']\").tab('show'); return false;",
                style = "color: #856404; font-weight: bold;",
                "Barometric Pressure"
              ),
              " workflow first."
            )
          )
        ),
        # ---- Right: logger image (1/3 width) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; text-align: center; padding-top: 10px;",
          tags$img(
            src = "img/toprightpic_waterlevel.png",
            style = "max-width: 100%; height: auto;",
            alt = "HOBO U20 water level logger"
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
              tags$li(tags$code("model"), " \u2014 logger model (e.g. HOBO_U20)"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 ",
                      tags$strong("must"), " be ", tags$code("\"barometric\""),
                      " or ", tags$code("\"waterlevel\""),
                      " \u2014 this determines how the logger data is classified and processed"),
              tags$li(tags$code("status"), " \u2014 deployment status"),
              tags$li(tags$code("latitude"), " / ", tags$code("longitude"),
                      " \u2014 decimal degrees",
                      " (e.g. ", tags$code("49.2827"), ", ", tags$code("-123.1207"), ")")
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload your own metadata CSV, or use the pre-loaded sample. ",
              "The file must contain one row per deployment period per logger.",
              "Be sure to include metadata for barometric stations as well, so they can be matched to water level stations and shown on the map."
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              tags$a(
                href = "sample_wl_metadata.csv",
                download = "sample_wl_metadata.csv",
                icon("download"),
                " Download sample water level metadata (pre-loaded demo)",
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
      # Row 2: Raw Water Level Logger Data
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Raw Water Level Logger Data Timeseries"
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
              "water level loggers. You can select multiple files at once. ",
              "The CSV must contain a ", tags$code("Date Time"), " column in ",
              "HOBOware's default export format: ",
              tags$code("MM/DD/YYYY HH:MM:SS AM/PM"),
              " (e.g. ", tags$code("06/01/2025 02:30:00 PM"), ")."
            ),
            tags$ol(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Ensure metadata is loaded (above) so logger serial ",
                      "numbers can be matched to stations."),
              tags$li("Select one or more raw HOBO U20 CSV files."),
              tags$li("Click ", tags$strong("Bind & Import Files"), " to parse ",
                      "and combine the data."),
              tags$li("Optionally use ", tags$strong("Snap Timestamps"), " to ",
                      "align readings to a regular interval (e.g. 15 min).")
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "If no files are uploaded, the app will use pre-loaded sample ",
              "data for demonstration."
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              icon("download"), " Download sample raw HOBO water level files:",
              tags$br(),
              tags$a(href = "WL_ALBR_ST_30_20533550_20250423.csv",
                     download = "WL_ALBR_ST_30_20533550_20250423.csv",
                     style = "color: #1565c0; font-size: 12px;",
                     "WL_ALBR_ST_30_20533550_20250423.csv"),
              tags$br(),
              tags$a(href = "WL_ALBR_ST_30_20533550_20251022.csv",
                     download = "WL_ALBR_ST_30_20533550_20251022.csv",
                     style = "color: #1565c0; font-size: 12px;",
                     "WL_ALBR_ST_30_20533550_20251022.csv"),
              tags$br(),
              tags$a(href = "WL_CEDA_ST_30_20558102_20250423.csv",
                     download = "WL_CEDA_ST_30_20558102_20250423.csv",
                     style = "color: #1565c0; font-size: 12px;",
                     "WL_CEDA_ST_30_20558102_20250423.csv"),
              tags$br(),
              tags$a(href = "WL_CEDA_ST_30_20558102_20251021.csv",
                     download = "WL_CEDA_ST_30_20558102_20251021.csv",
                     style = "color: #1565c0; font-size: 12px;",
                     "WL_CEDA_ST_30_20558102_20251021.csv"),
              tags$br(),
              tags$a(href = "WL_FISH_ST_30_20810314_20251024.csv",
                     download = "WL_FISH_ST_30_20810314_20251024.csv",
                     style = "color: #1565c0; font-size: 12px;",
                     "WL_FISH_ST_30_20810314_20251024.csv")
            ),
            tags$p(style = "font-size: 13px; margin-top: 10px;",
              tags$a(
                href = "#", `data-toggle` = "modal",
                `data-target` = "#hoboware_export_modal",
                style = "color: #1565c0;",
                icon("question-circle"),
                " How to export CSV files from HOBOware"
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
                      " (e.g. ", tags$code("ALBR_ST_30"), ")"),
              tags$li(tags$code("timestamp"), " \u2014 date-time in ",
                      tags$code("YYYY-MM-DD HH:MM:SS"), " format",
                      " (e.g. ", tags$code("2025-06-01 14:30:00"), ")"),
              tags$li(tags$code("waterpress_kPa"), " \u2014 absolute pressure in kPa",
                      " (e.g. ", tags$code("110.52"), ")"),
              tags$li(tags$code("watertemp_C"), " \u2014 water temperature in \u00b0C",
                      " (e.g. ", tags$code("12.3"), ")"),
              tags$li(tags$code("sn"), " \u2014 logger serial number",
                      " (e.g. ", tags$code("20533550"), ")"),
              tags$li(tags$code("metric"), " \u2014 ", tags$code("\"waterlevel\""))
            )
          )
        ),

        # ---- Right: File upload (narrow) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",

          # Raw HOBO files upload
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0; flex: 1;",
            tags$h4("Raw Water Level Logger Files", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload one or more raw HOBO U20 CSV files, or use the ",
              "pre-loaded sample data."
            ),
            fileInput(ns("raw_files"), NULL,
              accept = ".csv",
              multiple = TRUE,
              placeholder = "Choose HOBO U20 CSV files...",
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
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #90caf9; border-top: 3px solid #1565c0; border-radius: 50%; animation: wl-spin 0.8s linear infinite; vertical-align: middle;"
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
          "Processed Barometric Data Timeseries"
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
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #90caf9; border-top: 3px solid #1565c0; border-radius: 50%; animation: wl-spin 0.8s linear infinite; vertical-align: middle;"
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
          "Snap Timestamps: Water Level and Air Pressure"
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
              "Round timestamps to a regular interval so water level and ",
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
          "Review Raw Series for Water Level and Air Pressure"
        )
      ),

      tags$p(
        class = "clean-print",
        "Visually inspect the time series plots and summary statistics below ",
        "to look for anomalies such as spikes, flatlines, or gaps in the data. ",
        "The top two panels show water level data; the bottom two show barometric ",
        "data (if loaded). When you are ready, proceed to ",
        tags$strong("Convert Data Metrics"), " in the sidebar."
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
            onclick = "$(\"a[href='#shiny-tab-t_wl_convert']\").tab('show'); window.scrollTo(0,0); return false;",
            style = "color: #1565c0; font-weight: bold;",
            "Convert Data Metrics"
          ),
          " to subtract barometric pressure and convert to water depth."
        )
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 1. Import Data — Server
# =============================================================================
module_wl_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data   <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data
    rv_qaqc_data  <- session$userData$rv_qaqc_data

    # ------ Initialize from demo data ------
    observe({
      data_list <- rv_raw_data$waterlevel_data
      if (!is.null(data_list) && length(data_list) > 0 &&
          is.null(rv_bound_data$waterlevel_bound)) {
        combined <- dplyr::bind_rows(data_list)
        if ("timestamp" %in% names(combined) && is.character(combined$timestamp)) {
          combined$timestamp <- .parse_timestamp(combined$timestamp)
        }
        rv_bound_data$waterlevel_bound <- combined
      }
    })

    # ------ Convenience reactives ------
    wl_current <- reactive({
      rv_bound_data$waterlevel_bound
    })

    baro_qc_current <- reactive({
      rv_qaqc_data$baro_qaqc
    })

    # ------ Data status display ------
    output$data_status <- renderUI({
      wl_df <- wl_current()
      baro_df <- baro_qc_current()

      wl_msg <- if (is.null(wl_df) || nrow(wl_df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 8px;",
          icon("info-circle"),
          " No water level data loaded. Upload files below or use pre-loaded sample data."
        )
      } else {
        n_stations <- length(unique(wl_df$site_station_code))
        n_records <- format(nrow(wl_df), big.mark = ",")
        date_min <- format(min(wl_df$timestamp, na.rm = TRUE), "%Y-%m-%d")
        date_max <- format(max(wl_df$timestamp, na.rm = TRUE), "%Y-%m-%d")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 8px;",
          icon("check-circle"),
          sprintf(" Water level data: %d station(s), %s records (%s to %s)",
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

      tagList(wl_msg, baro_msg)
    })

    # ------ Station map (both WL + baro, colored by metric) ------
    output$station_map <- renderLeaflet({
      metadata <- rv_raw_data$wl_metadata
      req(metadata)

      # Filter to waterlevel + barometric stations
      wl_baro_meta <- metadata[metadata$metric %in% c("waterlevel", "barometric"), ]
      sites <- unique(wl_baro_meta[, c("site_station_code", "latitude", "longitude", "metric")])
      sites <- sites[!is.na(sites$latitude) & !is.na(sites$longitude), ]

      req(nrow(sites) > 0)

      # Split by metric for toggleable overlay groups
      wl_sites   <- sites[sites$metric == "waterlevel", ]
      baro_sites <- sites[sites$metric == "barometric", ]

      map <- leaflet(sites) %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo")

      if (nrow(wl_sites) > 0) {
        map <- map %>% addCircleMarkers(
          data = wl_sites,
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
          group = "Water Level",
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
          labels = c("Water Level", "Barometric"),
          title = "Metric"
        ) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Topo"),
          overlayGroups = c("Water Level", "Barometric"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # ------ Station metadata table (both metrics) ------
    output$station_table <- DT::renderDT({
      metadata <- rv_raw_data$wl_metadata
      req(metadata)

      wl_baro_meta <- metadata[metadata$metric %in% c("waterlevel", "barometric"), ]
      req(nrow(wl_baro_meta) > 0)

      display_cols <- intersect(
        c("site_station_code", "timestamp_deploy", "timestamp_remove",
          "model", "sn", "metric", "status", "latitude", "longitude"),
        names(wl_baro_meta)
      )

      DT::datatable(wl_baro_meta[, display_cols],
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
      metadata <- rv_raw_data$wl_metadata
      req(metadata)
      n_rows <- sum(metadata$metric %in% c("waterlevel", "barometric"))
      height_px <- max(200, n_rows * 50 + 80)
      plotlyOutput(session$ns("deployment_timeline"),
        height = paste0(height_px, "px")) %>% withSpinner()
    })

    # ------ Deployment timeline (Gantt chart, colored by metric) ------
    output$deployment_timeline <- renderPlotly({
      metadata <- rv_raw_data$wl_metadata
      req(metadata)

      wl_baro_meta <- metadata[metadata$metric %in% c("waterlevel", "barometric"), ]
      req(nrow(wl_baro_meta) > 0)

      # Parse deploy/remove timestamps
      wl_baro_meta$deploy <- as.POSIXct(wl_baro_meta$timestamp_deploy,
        format = "%m/%d/%Y %H:%M", tz = "UTC")
      wl_baro_meta$remove <- as.POSIXct(wl_baro_meta$timestamp_remove,
        format = "%m/%d/%Y %H:%M", tz = "UTC")

      # Order by metric then station then deploy time
      wl_baro_meta <- wl_baro_meta[order(wl_baro_meta$metric,
                                          wl_baro_meta$site_station_code,
                                          wl_baro_meta$deploy), ]

      # Create a unique label per row for the y-axis
      wl_baro_meta$row_label <- paste0(
        wl_baro_meta$site_station_code, " (SN: ", wl_baro_meta$sn, ") [",
        seq_len(nrow(wl_baro_meta)), "]"
      )
      # Preserve order as factor
      wl_baro_meta$row_label <- factor(wl_baro_meta$row_label,
        levels = rev(wl_baro_meta$row_label))

      # Colour by unique serial number
      wl_baro_meta$sn_factor <- as.character(wl_baro_meta$sn)

      # Build colour palette
      unique_sns <- unique(wl_baro_meta$sn_factor)
      palette <- RColorBrewer::brewer.pal(max(3, length(unique_sns)), "Set2")
      sn_colors <- setNames(palette[seq_along(unique_sns)], unique_sns)

      # Build plot with one trace per SN (for legend grouping)
      p <- plot_ly()
      legend_shown <- character(0)

      for (i in seq_len(nrow(wl_baro_meta))) {
        row <- wl_baro_meta[i, ]
        show_legend <- !(row$sn_factor %in% legend_shown)
        if (show_legend) legend_shown <- c(legend_shown, row$sn_factor)

        metric_label <- ifelse(row$metric == "waterlevel", "Water Level", "Barometric")

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
                     categoryarray = rev(levels(wl_baro_meta$row_label))),
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

        # Validate: only allow barometric and waterlevel
        invalid <- unique(metadata$metric[!metadata$metric %in% c("barometric", "waterlevel")])
        if (length(invalid) > 0) {
          showNotification(
            paste0("Invalid metric values removed: ", paste(invalid, collapse = ", "),
                   ". Only 'barometric' and 'waterlevel' are allowed."),
            type = "warning", duration = 8
          )
          metadata <- metadata[metadata$metric %in% c("barometric", "waterlevel"), ]
        }

        # Warn if no waterlevel rows found after filtering
        n_wl <- sum(metadata$metric == "waterlevel")
        if (n_wl == 0) {
          found_metrics <- unique(metadata$metric)
          showNotification(
            paste0("Warning: No rows with metric 'waterlevel' found in metadata. ",
                   "Found metric values: ",
                   if (length(found_metrics) > 0) paste(found_metrics, collapse = ", ") else "none",
                   "."),
            type = "warning", duration = 10
          )
        }

        rv_raw_data$wl_metadata <- metadata
        showNotification("Metadata updated.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error reading metadata:", conditionMessage(e)),
          type = "error", duration = 5)
      })
    })

    # ------ Update station selector for snap ------
    observe({
      df <- wl_current()
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

      # Always use the already-normalised metadata from rv_raw_data$wl_metadata.
      # The upload observer (input$meta_file) already reads, normalises the
      # metric column, and stores it there — re-reading the temp file here
      # would bypass that normalisation and overwrite the clean copy.
      metadata <- rv_raw_data$wl_metadata

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
          rv_bound_data$waterlevel_bound <- bound_data

          # Track bind results for summary display
          bound_stations <- unique(bound_data$site_station_code)
          meta_wl_stations <- unique(
            metadata$site_station_code[metadata$metric %in% c("waterlevel")]
          )
          matched <- intersect(bound_stations, meta_wl_stations)
          unmatched_in_data <- setdiff(bound_stations, meta_wl_stations)

          rv_bind_result$success <- TRUE
          rv_bind_result$n_stations <- length(bound_stations)
          rv_bind_result$n_records <- nrow(bound_data)
          rv_bind_result$matched_stations <- matched
          rv_bind_result$unmatched_sns <- unmatched_in_data
          rv_bind_result$metadata_stations <- meta_wl_stations

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
      df <- wl_current()
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
          metric = "WL"
        )

        rv_bound_data$waterlevel_bound <- result

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

        # Parse timestamps (handles date-only midnight rows like "2023-10-19")
        if (is.character(baro_data$timestamp)) {
          baro_data$timestamp <- .parse_timestamp(baro_data$timestamp)
        }

        # Validate station codes against metadata
        metadata <- rv_raw_data$wl_metadata
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
      df <- wl_current()
      req(df)

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `Start Date` = format(min(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
          `End Date` = format(max(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
          `Press Min (kPa)` = round(min(waterpress_kPa, na.rm = TRUE), 2),
          `Press Max (kPa)` = round(max(waterpress_kPa, na.rm = TRUE), 2),
          `Temp Min (C)` = round(min(watertemp_C, na.rm = TRUE), 1),
          `Temp Max (C)` = round(max(watertemp_C, na.rm = TRUE), 1),
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ------ Preview plot (4 panels: WL press, WL temp, baro press, baro temp) ------
    output$preview_plot <- renderPlotly({
      wl_df <- wl_current()
      req(wl_df, nrow(wl_df) > 0)

      baro_df <- baro_qc_current()

      wl_stations <- unique(wl_df$site_station_code)

      # Panel 1: Water pressure (kPa)
      p1 <- plot_ly()
      for (stn in wl_stations) {
        stn_data <- dplyr::filter(wl_df, site_station_code == stn)
        p1 <- p1 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~waterpress_kPa,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- WL Press"),
          legendgroup = stn,
          hovertemplate = paste0(stn, ": %{y:.2f} kPa<extra></extra>")
        )
      }

      # Panel 2: Water temperature (C)
      p2 <- plot_ly()
      for (stn in wl_stations) {
        stn_data <- dplyr::filter(wl_df, site_station_code == stn)
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
          title = list(text = "Water Level & Barometric Data Preview",
                       font = list(size = 14)),
          xaxis4 = list(title = "Date"),
          yaxis = list(title = "Water Press (kPa)"),
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
      wl <- rv_bound_data$waterlevel_bound
      req(wl, nrow(wl) > 0)
      ts_range <- range(wl$timestamp, na.rm = TRUE)
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

      wl <- rv_bound_data$waterlevel_bound
      if (!is.null(wl) && nrow(wl) > 0)
        rv_bound_data$waterlevel_bound <- wl[wl$timestamp >= from_ts & wl$timestamp <= to_ts, ]

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
        n <- nrow(rv_bound_data$waterlevel_bound)
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
# 2. Convert Data Metrics
# -----------------------------------------------------------------------------
module_wl_convert_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Convert Water Level Pressure to Meters", class = "custom-h1"),
      tags$h2("Barometric compensation and pressure-to-depth conversion",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "This step joins barometric pressure data to each water level timestamp,
        then converts absolute pressure (kPa) to water depth (metres) using
        barometric compensation, water density, and a field reference measurement."
      ),

      tags$div(
        class = "clean-print",
        tags$h4("Why Baro Compensation?", style = "margin-top: 10px;"),
        tags$p(
          "HOBO U20 loggers are not vented \u2014 they measure total pressure ",
          "(water + atmosphere). Atmospheric pressure varies by 1\u20133 kPa with ",
          "weather, which would appear as 10\u201330 cm of false water level ",
          "change if not subtracted."
        ),
        tags$h4("What About Elevation Differences?", style = "margin-top: 15px;"),
        tags$p(
          "No explicit altitude correction is applied \u2014 the metadata ",
          "schema does not include an elevation column. Instead, systematic ",
          "pressure offsets between stations at different elevations are ",
          "absorbed by the regression calibration step (when temporal overlap ",
          "exists). ",
          "For ", tags$strong("pressure"), ", this works well in practice: ",
          "the final depth conversion anchors to a field reference measurement, ",
          "so a constant offset in the baro series cancels out. ",
          "For ", tags$strong("air temperature"), ", no calibration is performed, ",
          "if the composite series switches between stations at different ",
          "elevations, step changes in air temperature may occur at the ",
          "transition points."
        )
      ),

      hr(),

      # Part A: Add Nearest Baro
      tags$h3("Part A: Add Nearest Barometric Station", style = "color: #1565c0; margin-bottom: 15px;"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",

            tags$h4("How Station Matching Works", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "The function uses ", tags$code("geosphere::distHaversine()"), " to ",
              "rank all barometric stations by horizontal distance (lat/lon) from ",
              "each water level site. It keeps the nearest ", tags$code("max_stations"),
              " (default 5) within an optional ", tags$code("max_km"), " radius."
            ),
            tags$p(style = "font-size: 13px;",
              "Barometric data is joined to water level data by an exact ",
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
                   "Matches each water level station to the nearest barometric station(s) ",
                   "by GPS distance and joins baro pressure/temperature by timestamp. ",
                   "Outputs the input data frame with added ",
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
            selectizeInput(ns("baro_wl_stations"), NULL,
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
              "Leave station selector empty to process all WL stations. Select ",
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
        ),

        tags$div(
          style = "background-color: #ffffff; padding: 20px; border-radius: 5px; margin-top: 15px; border: 1px solid #e0e0e0;",
          tags$h4("What to Look For", style = "margin-top: 0; color: #1565c0;"),
          tags$p(style = "font-size: 13px;",
            "Review the barometric pressure overlay against the raw water pressure ",
            "time series. This is the data that will be used to compensate for ",
            "atmospheric pressure during the conversion step. Look for:"
          ),
          tags$ul(
            style = "font-size: 13px; padding-left: 20px;",
            tags$li(tags$strong("Gaps in barometric data"), " \u2014 missing baro values ",
                    "will produce ", tags$code("NA"), " in the converted water level. If ",
                    "the nearest baro station has gaps, consider adding additional baro ",
                    "stations or adjusting the ", tags$code("max_stations"), " parameter ",
                    "in ", tags$code("add_nearest_baro()"), " to increase overlap."),
            tags$li(tags$strong("Sudden spikes in air pressure"), " \u2014 sharp jumps or ",
                    "drops in the barometric trace that do not correspond to real weather ",
                    "events may indicate logger malfunction or sensor disturbance. These ",
                    "will propagate directly into the water level calculation. Return to the ",
                    "Barometric Pressure QC tab to flag and remove these before converting."),
            tags$li(tags$strong("Temporal misalignment"), " \u2014 verify that the baro and ",
                    "water level traces overlap in time. If timestamps are offset (e.g. ",
                    "different time zones), the compensation will be incorrect."),
            tags$li(tags$strong("Station assignment"), " \u2014 confirm that the correct ",
                    "barometric station has been matched to each water level station. The ",
                    "nearest station by distance is selected automatically, but you can ",
                    "override this if a closer station has poorer data quality.")
          ),
          tags$p(style = "font-size: 12px; color: #666; margin-top: 10px;",
            "See ", tags$code("add_nearest_baro()"), " and ",
            tags$code("convert_waterlevel_kPa_m()"), " in the ",
            tags$code("waterlogged"), " R package documentation for details."
          )
        )
      ),

      hr(),

      # Part B: Convert to Metres
      tags$h3("Part B: Convert Pressure to Water Depth", style = "color: #2e7d32; margin-bottom: 15px;"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("The Conversion", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "HOBO U20 loggers record absolute pressure (water column + atmosphere) ",
              "in kilopascals. To obtain a meaningful water level in metres, we need ",
              "to remove the atmospheric component, account for water density, and ",
              "anchor the result to a known field measurement. This step automates ",
              "that physics:"
            ),
            tags$ol(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Calculate water density from temperature using the Kell (1975) polynomial"),
              tags$li("Convert water pressure to depth: depth = P / (\u03c1 \u00b7 g)"),
              tags$li("Subtract the baro-equivalent depth to remove atmospheric pressure"),
              tags$li("Compute a compensation constant ", tags$em("k"),
                      " from a field reference measurement and apply it to the entire time series")
            ),
            tags$p(style = "font-size: 13px; margin-top: 10px; color: #666;",
              "The field reference can be either a ", tags$strong("staff gauge reading"),
              " (stage) or a ", tags$strong("direct depth measurement"),
              " from the water surface to the sensor. Upload your reference data on ",
              "the right, select which measurement to anchor each station to, and ",
              "click Convert."
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Stage / Depth Reference File", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "The stage readings CSV contains field reference measurements used to ",
              "anchor the pressure-to-depth conversion. Each row records a site visit ",
              "where a staff gauge reading or direct depth measurement was taken."
            ),
            tags$p(style = "font-weight: bold; margin-bottom: 5px;",
              "Required columns:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$code("site_station_code"), " \u2014 station identifier. ",
                      tags$strong("Must exactly match"), " the codes in your metadata file",
                      " (e.g. ", tags$code("ALBR_ST_30"), ")"),
              tags$li(tags$code("timestamp"), " \u2014 date-time of the field measurement in ",
                      tags$code("MM/DD/YYYY HH:MM"), " format",
                      " (e.g. ", tags$code("07/25/2024 13:00"), ").",
                      " Also accepts ", tags$code("YYYY-MM-DD HH:MM"), "."),
              tags$li(tags$code("stage_m"), " \u2014 staff gauge reading in metres",
                      " (e.g. ", tags$code("0.35"), ").",
                      " Use this ", tags$strong("or"), " ", tags$code("depth_m"),
                      ", not both."),
              tags$li(tags$code("depth_m"), " \u2014 direct depth measurement (water surface ",
                      "to sensor) in metres",
                      " (e.g. ", tags$code("0.43"), ").",
                      " Use this ", tags$strong("or"), " ", tags$code("stage_m"),
                      ", not both.")
            ),
            tags$p(style = "font-weight: bold; margin-bottom: 5px;",
              "Optional columns:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$code("metric"), " \u2014 ", tags$code("\"WL\""),
                      " (water level)"),
              tags$li(tags$code("site_name"), " \u2014 descriptive site name"),
              tags$li(tags$code("site_comments"), " \u2014 field notes for this visit"),
              tags$li(tags$code("logger_dry"), ", ", tags$code("logger_ice"), ", ",
                      tags$code("logger_disturbance"),
                      " \u2014 flags (", tags$code("1"), " = true) noting field conditions")
            ),
            tags$p(style = "font-size: 13px; color: #666; margin-top: 10px;",
              "The conversion function will match the reference measurement closest in ",
              "time to each station's data. Only one of ", tags$code("stage_m"), " or ",
              tags$code("depth_m"), " should be populated per row \u2014 the column you ",
              "use determines the ", tags$code("reference_type"), " parameter ",
              "(", tags$code("\"stage\""), " or ", tags$code("\"depth\""), ")."
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              tags$a(
                href = "sample_stage_readings.csv",
                download = "sample_stage_readings.csv",
                icon("download"),
                " Download sample stage readings CSV",
                style = "color: #2e7d32;"
              )
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("convert_waterlevel_kPa_m()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Subtracts barometric pressure, converts water pressure to depth using ",
                   "temperature-dependent density, and anchors the result to a field reference ",
                   "measurement. Outputs the data frame with a new ", tags$code("waterlevel_m"),
                   " column.")
          )
        ),
        column(
          width = 8,

          # ---- Sub-section 1: Upload Stage Readings ----
          tags$div(
            style = "border-top: 2px solid #2e7d32; margin: 0 0 20px 0; position: relative;",
            tags$span(
              style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
              "Stage / Depth Reference Data"
            )
          ),

          tags$div(
            style = "margin-top: 10px; margin-bottom: 15px;",
            fluidRow(
              column(8,
                fileInput(ns("stage_upload"), "Upload Stage Readings CSV:",
                  accept = ".csv", width = "100%")
              ),
              column(4,
                tags$div(style = "margin-top: 28px;")
              )
            ),
            uiOutput(ns("stage_upload_status"))
          ),

          conditionalPanel(
            condition = "output.show_stage_table",
            ns = ns,

            DT::DTOutput(ns("stage_ref_table")),

            tags$div(style = "margin-top: 15px;",
              uiOutput(ns("measurement_selectors"))
            )
          ),

          # ---- Sub-section 2: Station Readiness ----
          tags$div(
            style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
            tags$span(
              style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
              "Station Readiness"
            )
          ),

          uiOutput(ns("convert_readiness")),

          # ---- Sub-section 3: Convert Water Level ----
          tags$div(
            style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
            tags$span(
              style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
              "Convert Water Level"
            )
          ),

          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; margin-bottom: 15px;",

            fluidRow(
              column(6,
                selectizeInput(ns("convert_wl_stations"), "Stations to Convert:",
                  choices = NULL, multiple = TRUE, width = "100%",
                  options = list(
                    placeholder = "All ready stations (default)",
                    plugins = list("remove_button")
                  )
                )
              ),
              column(6,
                selectInput(ns("reference_type"), "Reference Type:",
                  choices = c("Stage" = "stage", "Depth" = "depth"),
                  width = "100%"
                )
              )
            ),

            actionButton(ns("run_convert"), "Convert to Metres",
              class = "btn-success btn-lg", width = "100%",
              icon = icon("ruler-vertical"),
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
          plotly::plotlyOutput(ns("convert_wl_plot"), height = "650px")
        ),

        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Reference Measurement Comparison"
          )
        ),

        uiOutput(ns("ref_comparison_station_label")),
        DT::DTOutput(ns("convert_ref_comparison_table")),

        # ---- Next step link ----
        tags$div(
          style = "margin-top: 25px; padding: 15px; background-color: #e8f5e9; border-radius: 5px; text-align: center;",
          tags$p(style = "margin: 0; font-size: 14px;",
            icon("arrow-right"),
            " Ready to proceed? Continue to ",
            tags$a(
              href = "#",
              onclick = "$(\"a[href='#shiny-tab-t_wl_qaqc']\").tab('show'); window.scrollTo(0,0); return false;",
              style = "color: #2e7d32; font-weight: bold;",
              "QA Water Level"
            ),
            " to run automated quality checks and review flagged data."
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_wl_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data       <- session$userData$rv_raw_data
    rv_bound_data     <- session$userData$rv_bound_data
    rv_qaqc_data      <- session$userData$rv_qaqc_data

    # Module-local reactives
    baro_result   <- reactiveVal(NULL)
    baro_messages <- reactiveVal(NULL)

    # Conditional panel visibility
    output$show_baro_results <- reactive({ !is.null(baro_result()) })
    outputOptions(output, "show_baro_results", suspendWhenHidden = FALSE)

    # ---- Data readiness banner ----
    output$baro_readiness <- renderUI({
      wl   <- rv_bound_data$waterlevel_bound
      baro <- rv_qaqc_data$baro_qaqc
      if (is.null(wl) || nrow(wl) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No water level data available. Import data in the Import tab first."
        )
      } else if (is.null(baro) || nrow(baro) == 0) {
        n_stns <- length(unique(wl$site_station_code))
        n_recs <- format(nrow(wl), big.mark = ",")
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          sprintf(" WL data ready (%d station(s), %s records) but no QC'd barometric data found. Process baro data first.",
                  n_stns, n_recs)
        )
      } else {
        n_stns <- length(unique(wl$site_station_code))
        n_recs <- format(nrow(wl), big.mark = ",")
        n_baro <- length(unique(baro$site_station_code))
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Ready: %d WL station(s), %s records | %d baro station(s) available",
                  n_stns, n_recs, n_baro)
        )
      }
    })

    # ---- Update WL station selector ----
    observe({
      wl <- rv_bound_data$waterlevel_bound
      if (!is.null(wl) && nrow(wl) > 0) {
        stns <- sort(unique(wl$site_station_code))
        updateSelectizeInput(session, "baro_wl_stations", choices = stns,
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
      wl_data   <- rv_bound_data$waterlevel_bound
      baro_data <- rv_qaqc_data$baro_qaqc
      metadata  <- rv_raw_data$wl_metadata

      if (is.null(wl_data) || nrow(wl_data) == 0) {
        showNotification("No water level data. Import data first.", type = "error", duration = 5)
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
        showNotification("Adding barometric data to water level stations...",
                         type = "message", duration = NULL, id = "wl_baro_progress")

        # Determine station subset
        selected_stns <- input$baro_wl_stations
        run_subset <- length(selected_stns) > 0
        input_data <- if (run_subset) {
          dplyr::filter(wl_data, site_station_code %in% selected_stns)
        } else {
          wl_data
        }

        # Deduplicate baro and WL data by (site_station_code, timestamp)
        # to prevent cartesian explosion in add_nearest_baro() joins
        n_baro_before <- nrow(baro_data)
        baro_data <- baro_data %>%
          dplyr::arrange(site_station_code, timestamp) %>%
          dplyr::filter(!duplicated(paste(site_station_code, timestamp), fromLast = TRUE))
        n_baro_dupes <- n_baro_before - nrow(baro_data)

        n_wl_before <- nrow(input_data)
        input_data <- input_data %>%
          dplyr::arrange(site_station_code, timestamp) %>%
          dplyr::filter(!duplicated(paste(site_station_code, timestamp), fromLast = TRUE))
        n_wl_dupes <- n_wl_before - nrow(input_data)

        if (n_baro_dupes > 0) cat("[add_baro] Removed", n_baro_dupes, "duplicate baro rows\n")
        if (n_wl_dupes > 0) cat("[add_baro] Removed", n_wl_dupes, "duplicate WL rows\n")

        cat("[add_baro] Input data:", nrow(input_data), "rows,",
            length(unique(input_data$site_station_code)), "station(s)\n")
        cat("[add_baro] Baro data:", nrow(baro_data), "rows,",
            length(unique(baro_data$site_station_code)), "station(s)\n")
        cat("[add_baro] Metadata:", nrow(metadata), "rows\n")

        # Build baro selection arg
        baro_sel <- if (input$baro_selection == "auto") "auto" else input$baro_selection
        cat("[add_baro] baro_selection:", baro_sel, "\n")

        # Build max_km (NA → NULL)
        max_km_val <- input$max_km
        if (is.na(max_km_val)) max_km_val <- NULL
        cat("[add_baro] max_km:", if (is.null(max_km_val)) "NULL" else max_km_val,
            "| max_stations:", input$max_stations, "\n")

        if (run_subset) {
          cat("[add_baro] Running subset for station(s):", paste(selected_stns, collapse = ", "), "\n")
        } else {
          cat("[add_baro] Running for ALL stations\n")
        }

        cat("[add_baro] Calling add_nearest_baro() ...\n")
        t_start <- Sys.time()

        # Capture message() output for calibration diagnostics
        msgs <- character(0)
        result <- withCallingHandlers(
          add_nearest_baro(
            input_data       = input_data,
            metric           = "water level",
            baro_data        = baro_data,
            metadata         = metadata,
            max_km           = max_km_val,
            max_stations     = input$max_stations,
            baro_site_selection = baro_sel
          ),
          message = function(m) {
            cat("[add_baro][msg]", conditionMessage(m))
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            cat("[add_baro][warn]", conditionMessage(w), "\n")
            invokeRestart("muffleWarning")
          }
        )

        t_elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1)
        cat("[add_baro] add_nearest_baro() returned in", t_elapsed, "sec\n")
        cat("[add_baro] Result class:", paste(class(result), collapse = "/"), "\n")

        # add_nearest_baro returns a list; extract the data frame
        if (is.list(result) && !is.data.frame(result)) {
          cat("[add_baro] Extracting data frame from list (length", length(result), ")\n")
          result <- result[[1]]
        }

        cat("[add_baro] Result:", nrow(result), "rows,", ncol(result), "cols\n")
        cat("[add_baro] Columns:", paste(names(result), collapse = ", "), "\n")

        # Merge-back: preserve existing results for non-selected stations
        if (run_subset && !is.null(baro_result())) {
          existing <- dplyr::filter(baro_result(),
                                    !(site_station_code %in% selected_stns))
          cat("[add_baro] Merge-back: keeping", nrow(existing), "existing rows for other stations\n")
          result <- dplyr::bind_rows(existing, result) %>%
            dplyr::arrange(site_station_code, timestamp)
        }

        cat("[add_baro] Storing result in baro_result() and rv_bound_data$waterlevel_with_baro\n")
        baro_result(result)
        baro_messages(msgs)
        rv_bound_data$waterlevel_with_baro <- result

        removeNotification("wl_baro_progress")
        n_stns <- length(unique(result$site_station_code))
        cat("[add_baro] Done. Baro data added for", n_stns, "station(s)\n")
        showNotification(
          sprintf("Baro data added for %d station(s).", n_stns),
          type = "message", duration = 4
        )

      }, error = function(e) {
        cat("[add_baro][ERROR]", conditionMessage(e), "\n")
        removeNotification("wl_baro_progress")
        showNotification(paste("Baro join error:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })

    # ---- Summary table ----
    output$baro_summary_table <- DT::renderDT({
      df <- baro_result()
      req(df, "airpress_kPa" %in% names(df), "baro_site_stn_code" %in% names(df))

      metadata <- rv_raw_data$wl_metadata

      # Build a lookup of lat/lon per station from metadata
      coords <- metadata %>%
        dplyr::select(site_station_code, latitude, longitude) %>%
        dplyr::distinct(site_station_code, .keep_all = TRUE)

      # One row per WL station × baro station
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

      # Compute Haversine distance between each WL and baro station
      summary_df <- summary_df %>%
        dplyr::left_join(coords, by = "site_station_code") %>%
        dplyr::rename(wl_lat = latitude, wl_lon = longitude) %>%
        dplyr::left_join(coords, by = c("baro_site_stn_code" = "site_station_code")) %>%
        dplyr::rename(baro_lat = latitude, baro_lon = longitude) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          `Distance (km)` = if (!is.na(wl_lat) && !is.na(wl_lon) &&
                                !is.na(baro_lat) && !is.na(baro_lon)) {
            round(geosphere::distHaversine(
              c(wl_lon, wl_lat), c(baro_lon, baro_lat)
            ) / 1000, 1)
          } else {
            NA_real_
          }
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(
          `WL Station`   = site_station_code,
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
        # Reconstruct the summary table row order to find the WL station
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

      # Parse calibration lines for this station
      # Messages like: "[ALBR_ST_30] Calibrated ALBR_WX_10 -> ALBR_WX_01: n_overlap=500, R2=0.998"
      stn_msgs <- msgs[grepl(stn, msgs, fixed = TRUE)]
      cal_lines <- stn_msgs[grepl("Calibrat|calibrat|R2|r2|overlap", stn_msgs)]

      if (length(cal_lines) == 0) {
        # Check if single station match
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

    # ---- Review plot ----
    output$baro_assign_plot <- plotly::renderPlotly({
      df <- selected_station_data()
      req(nrow(df) > 0, "airpress_kPa" %in% names(df))

      # Top panel: WL raw pressure + matched baro pressure
      p1 <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = df, x = ~timestamp, y = ~waterpress_kPa,
          type = "scatter", mode = "lines",
          name = "WL Pressure (raw)",
          line = list(color = "#90CAF9", width = 1)
        )

      # Add baro pressure, colored by contributing station
      if ("baro_site_stn_code" %in% names(df)) {
        baro_stns <- unique(na.omit(df$baro_site_stn_code))
        baro_colors <- c("#E53935", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1",
                         "#6D4C41", "#D81B60", "#1E88E5", "#FDD835", "#546E7A")
        for (i in seq_along(baro_stns)) {
          stn_data <- dplyr::filter(df, baro_site_stn_code == baro_stns[i])
          if (nrow(stn_data) > 0) {
            p1 <- p1 %>% plotly::add_trace(
              data = stn_data, x = ~timestamp, y = ~airpress_kPa,
              type = "scatter", mode = "lines",
              name = paste0("Baro: ", baro_stns[i]),
              line = list(color = baro_colors[((i - 1) %% length(baro_colors)) + 1], width = 1)
            )
          }
        }
      } else {
        p1 <- p1 %>% plotly::add_trace(
          data = df, x = ~timestamp, y = ~airpress_kPa,
          type = "scatter", mode = "lines",
          name = "Baro Pressure",
          line = list(color = "#E53935", width = 1)
        )
      }

      p1 %>% plotly::layout(
          yaxis = list(title = "Pressure (kPa)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.1)
        )
    })

    # =========================================================================
    # Part B: Convert Pressure to Water Depth — Server Logic
    # =========================================================================

    rv_converted_data <- session$userData$rv_converted_data

    # Module-local reactives
    convert_result   <- reactiveVal(NULL)   # list(site_wl, ref_dat)
    convert_ref_data <- reactiveVal(NULL)   # validated reference data frame

    # ---- Conditional panel visibility ----
    output$show_stage_table <- reactive({ !is.null(convert_ref_data()) })
    outputOptions(output, "show_stage_table", suspendWhenHidden = FALSE)

    output$show_convert_results <- reactive({ !is.null(convert_result()) })
    outputOptions(output, "show_convert_results", suspendWhenHidden = FALSE)

    # ---- Demo pre-load: initialize convert_ref_data from rv_raw_data$stage_readings ----
    observe({
      demo <- rv_raw_data$stage_readings
      req(demo)
      # Only initialize once (when convert_ref_data is NULL)
      if (!is.null(convert_ref_data())) return()

      tryCatch({
        validated <- QAQC_reference_data(ref_data = demo)
        convert_ref_data(validated)
        cat("[convert_wl] Demo stage readings loaded:", nrow(validated), "rows\n")
      }, error = function(e) {
        cat("[convert_wl] Demo stage readings validation failed:", conditionMessage(e), "\n")
      })
    }) |> bindEvent(rv_raw_data$stage_readings, once = TRUE)

    # ---- Upload handler ----
    observeEvent(input$stage_upload, {
      file <- input$stage_upload
      req(file)

      tryCatch({
        uploaded <- read.csv(file$datapath, stringsAsFactors = FALSE)
        validated <- QAQC_reference_data(ref_data = uploaded)
        convert_ref_data(validated)
        rv_raw_data$stage_readings <- uploaded  # store raw for convert function
        cat("[convert_wl] Uploaded stage readings:", nrow(validated), "rows\n")
        showNotification(
          sprintf("Stage readings loaded: %d records", nrow(validated)),
          type = "message", duration = 4
        )
      }, error = function(e) {
        cat("[convert_wl][ERROR] Stage upload:", conditionMessage(e), "\n")
        showNotification(paste("Stage readings error:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })

    # ---- Stage upload status banner ----
    output$stage_upload_status <- renderUI({
      ref <- convert_ref_data()
      if (is.null(ref)) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No stage/depth reference data loaded."
        )
      } else {
        n_stns <- length(unique(ref$site_station_code))
        n_stage <- sum(!is.na(ref$stage_m))
        n_depth <- sum(!is.na(ref$depth_m))
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" %d reference records loaded (%d station(s), %d stage, %d depth measurements)",
                  nrow(ref), n_stns, n_stage, n_depth)
        )
      }
    })

    # ---- Stage reference table ----
    output$stage_ref_table <- DT::renderDT({
      ref <- convert_ref_data()
      req(ref)

      display_cols <- intersect(
        c("site_station_code", "timestamp", "stage_m", "depth_m", "site_comments"),
        names(ref)
      )
      display_df <- ref[, display_cols, drop = FALSE]
      display_df$timestamp <- format(display_df$timestamp, "%Y-%m-%d %H:%M")

      DT::datatable(display_df,
        filter = "top",
        selection = "none",
        options = list(
          dom = "ltip",
          pageLength = 5,
          lengthMenu = list(c(5, 50, -1), c("5", "50", "All")),
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # ---- Available measurements per station ----
    # Mirrors the internal filtering in convert_waterlevel_single()
    available_measurements <- reactive({
      ref <- convert_ref_data()
      wl_baro <- rv_bound_data$waterlevel_with_baro
      req(ref, wl_baro)

      ref_type <- input$reference_type %||% "stage"
      ref_col <- if (ref_type == "depth") "depth_m" else "stage_m"

      wl_stations <- unique(wl_baro$site_station_code)
      result <- list()

      for (stn in wl_stations) {
        stn_ref <- ref %>%
          dplyr::filter(site_station_code == stn)

        # Must have the reference column
        if (!ref_col %in% names(stn_ref)) next
        stn_ref <- stn_ref %>%
          dplyr::mutate(ref_m = .data[[ref_col]]) %>%
          dplyr::filter(!is.na(ref_m))

        if (nrow(stn_ref) == 0) next

        # Round timestamps to hour and deduplicate (keep last)
        stn_ref$timestamp <- lubridate::round_date(stn_ref$timestamp, "hour")
        stn_ref <- stn_ref %>%
          dplyr::group_by(timestamp) %>%
          dplyr::slice_tail(n = 1) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(timestamp)

        # Filter to logger time range
        stn_wl <- dplyr::filter(wl_baro, site_station_code == stn)
        ts_range <- range(stn_wl$timestamp, na.rm = TRUE)
        stn_ref <- stn_ref %>%
          dplyr::filter(timestamp >= ts_range[1], timestamp <= ts_range[2])

        if (nrow(stn_ref) > 0) {
          result[[stn]] <- stn_ref
        }
      }

      result
    })

    # ---- Measurement selectors (per-station radioButtons) ----
    output$measurement_selectors <- renderUI({
      avail <- available_measurements()
      req(length(avail) > 0)

      ns <- session$ns

      selector_list <- lapply(names(avail), function(stn) {
        ref_rows <- avail[[stn]]
        choices <- seq_len(nrow(ref_rows))
        labels <- paste0(
          "#", choices, ": ",
          format(ref_rows$timestamp, "%Y-%m-%d %H:%M"),
          " = ", sprintf("%.3f", ref_rows$ref_m), " m",
          ifelse(nchar(trimws(ref_rows$site_comments)) > 0,
                 paste0(" (", substr(trimws(ref_rows$site_comments), 1, 50), ")"),
                 "")
        )
        names(choices) <- labels

        sanitized <- gsub("[^A-Za-z0-9_]", "_", stn)

        tags$div(
          style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          tags$strong(stn, style = "color: #2e7d32;"),
          radioButtons(ns(paste0("meas_select_", sanitized)),
            label = NULL,
            choices = choices,
            selected = 1,
            inline = FALSE
          )
        )
      })

      do.call(tagList, selector_list)
    })

    # ---- Station readiness ----
    output$convert_readiness <- renderUI({
      ref <- convert_ref_data()
      wl_baro <- rv_bound_data$waterlevel_with_baro
      wl_bound <- rv_bound_data$waterlevel_bound

      # Get all WL stations
      wl_stns <- character(0)
      if (!is.null(wl_bound) && nrow(wl_bound) > 0) {
        wl_stns <- unique(wl_bound$site_station_code)
      } else if (!is.null(wl_baro) && nrow(wl_baro) > 0) {
        wl_stns <- unique(wl_baro$site_station_code)
      }

      if (length(wl_stns) == 0) {
        return(tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No water level stations available."
        ))
      }

      baro_stns <- if (!is.null(wl_baro)) unique(wl_baro$site_station_code) else character(0)
      avail <- tryCatch(available_measurements(), error = function(e) list())

      status_items <- lapply(wl_stns, function(stn) {
        has_baro <- stn %in% baro_stns
        has_ref  <- stn %in% names(avail)
        missing  <- character(0)
        if (!has_baro) missing <- c(missing, "no baro data")
        if (!has_ref)  missing <- c(missing, "no in-range reference measurements")

        if (length(missing) == 0) {
          n_ref <- nrow(avail[[stn]])
          tags$div(style = "margin-bottom: 5px;",
            icon("check-circle", style = "color: #2e7d32;"),
            tags$strong(stn), sprintf(" \u2014 Ready (%d reference measurement%s)", n_ref, ifelse(n_ref == 1, "", "s"))
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

    # ---- Update station selector with ready stations ----
    observe({
      avail <- available_measurements()
      stns <- names(avail)
      updateSelectizeInput(session, "convert_wl_stations", choices = stns,
                           selected = character(0))
    })

    # ---- Run conversion ----
    observeEvent(input$run_convert, {
      wl_baro   <- rv_bound_data$waterlevel_with_baro
      ref       <- convert_ref_data()
      ref_raw   <- rv_raw_data$stage_readings  # raw data for convert function
      metadata  <- rv_raw_data$wl_metadata

      if (is.null(wl_baro) || nrow(wl_baro) == 0) {
        showNotification("No water level + baro data. Run Part A first.", type = "error", duration = 5)
        return()
      }
      if (is.null(ref) || is.null(ref_raw)) {
        showNotification("No reference data loaded.", type = "error", duration = 5)
        return()
      }

      avail <- available_measurements()
      if (length(avail) == 0) {
        showNotification("No stations have in-range reference measurements.", type = "error", duration = 5)
        return()
      }

      # Determine which stations to process
      selected_stns <- input$convert_wl_stations
      run_subset <- length(selected_stns) > 0
      target_stns <- if (run_subset) {
        intersect(selected_stns, names(avail))
      } else {
        names(avail)
      }

      if (length(target_stns) == 0) {
        showNotification("None of the selected stations are ready.", type = "error", duration = 5)
        return()
      }

      ref_type <- input$reference_type %||% "stage"

      tryCatch({
        showNotification("Converting water pressure to depth...",
                         type = "message", duration = NULL, id = "wl_convert_progress")

        all_site_wl <- list()
        all_ref_dat <- list()

        for (stn in target_stns) {
          sanitized <- gsub("[^A-Za-z0-9_]", "_", stn)
          meas_idx <- as.integer(input[[paste0("meas_select_", sanitized)]]) %||% 1L

          cat("[convert_wl] Processing", stn, "| measurement:", meas_idx, "| ref_type:", ref_type, "\n")

          stn_result <- tryCatch({
            msgs <- character(0)
            res <- withCallingHandlers(
              convert_waterlevel_kPa_m(
                input_data         = wl_baro,
                select_station     = stn,
                reference_data     = ref_raw,
                reference_type     = ref_type,
                select_measurement = meas_idx,
                return_plots       = FALSE
              ),
              message = function(m) {
                cat("[convert_wl][msg]", conditionMessage(m))
                msgs <<- c(msgs, conditionMessage(m))
                invokeRestart("muffleMessage")
              },
              warning = function(w) {
                cat("[convert_wl][warn]", conditionMessage(w), "\n")
                invokeRestart("muffleWarning")
              }
            )
            res
          }, error = function(e) {
            cat("[convert_wl][ERROR]", stn, ":", conditionMessage(e), "\n")
            showNotification(
              sprintf("Conversion failed for %s: %s", stn, conditionMessage(e)),
              type = "warning", duration = 6
            )
            NULL
          })

          if (!is.null(stn_result)) {
            stn_wl <- stn_result$site_wl
            n_valid <- sum(!is.na(stn_wl$waterlevel_m))
            n_total <- nrow(stn_wl)
            cat("[convert_wl]", stn, ":", n_valid, "/", n_total, "valid waterlevel_m values\n")

            if (n_valid == 0) {
              # Check baro coverage to give a specific warning
              n_baro <- sum(!is.na(stn_wl$airpress_kPa))
              if (n_baro == 0) {
                showNotification(
                  sprintf("%s: All water levels are NA because no barometric pressure data matched. Snap timestamps to align baro and WL data.", stn),
                  type = "warning", duration = 10
                )
              } else {
                showNotification(
                  sprintf("%s: All water levels are NA. Check that the reference measurement timestamp aligns with available baro data.", stn),
                  type = "warning", duration = 10
                )
              }
            } else if (n_valid < n_total) {
              pct_valid <- round(100 * n_valid / n_total, 1)
              cat("[convert_wl]", stn, ": only", pct_valid, "% of records have valid water levels (baro gaps)\n")
            }

            all_site_wl[[stn]] <- stn_wl
            # Add station code to ref_dat if missing
            stn_ref <- stn_result$ref_dat
            if (!"site_station_code" %in% names(stn_ref)) {
              stn_ref$site_station_code <- stn
            }
            all_ref_dat[[stn]] <- stn_ref
          }
        }

        if (length(all_site_wl) == 0) {
          removeNotification("wl_convert_progress")
          showNotification("All station conversions failed.", type = "error", duration = 5)
          return()
        }

        combined_wl  <- dplyr::bind_rows(all_site_wl) %>%
          dplyr::arrange(site_station_code, timestamp)
        combined_ref <- dplyr::bind_rows(all_ref_dat)

        # Merge-back: preserve existing results for non-selected stations
        if (run_subset && !is.null(convert_result())) {
          existing_wl <- convert_result()$site_wl %>%
            dplyr::filter(!(site_station_code %in% target_stns))
          existing_ref <- convert_result()$ref_dat %>%
            dplyr::filter(!(site_station_code %in% target_stns))
          cat("[convert_wl] Merge-back: keeping", nrow(existing_wl), "existing rows\n")
          combined_wl  <- dplyr::bind_rows(existing_wl, combined_wl) %>%
            dplyr::arrange(site_station_code, timestamp)
          combined_ref <- dplyr::bind_rows(existing_ref, combined_ref)
        }

        result_list <- list(site_wl = combined_wl, ref_dat = combined_ref)
        convert_result(result_list)
        rv_converted_data$waterlevel_converted <- combined_wl

        removeNotification("wl_convert_progress")
        n_stns <- length(unique(combined_wl$site_station_code))
        cat("[convert_wl] Done. Converted", n_stns, "station(s),", nrow(combined_wl), "rows\n")
        showNotification(
          sprintf("Water level converted for %d station(s).", n_stns),
          type = "message", duration = 4
        )

      }, error = function(e) {
        cat("[convert_wl][ERROR]", conditionMessage(e), "\n")
        removeNotification("wl_convert_progress")
        showNotification(paste("Conversion error:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })

    # ---- Conversion summary table ----
    output$convert_summary_table <- DT::renderDT({
      res <- convert_result()
      req(res)
      df <- res$site_wl
      req(df, "waterlevel_m" %in% names(df))

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records    = dplyr::n(),
          `Valid WL`   = sum(!is.na(waterlevel_m)),
          `WL Min (m)` = if (all(is.na(waterlevel_m))) NA_real_ else round(min(waterlevel_m, na.rm = TRUE), 3),
          `WL Max (m)` = if (all(is.na(waterlevel_m))) NA_real_ else round(max(waterlevel_m, na.rm = TRUE), 3),
          `WL Mean (m)` = if (all(is.na(waterlevel_m))) NA_real_ else round(mean(waterlevel_m, na.rm = TRUE), 3),
          `Ref Type`    = dplyr::first(waterlevel_reference),
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
      res <- convert_result()
      req(res)
      stns <- sort(unique(res$site_wl$site_station_code))
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
        res <- convert_result()
        req(res)
        stns <- sort(unique(res$site_wl$site_station_code))
        if (row_idx <= length(stns)) {
          updateSelectInput(session, "convert_detail_station", selected = stns[row_idx])
        }
      }
    })

    # ---- Selected converted station data ----
    selected_convert_data <- reactive({
      res <- convert_result()
      stn <- input$convert_detail_station
      req(res, stn)
      dplyr::filter(res$site_wl, site_station_code == stn)
    })

    selected_convert_ref <- reactive({
      res <- convert_result()
      stn <- input$convert_detail_station
      req(res, stn)
      ref <- res$ref_dat
      if ("site_station_code" %in% names(ref)) {
        dplyr::filter(ref, site_station_code == stn)
      } else {
        ref
      }
    })

    # ---- Station detail info ----
    output$convert_station_info <- renderUI({
      df <- selected_convert_data()
      req(nrow(df) > 0, "waterlevel_m" %in% names(df))

      ref <- selected_convert_ref()
      n_ref <- if (!is.null(ref)) nrow(ref) else 0
      n_total    <- nrow(df)
      n_valid    <- sum(!is.na(df$waterlevel_m))
      date_range <- paste(format(min(df$timestamp), "%Y-%m-%d"),
                          "to", format(max(df$timestamp), "%Y-%m-%d"))

      if (n_valid == 0) {
        wl_range <- "no valid values"
        wl_style <- "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px;"
        wl_note  <- tags$p(style = "margin: 5px 0 0 0; color: #856404;",
          icon("exclamation-triangle"),
          " All water levels are NA. This usually means barometric pressure data did not match ",
          "the water level timestamps. Use Snap Timestamps in the Import step to align them."
        )
      } else {
        wl_range <- sprintf("%.3f \u2013 %.3f m",
                            min(df$waterlevel_m, na.rm = TRUE),
                            max(df$waterlevel_m, na.rm = TRUE))
        wl_style <- "background-color: #f8f9fa; padding: 12px; border-radius: 5px;"
        wl_note  <- NULL
      }

      tags$div(
        style = wl_style,
        tags$p(style = "margin: 0;",
          sprintf("%s records (%s valid) | WL range: %s | %s",
                  format(n_total, big.mark = ","), format(n_valid, big.mark = ","),
                  wl_range, date_range)
        ),
        tags$p(style = "margin: 5px 0 0 0; color: #555;",
          sprintf("%d reference point%s", n_ref, ifelse(n_ref == 1, "", "s"))
        ),
        wl_note
      )
    })

    # ---- Conversion review plot ----
    output$convert_wl_plot <- plotly::renderPlotly({
      df <- selected_convert_data()
      req(nrow(df) > 0, "waterlevel_m" %in% names(df))

      ref <- selected_convert_ref()

      p <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = df, x = ~timestamp, y = ~waterlevel_m,
          type = "scatter", mode = "lines",
          name = "Water Level (m)",
          line = list(color = "#1B5E20", width = 1.5)
        )

      # Add reference points as orange diamonds
      if (!is.null(ref) && nrow(ref) > 0 && "ref_m" %in% names(ref)) {
        p <- p %>% plotly::add_trace(
          data = ref, x = ~timestamp, y = ~ref_m,
          type = "scatter", mode = "markers",
          name = "Reference Points",
          marker = list(
            color = "#FF9800", size = 10, symbol = "diamond",
            line = list(color = "#E65100", width = 1)
          ),
          text = ~ifelse(!is.na(site_comments) & nchar(trimws(site_comments)) > 0,
                         site_comments, ""),
          hoverinfo = "x+y+text"
        )
      }

      p %>% plotly::layout(
        yaxis = list(title = "Water Level (m)"),
        xaxis = list(title = ""),
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.1)
      )
    })

    # ---- Reference comparison station label ----
    output$ref_comparison_station_label <- renderUI({
      stn <- input$convert_detail_station
      req(stn)
      df <- selected_convert_data()
      req(nrow(df) > 0)

      ref_type <- if ("waterlevel_reference" %in% names(df)) {
        dplyr::first(na.omit(df$waterlevel_reference))
      } else {
        input$reference_type %||% "stage"
      }
      ref_label <- if (ref_type == "depth") "depth" else "stage"
      date_range <- paste(format(min(df$timestamp), "%Y-%m-%d"),
                          "to", format(max(df$timestamp), "%Y-%m-%d"))

      tags$div(
        style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; border: 2px solid #2e7d32; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 14px;",
          "Comparing manual ", tags$strong(ref_label), " readings against sensor water level for station: ",
          tags$strong(stn, style = "color: #2e7d32;"),
          tags$span(style = "color: #555; margin-left: 10px;",
            sprintf("(logger period: %s)", date_range)
          )
        ),
        tags$p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
          "Only reference measurements within the logger deployment period are shown. ",
          "Sensor WL is interpolated from the converted time series at each measurement timestamp."
        )
      )
    })

    # ---- Reference measurement comparison table ----
    output$convert_ref_comparison_table <- DT::renderDT({
      df  <- selected_convert_data()
      stn <- input$convert_detail_station
      req(df, stn, nrow(df) > 0, "waterlevel_m" %in% names(df))

      # Get all reference readings for this station
      all_ref <- convert_ref_data()
      req(all_ref)
      stn_ref <- all_ref %>%
        dplyr::filter(site_station_code == stn) %>%
        dplyr::arrange(timestamp)

      if (nrow(stn_ref) == 0) return(NULL)

      # Filter to only in-range readings
      ts_range <- range(df$timestamp, na.rm = TRUE)
      stn_ref <- stn_ref %>%
        dplyr::filter(timestamp >= ts_range[1] & timestamp <= ts_range[2])

      if (nrow(stn_ref) == 0) return(NULL)

      # Determine reference type from the converted data
      ref_type <- if ("waterlevel_reference" %in% names(df)) {
        dplyr::first(na.omit(df$waterlevel_reference))
      } else {
        input$reference_type %||% "stage"
      }
      ref_col <- if (ref_type == "depth") "depth_m" else "stage_m"
      ref_label <- if (ref_type == "depth") "Depth (m)" else "Stage (m)"

      # Build the reference value column
      stn_ref$ref_value <- if (ref_col %in% names(stn_ref)) stn_ref[[ref_col]] else NA_real_

      # Interpolate sensor water level at each reference timestamp
      ts_numeric <- as.numeric(df$timestamp)
      wl_values  <- df$waterlevel_m

      valid_idx <- !is.na(wl_values)
      if (sum(valid_idx) >= 2) {
        ref_numeric <- as.numeric(stn_ref$timestamp)
        stn_ref$sensor_wl <- approx(
          x = ts_numeric[valid_idx],
          y = wl_values[valid_idx],
          xout = ref_numeric,
          rule = 1
        )$y
      } else {
        stn_ref$sensor_wl <- NA_real_
      }

      # Compute difference: manual reference - sensor
      stn_ref$difference <- stn_ref$ref_value - stn_ref$sensor_wl

      # Build display table
      display_df <- data.frame(
        Timestamp    = format(stn_ref$timestamp, "%Y-%m-%d %H:%M"),
        Reference    = round(stn_ref$ref_value, 4),
        `Sensor WL`  = round(stn_ref$sensor_wl, 4),
        Difference   = round(stn_ref$difference, 4),
        Comments     = ifelse(!is.na(stn_ref$site_comments) & nchar(trimws(stn_ref$site_comments)) > 0,
                              substr(trimws(stn_ref$site_comments), 1, 80), ""),
        check.names  = FALSE,
        stringsAsFactors = FALSE
      )
      names(display_df)[2] <- ref_label

      DT::datatable(display_df,
        selection = "none",
        options = list(
          dom = "tip",
          pageLength = 10,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = c(1, 2, 3))
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle("Difference",
          color = DT::styleInterval(c(-0.05, 0.05), c("#d32f2f", "#2e7d32", "#d32f2f"))
        )
    })

  })
}


# -----------------------------------------------------------------------------
# 3. QA Water Level
# -----------------------------------------------------------------------------
module_wl_qaqc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("QC Water Level", class = "custom-h1"),
      tags$h2("Automated quality checks and visual review", class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Run automated quality checks on converted water level data. The
        function creates ", tags$code("waterlevel_m_adj"), " and ",
        tags$code("watertemp_C_adj"), " columns, applying corrections for
        negative values and ice. Flags are added for dry, ice, and disturbance
        conditions for your review. Use the manual adjustment tools to remove
        bad periods, correct zero shifts, flatten spikes, or fix cable offsets."
      ),

      # ---- Data readiness status ----
      uiOutput(ns("qaqc_status")),

      hr(),

      # ================================================================
      # Section 1: Run Automated QAQC
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
              tags$li(tags$strong("NEGATIVE_WL"), " \u2014 negative water level set to 0"),
              tags$li(tags$strong("LOGGER_ICE"), " \u2014 temp < -1\u00b0C removed"),
              tags$li(tags$strong("NEGATIVE_WT"), " \u2014 temp -1 to 0\u00b0C set to 0"),
              tags$li(tags$strong("FLAG_DRY"), " \u2014 water press \u2264 air press + warm"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 water temp below ice threshold"),
              tags$li(tags$strong("FLAG_DISTURBANCE"), " \u2014 step change exceeds disturbance threshold")
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
            tags$code("qc_waterlevel()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Automated QAQC with flag and edit columns"),
            tags$code("adjust_logger_NA()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Remove data for ice / dry / disturbance periods"),
            tags$code("adjust_WL_zero()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Shift water level by constant offset"),
            tags$code("adjust_waterlevel_spike()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Linear interpolation across spike period"),
            tags$code("adjust_WL_offset()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Correct cable offset from timestamp onward")
          )
        ),

        # ---- Right: Station selector & run button (67%) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex; min-width: 0;",
          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1; min-width: 0; overflow-x: auto;",
            tags$div(
              style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px; border: 2px solid #2e7d32; margin-bottom: 15px;",
              selectInput(ns("station_select"), "Select Station:",
                choices = NULL, width = "100%")
            ),
            tags$h4("QAQC Thresholds", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666; margin-bottom: 10px;",
              icon("sliders-h"),
              " Adjust the thresholds used by ", tags$code("qc_waterlevel()"),
              ". Default values are shown."
            ),
            fluidRow(
              column(4,
                numericInput(ns("threshold_ice"), "Ice threshold (\u00b0C)",
                  value = 0.3, min = 0, max = 5, step = 0.1)
              ),
              column(4,
                numericInput(ns("threshold_dry"), "Dry temp threshold (\u00b0C)",
                  value = 19, min = 0, max = 40, step = 1)
              ),
              column(4,
                numericInput(ns("threshold_disturb"), "Disturbance threshold (m)",
                  value = 0.1, min = 0.01, max = 1, step = 0.01)
              )
            ),
            actionButton(ns("run_qaqc"), "Run Water Level QAQC",
              class = "btn-success btn-lg", width = "100%",
              icon = icon("play"),
              style = "color: #ffffff;"
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
        # Section 3: Manual Adjustments
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Manual Adjustments"
          )
        ),

        tags$p(
          class = "clean-print",
          "Use the plot below to review flagged data. Switch to ",
          tags$strong("Select Range"), " mode and drag across the time series ",
          "to highlight periods that need correction. Use ", tags$strong("Zoom"),
          " mode to navigate and inspect the data more closely. Once you have ",
          "selected one or more ranges, click an action button to apply the ",
          "appropriate correction \u2014 remove bad data, shift the zero reference, ",
          "flatten a spike, or fix a cable offset."
        ),

        tags$div(
          style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #e0e0e0;",

          # Controls — centered, stacked vertically
          tags$div(
            style = "display: flex; flex-direction: column; align-items: center; gap: 10px;",

            # Row 1: Mode toggle
            shinyWidgets::radioGroupButtons(
              inputId = ns("plot_mode"),
              label = NULL,
              choices = c("Zoom" = "zoom", "Select Range" = "select"),
              selected = "zoom",
              size = "lg",
              status = "success"
            ),

            # Row 2: Range management + 4 action buttons
            tags$div(
              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; justify-content: center;",
              actionButton(ns("undo_range"), "Undo",
                class = "btn-default btn-sm", icon = icon("undo")),
              actionButton(ns("clear_ranges"), "Clear Ranges",
                class = "btn-default btn-sm", icon = icon("times")),
              actionButton(ns("btn_remove"), "Remove Selected",
                class = "btn-warning btn-sm", icon = icon("eraser"),
                style = "color: #fff;"),
              actionButton(ns("btn_zero"), "Adjust WL Zero",
                class = "btn-info btn-sm", icon = icon("arrows-alt-v"),
                style = "color: #fff;"),
              actionButton(ns("btn_spike"), "Flatten Spike",
                class = "btn-danger btn-sm", icon = icon("chart-line"),
                style = "color: #fff;"),
              actionButton(ns("btn_offset"), "Cable Offset",
                class = "btn-dark btn-sm", icon = icon("exchange-alt"),
                style = "color: #000;")
            ),

            # Row 3: Range status
            uiOutput(ns("range_status"))
          ),

          # Adjustment explanation panel
          tags$div(
            style = "margin-top: 20px; padding: 15px; background-color: #e8f5e9; border-radius: 5px;",
            fluidRow(
              column(3,
                tags$h5("Remove Selected", style = "margin-top: 0; color: #856404;"),
                tags$p(style = "font-size: 12px;",
                  "Set adjusted values to NA in selected ranges. ",
                  "Choose reason: ice, dry, or disturbance. ",
                  tags$code("adjust_logger_NA()")
                )
              ),
              column(3,
                tags$h5("Adjust WL Zero", style = "margin-top: 0; color: #0c5460;"),
                tags$p(style = "font-size: 12px;",
                  "Shift water level by a constant offset (metres). ",
                  "Apply to selected range or entire series. ",
                  tags$code("adjust_WL_zero()")
                )
              ),
              column(3,
                tags$h5("Flatten Spike", style = "margin-top: 0; color: #721c24;"),
                tags$p(style = "font-size: 12px;",
                  "Replace spike with linear interpolation between ",
                  "start and end of one selected range. ",
                  tags$code("adjust_waterlevel_spike()")
                )
              ),
              column(3,
                tags$h5("Cable Offset", style = "margin-top: 0; color: #1b1e21;"),
                tags$p(style = "font-size: 12px;",
                  "Correct step change from cable break. Uses one ",
                  "range: start=pre-break, end=post-break reference. ",
                  tags$code("adjust_WL_offset()")
                )
              )
            )
          )
        ),

        # ================================================================
        # Section 4: Visual Review
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Visual Review"
          )
        ),

        tags$div(
          style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #e0e0e0;",

          # Station info header
          uiOutput(ns("station_info")),

          # Time series plot (4 panels)
          tags$div(style = "margin-top: 15px;",
            plotlyOutput(ns("qaqc_plot"), height = "850px") %>% withSpinner()
          ),

          # Flag review plot (separate)
          tags$div(style = "margin-top: 15px;",
            plotlyOutput(ns("flag_plot"), height = "250px") %>% withSpinner()
          )
        ),

        # ================================================================
        # Section 5: Station Summary Statistics
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Station Summary Statistics"
          )
        ),

        uiOutput(ns("summary_station_label")),

        fluidRow(
          column(
            width = 6,
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("Water Level Summary (raw vs adjusted)", style = "margin-top: 0;"),
              verbatimTextOutput(ns("wl_summary"))
            )
          ),
          column(
            width = 6,
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
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
                "Download the QAQC'd water level data as CSV for the ",
                tags$strong("selected station"), ". The exported file ",
                "contains raw columns, adjusted columns, and all edit/flag ",
                "columns. The QAQC log records every automated and manual ",
                "adjustment applied."
              ),
              tags$p(style = "font-size: 13px; color: #666;",
                "Note: unlike barometric export, water level data is exported ",
                "per station. Select the station you want to export before ",
                "downloading."
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
    tags$div(
      style = "text-align: right; margin-top: 20px; margin-bottom: 10px;",
      tags$a(
        href = "#",
        onclick = "$(\"a[href='#shiny-tab-t_wl_datum']\").tab('show'); return false;",
        style = "color: #2e7d32; font-weight: bold; font-size: 14px;",
        "Ready to proceed? (Optionally) Continue to Adjust Datum ",
        icon("arrow-right")
      )
    ),
    .wireframe_footer()
  )
}

module_wl_qaqc_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_converted_data <- session$userData$rv_converted_data
    rv_qaqc_data      <- session$userData$rv_qaqc_data
    rv_raw_data        <- session$userData$rv_raw_data

    # Module-local state
    wl_qc_result    <- reactiveVal(NULL)   # full dataset (all stations)
    qaqc_log        <- reactiveVal(NULL)   # accumulated QAQC log

    # Range selection
    selected_ranges <- reactiveVal(list())
    range_history   <- reactiveVal(list())

    # Conditional panel visibility
    output$show_results <- reactive({ !is.null(wl_qc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    # ------ Populate station selector from converted data ------
    observe({
      df <- rv_converted_data$waterlevel_converted
      req(df, nrow(df) > 0)
      stations <- sort(unique(df$site_station_code))
      # Only stations with at least some non-NA waterlevel_m
      stations <- stations[sapply(stations, function(s) {
        any(!is.na(df$waterlevel_m[df$site_station_code == s]))
      })]
      updateSelectInput(session, "station_select",
        choices = stations, selected = stations[1])
    })

    # ------ Data readiness status ------
    output$qaqc_status <- renderUI({
      df <- rv_converted_data$waterlevel_converted
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No converted water level data available. Complete Part B (Convert Pressure to Water Depth) first."
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
      converted_data <- rv_converted_data$waterlevel_converted
      req(converted_data)

      tryCatch({
        showNotification("Running Water Level QAQC...",
          type = "message", duration = NULL, id = "wl_qaqc_progress")

        result <- qc_waterlevel(
          input_data             = converted_data,
          select_station         = station,
          temp_ice_flag_C        = input$threshold_ice,
          temp_dry_flag_C        = input$threshold_dry,
          wl_spike_flag_m        = input$threshold_disturb,
          log_root               = NULL,
          user                   = NULL
        )

        qaqc_log(attr(result, "qaqc_log"))

        # Merge: replace station in full dataset
        other_data <- dplyr::filter(converted_data, site_station_code != station)
        full_data <- dplyr::bind_rows(other_data, result) %>%
          dplyr::arrange(site_station_code, timestamp)

        wl_qc_result(full_data)
        rv_qaqc_data$qc_waterlevel <- full_data

        # Reset range state
        selected_ranges(list())
        range_history(list())

        removeNotification("wl_qaqc_progress")
        showNotification(sprintf("QAQC complete for %s", station),
          type = "message", duration = 5)

      }, error = function(e) {
        removeNotification("wl_qaqc_progress")
        showNotification(paste("QAQC error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Selected station data ------
    selected_data <- reactive({
      req(input$station_select, wl_qc_result())
      dplyr::filter(wl_qc_result(), site_station_code == input$station_select)
    })

    # ------ Station info ------
    output$station_info <- renderUI({
      data <- selected_data()
      req(nrow(data) > 0)

      flag_cols <- c("FLAG_DISTURBANCE", "FLAG_ICE", "OUTLIER_WT", "FLAG_DRY")
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
      req(wl_qc_result())
      data <- selected_data()
      req(nrow(data) > 0)

      # Build summary with guards for columns that may not exist
      safe_sum <- function(col) {
        if (col %in% names(data)) sum(as.logical(data[[col]]), na.rm = TRUE) else 0L
      }

      summary_df <- data.frame(
        Station = data$site_station_code[1],
        Records = nrow(data),
        FLAG_DISTURBANCE = safe_sum("FLAG_DISTURBANCE"),
        FLAG_ICE = safe_sum("FLAG_ICE"),
        OUTLIER_WT = safe_sum("OUTLIER_WT"),
        FLAG_DRY = safe_sum("FLAG_DRY"),
        WL_NA = sum(is.na(data$waterlevel_m_adj)),
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

    # ------ QAQC Plot (3-panel time series) ------
    # JS callback: intercepts drag in Select Range mode
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

    output$qaqc_plot <- renderPlotly({
      data <- selected_data()
      req(nrow(data) > 0)
      station <- input$station_select

      # Panel 1: Water Level
      p1 <- plot_ly(data, x = ~timestamp, y = ~waterlevel_m,
              type = "scattergl", mode = "lines",
              line = list(color = "rgba(35,61,77,0.4)", width = 1),
              name = "Raw WL",
              hovertemplate = "Raw: %{y:.3f} m<extra></extra>") %>%
        add_trace(y = ~waterlevel_m_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#233d4d", width = 1),
              name = "Adjusted WL",
              hovertemplate = "Adj: %{y:.3f} m<extra></extra>")

      # Panel 2: Pressure
      p2 <- plot_ly(data, x = ~timestamp, y = ~waterpress_kPa,
              type = "scattergl", mode = "lines",
              line = list(color = "#915e3d", width = 1),
              name = "Water Pressure",
              hovertemplate = "Water: %{y:.2f} kPa<extra></extra>") %>%
        add_trace(y = ~airpress_kPa,
              type = "scattergl", mode = "lines",
              line = list(color = "rgba(145,94,61,0.4)", width = 1),
              name = "Air Pressure",
              hovertemplate = "Air: %{y:.2f} kPa<extra></extra>")

      # Panel 3: Water Temperature
      p3 <- plot_ly(data, x = ~timestamp, y = ~watertemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "rgba(97,155,138,0.4)", width = 1),
              name = "Raw Water Temp",
              hovertemplate = "Raw: %{y:.1f} \u00b0C<extra></extra>") %>%
        add_trace(y = ~watertemp_C_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#619b8a", width = 1),
              name = "Adj Water Temp",
              hovertemplate = "Adj: %{y:.1f} \u00b0C<extra></extra>")

      # Panel 4: Air Temperature
      p4 <- plot_ly(data, x = ~timestamp, y = ~airtemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "#FDB840", width = 1),
              name = "Air Temp",
              hovertemplate = "Air: %{y:.1f} \u00b0C<extra></extra>")

      subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = paste("Water Level QAQC -", station), font = list(size = 14)),
          xaxis = list(title = ""),
          xaxis2 = list(title = ""),
          xaxis3 = list(title = ""),
          xaxis4 = list(title = "Date"),
          yaxis  = list(title = "Water Level (m)", fixedrange = TRUE),
          yaxis2 = list(title = "Pressure (kPa)", fixedrange = TRUE),
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
            // Shared sync guard (global per module) prevents cross-widget loops
            if (!window._wlSyncGuard) window._wlSyncGuard = false;
            el.on('plotly_relayout', function(evt) {
              if (window._wlSyncGuard) return;
              // Skip when in select mode — drag_range_js handles restore
              var mode = Shiny.shinyapp.$inputValues[modeId];
              if (mode === 'select') return;
              var keys = Object.keys(evt);
              var isShapeOnly = keys.every(function(k) {
                return k === 'shapes' || k.indexOf('shapes[') === 0;
              });
              if (isShapeOnly) return;
              var flagEl = document.getElementById(flagId);
              if (!flagEl || !flagEl.layout) return;
              window._wlSyncGuard = true;
              if (evt['xaxis.autorange']) {
                Plotly.relayout(flagEl, {'xaxis.autorange': true}).then(function() { window._wlSyncGuard = false; });
              } else {
                var x0 = evt['xaxis.range[0]'];
                var x1 = evt['xaxis.range[1]'];
                if (x0 && x1) {
                  Plotly.relayout(flagEl, {'xaxis.range[0]': x0, 'xaxis.range[1]': x1}).then(function() { window._wlSyncGuard = false; });
                } else { window._wlSyncGuard = false; }
              }
            });
            el.on('plotly_doubleclick', function() {
              var flagEl = document.getElementById(flagId);
              if (flagEl) {
                window._wlSyncGuard = true;
                Plotly.relayout(flagEl, {'xaxis.autorange': true}).then(function() { window._wlSyncGuard = false; });
              }
            });
          }
        ", session$ns("flag_plot"), session$ns("plot_mode")))
    })

    # ------ Flag review plot (separate) ------
    output$flag_plot <- renderPlotly({
      data <- selected_data()
      req(nrow(data) > 0)

      # Collect all flag/edit columns that exist and have TRUE values
      flag_cols <- c("FLAG_DISTURBANCE", "FLAG_ICE", "OUTLIER_WT", "FLAG_DRY")
      edit_cols <- grep("^edit_", names(data), value = TRUE)
      all_flag_cols <- intersect(c(flag_cols, edit_cols), names(data))

      # Plotly symbols cycling
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
            if (!window._wlSyncGuard) window._wlSyncGuard = false;
            el.on('plotly_relayout', function(evt) {
              if (window._wlSyncGuard) return;
              var keys = Object.keys(evt);
              var isShapeOnly = keys.every(function(k) {
                return k === 'shapes' || k.indexOf('shapes[') === 0;
              });
              if (isShapeOnly) return;
              var mainEl = document.getElementById(mainId);
              if (!mainEl || !mainEl.layout) return;
              window._wlSyncGuard = true;
              if (evt['xaxis.autorange']) {
                Plotly.relayout(mainEl, {'xaxis.autorange': true}).then(function() { window._wlSyncGuard = false; });
              } else {
                var x0 = evt['xaxis.range[0]'];
                var x1 = evt['xaxis.range[1]'];
                if (x0 && x1) {
                  Plotly.relayout(mainEl, {'xaxis.range[0]': x0, 'xaxis.range[1]': x1}).then(function() { window._wlSyncGuard = false; });
                } else { window._wlSyncGuard = false; }
              }
            });
            el.on('plotly_doubleclick', function() {
              var mainEl = document.getElementById(mainId);
              if (mainEl) {
                window._wlSyncGuard = true;
                Plotly.relayout(mainEl, {'xaxis.autorange': true}).then(function() { window._wlSyncGuard = false; });
              }
            });
          }
        ", session$ns("qaqc_plot")))
    })

    # ------ Multi-range selection via drag ------
    # Helper: merge overlapping ranges (sorted by start)
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

    # Receive drag range from JS onRender callback
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

    # Draw selection rectangles when ranges are added
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

    # Reset ranges on station change
    observeEvent(input$station_select, {
      selected_ranges(list())
      range_history(list())
    })

    # Undo last range addition
    observeEvent(input$undo_range, {
      hist <- range_history()
      if (length(hist) > 0) {
        selected_ranges(hist[[length(hist)]])
        range_history(hist[-length(hist)])
      }
    })

    # Clear all ranges — force plot re-render to remove JS-drawn shapes
    observeEvent(input$clear_ranges, {
      range_history(c(range_history(), list(selected_ranges())))
      selected_ranges(list())
      df <- wl_qc_result()
      if (!is.null(df)) {
        attr(df, ".redraw") <- Sys.time()
        wl_qc_result(df)
      }
    })

    # Status text showing selected range count
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

    # ------ Helper: snap a character timestamp to nearest actual data timestamp ------
    # Plotly sends timestamps in various formats (with/without fractional seconds,
    # date-only at edges, or epoch ms). Try multiple parsers to handle all cases.
    parse_plotly_ts <- function(ts_char) {
      # Try common formats in order
      ts <- lubridate::ymd_hms(ts_char, quiet = TRUE)
      if (!is.na(ts)) return(ts)
      ts <- lubridate::ymd_hm(ts_char, quiet = TRUE)
      if (!is.na(ts)) return(ts)
      ts <- lubridate::ymd(ts_char, quiet = TRUE)
      if (!is.na(ts)) return(ts)
      # Plotly sometimes sends "YYYY-MM-DD HH:MM:SS.ffffff" — strip fractional part
      stripped <- sub("\\.\\d+$", "", ts_char)
      ts <- lubridate::ymd_hms(stripped, quiet = TRUE)
      if (!is.na(ts)) return(ts)
      # Last resort: try base R
      ts <- tryCatch(as.POSIXct(ts_char), error = function(e) NA)
      ts
    }

    snap_to_data <- function(ts_char, timestamps) {
      ts <- parse_plotly_ts(ts_char)
      if (is.na(ts)) return(ts_char)
      idx <- which.min(abs(as.numeric(difftime(timestamps, ts, units = "secs"))))
      format(timestamps[idx], "%Y-%m-%d %H:%M:%S")
    }

    # Snap both ends of each range to actual timestamps in station data
    snap_ranges <- function(ranges, station_timestamps) {
      lapply(ranges, function(r) {
        c(snap_to_data(r[1], station_timestamps),
          snap_to_data(r[2], station_timestamps))
      })
    }

    # ------ Helper: merge station data back into full dataset ------
    merge_station_back <- function(full_data, station_data, station) {
      other_data <- dplyr::filter(full_data, site_station_code != station)
      dplyr::bind_rows(other_data, station_data) %>%
        dplyr::arrange(site_station_code, timestamp)
    }

    # Helper: post-adjustment update (shared by all 4 adjustment buttons)
    apply_adjustment_result <- function(station_data, station) {
      updated <- merge_station_back(wl_qc_result(), station_data, station)
      selected_ranges(list())
      range_history(list())
      wl_qc_result(updated)
      rv_qaqc_data$qc_waterlevel <- updated

      # Append adjustment log to accumulated qaqc_log
      adj_log <- attr(station_data, "qaqc_log")
      if (!is.null(adj_log)) {
        qaqc_log(dplyr::bind_rows(qaqc_log(), adj_log))
      }
    }

    # ------ Button 1: Remove Selected Periods (adjust_logger_NA) ------
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
        station_data <- dplyr::filter(wl_qc_result(), site_station_code == station)

        # Snap range endpoints to nearest actual timestamps in data
        snapped <- snap_ranges(ranges, station_data$timestamp)

        for (r in snapped) {
          station_data <- adjust_logger_NA(
            input_data       = station_data,
            select_station   = station,
            metric           = "waterlevel",
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

    # ------ Button 2: Adjust WL Zero (adjust_WL_zero) ------
    observeEvent(input$btn_zero, {
      ranges <- selected_ranges()

      range_text <- if (length(ranges) == 0) {
        tags$p(tags$em("No range selected \u2014 offset will be applied to entire series."))
      } else {
        tags$ul(lapply(ranges, function(r) tags$li(paste(r[1], "to", r[2]))))
      }

      showModal(modalDialog(
        title = "Adjust Water Level Zero",
        tags$p(sprintf("Station: %s", input$station_select)),
        tags$h5("Selected Ranges:"),
        range_text,
        numericInput(session$ns("modal_zero_value"), "Offset value (metres):",
          value = 0, step = 0.01),
        textInput(session$ns("modal_zero_note"), "Note (required):", value = ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("modal_zero_confirm"), "Apply Zero Adjustment",
            class = "btn-info", icon = icon("arrows-alt-v"),
            style = "color: #ffffff;")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$modal_zero_confirm, {
      note <- input$modal_zero_note
      if (is.null(note) || nchar(trimws(note)) == 0) {
        showNotification("Enter a note describing the zero adjustment.",
          type = "error", duration = 3)
        return()
      }

      removeModal()
      station <- input$station_select
      req(station)
      change_val <- input$modal_zero_value
      ranges <- selected_ranges()

      tryCatch({
        station_data <- dplyr::filter(wl_qc_result(), site_station_code == station)

        if (length(ranges) > 0) {
          # Snap range endpoints to nearest actual timestamps in data
          snapped <- snap_ranges(ranges, station_data$timestamp)

          # Apply to each selected range
          for (r in snapped) {
            station_data <- adjust_WL_zero(
              input_data      = station_data,
              select_station  = station,
              change_value    = change_val,
              timestamp_start = r[1],
              timestamp_end   = r[2],
              manual_note     = paste0(note, " (", r[1], " to ", r[2], ")"),
              log_root        = NULL
            )
          }
        } else {
          # Apply to entire series
          station_data <- adjust_WL_zero(
            input_data      = station_data,
            select_station  = station,
            change_value    = change_val,
            timestamp_start = NULL,
            timestamp_end   = NULL,
            manual_note     = note,
            log_root        = NULL
          )
        }

        apply_adjustment_result(station_data, station)
        showNotification(
          sprintf("Zero adjustment applied for %s (%.3f m)", station, change_val),
          type = "message", duration = 5)

      }, error = function(e) {
        showNotification(paste("Zero adjustment error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Button 3: Flatten Spike (adjust_waterlevel_spike) ------
    observeEvent(input$btn_spike, {
      ranges <- selected_ranges()
      if (length(ranges) != 1) {
        showNotification("Select exactly 1 range for spike flattening.",
          type = "warning", duration = 4)
        return()
      }

      showModal(modalDialog(
        title = "Flatten Water Level Spike",
        tags$p(sprintf("Station: %s", input$station_select)),
        tags$p(sprintf("Range: %s to %s", ranges[[1]][1], ranges[[1]][2])),
        tags$p(style = "font-size: 13px; color: #666;",
          "Values within this range will be replaced with linear interpolation ",
          "between the raw water level at the start and end timestamps. ",
          "This is useful for removing short-lived anomalies caused by ice build-up, ",
          "debris, or sensor disturbance that produce sudden jumps or dips in the record."
        ),
        tags$p(style = "font-size: 13px; color: #666; margin-top: 6px;",
          tags$strong("Example: "), "A sensor frozen in ice might read 0.85 m, then spike ",
          "to 1.40 m as ice shifts, before returning to 0.87 m. After flattening, the ",
          "adjusted column will show a smooth, straight line from 0.85 m to 0.87 m across ",
          "that period, and the spike values will be overwritten with the interpolated values."
        ),
        selectInput(session$ns("modal_spike_reason"), "Reason:",
          choices = c("ice", "disturbance"),
          selected = "ice"),
        textInput(session$ns("modal_spike_note"), "Note (required):", value = ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("modal_spike_confirm"), "Flatten Spike",
            class = "btn-danger", icon = icon("chart-line"),
            style = "color: #ffffff;")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$modal_spike_confirm, {
      note <- input$modal_spike_note
      if (is.null(note) || nchar(trimws(note)) == 0) {
        showNotification("Enter a note describing the spike.",
          type = "error", duration = 3)
        return()
      }

      removeModal()
      station <- input$station_select
      req(station)
      ranges <- selected_ranges()
      req(length(ranges) == 1)

      tryCatch({
        station_data <- dplyr::filter(wl_qc_result(), site_station_code == station)

        # Snap range endpoints to nearest actual timestamps in data
        snapped <- snap_ranges(ranges, station_data$timestamp)

        station_data <- adjust_waterlevel_spike(
          input_data       = station_data,
          select_station   = station,
          timestamp_start  = snapped[[1]][1],
          timestamp_end    = snapped[[1]][2],
          reason_to_adjust = input$modal_spike_reason,
          manual_note      = note,
          log_root         = NULL
        )

        apply_adjustment_result(station_data, station)
        showNotification(
          sprintf("Spike flattened for %s", station),
          type = "message", duration = 5)

      }, error = function(e) {
        showNotification(paste("Spike flatten error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Button 4: Cable Offset (adjust_WL_offset) ------
    observeEvent(input$btn_offset, {
      ranges <- selected_ranges()
      if (length(ranges) != 1) {
        showNotification("Select exactly 1 range for cable offset correction.",
          type = "warning", duration = 4)
        return()
      }

      showModal(modalDialog(
        title = "Cable Offset Correction",
        tags$p(sprintf("Station: %s", input$station_select)),
        tags$p(tags$strong("Pre-break reference: "), ranges[[1]][1]),
        tags$p(tags$strong("Post-break reference: "), ranges[[1]][2]),
        tags$p(style = "font-size: 13px; color: #666;",
          "Offset is computed from the raw water level difference at these two ",
          "timestamps and applied from the post-break timestamp onward. ",
          "This corrects for a sudden, persistent shift in the water level record caused ",
          "by physical movement of the sensor cable (e.g., the cable slipping in its housing, ",
          "or being pulled during a site visit)."
        ),
        tags$p(style = "font-size: 13px; color: #666; margin-top: 6px;",
          tags$strong("Example: "), "A logger reads a steady base level of 0.50 m. After a ",
          "site visit the record jumps to 0.55 m and stays there, even though actual water ",
          "levels haven't changed. The offset (0.05 m) is calculated from the difference at ",
          "the two reference timestamps you selected, and all values from the post-break point ",
          "onward are shifted down by 0.05 m so the record is continuous again."
        ),
        textInput(session$ns("modal_offset_note"), "Note (required):", value = ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("modal_offset_confirm"), "Apply Offset",
            class = "btn-dark", icon = icon("exchange-alt"),
            style = "color: #ffffff;")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$modal_offset_confirm, {
      note <- input$modal_offset_note
      if (is.null(note) || nchar(trimws(note)) == 0) {
        showNotification("Enter a note describing the offset correction.",
          type = "error", duration = 3)
        return()
      }

      removeModal()
      station <- input$station_select
      req(station)
      ranges <- selected_ranges()
      req(length(ranges) == 1)

      tryCatch({
        station_data <- dplyr::filter(wl_qc_result(), site_station_code == station)

        # Snap range endpoints to nearest actual timestamps in data
        snapped <- snap_ranges(ranges, station_data$timestamp)

        station_data <- adjust_WL_offset(
          input_data      = station_data,
          select_station  = station,
          timestamp_start = snapped[[1]][1],
          timestamp_end   = snapped[[1]][2],
          manual_note     = note,
          log_root        = NULL
        )

        apply_adjustment_result(station_data, station)
        showNotification(
          sprintf("Cable offset applied for %s", station),
          type = "message", duration = 5)

      }, error = function(e) {
        showNotification(paste("Offset error:", conditionMessage(e)),
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
        style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; border: 2px solid #2e7d32; margin-bottom: 15px;",
        tags$p(style = "margin: 0; font-size: 14px;",
          "Showing summary statistics for station: ",
          tags$strong(stn, style = "color: #2e7d32;"),
          tags$span(style = "color: #555; margin-left: 10px;",
            sprintf("(%s records, %s)", format(nrow(data), big.mark = ","), date_range)
          )
        )
      )
    })

    output$wl_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      n_dup <- sum(duplicated(data$timestamp))
      cat("Records:", nrow(data), " | Duplicate timestamps:", n_dup, "\n\n")
      cat("--- Raw Water Level (m) ---\n")
      print(round(summary(data$waterlevel_m), 3))
      cat("\n--- Adjusted Water Level (m) ---\n")
      if ("waterlevel_m_adj" %in% names(data)) {
        print(round(summary(data$waterlevel_m_adj), 3))
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
      if ("watertemp_C_adj" %in% names(data)) {
        print(round(summary(data$watertemp_C_adj), 1))
      } else {
        cat("(column not yet created)\n")
      }
    })

    # ------ Download: Clean CSV ------
    output$download_csv <- downloadHandler(
      filename = function() {
        station <- input$station_select
        paste0("wl_qaqc_", station, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        data <- selected_data()
        req(nrow(data) > 0)

        export_cols <- c(
          "site_station_code", "timestamp",
          "waterlevel_m", "waterlevel_m_adj",
          "watertemp_C", "watertemp_C_adj",
          "waterpress_kPa", "airpress_kPa", "airtemp_C"
        )
        flag_cols <- grep("^(edit_|flag_|neg_)", names(data), value = TRUE)
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
        paste0("wl_qaqc_log_", station, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        log <- qaqc_log()
        if (!is.null(log) && nrow(log) > 0) {
          # Filter to selected station if station column exists
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

    # ------ Download: Station Metadata ------
    output$download_metadata <- downloadHandler(
      filename = function() {
        paste0("wl_station_metadata_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        meta <- rv_raw_data$metadata
        if (!is.null(meta) && nrow(meta) > 0) {
          wl_meta <- meta[meta$metric %in% c("waterlevel", "water level", "WL"), ]
          write.csv(wl_meta, file, row.names = FALSE)
        } else {
          write.csv(data.frame(), file, row.names = FALSE)
        }
      }
    )

  })
}


## module_wl_adjust_ui / module_wl_adjust_server removed —
## Manual adjustment functionality is now part of module_wl_qaqc.


# -----------------------------------------------------------------------------
# 4. Adjust Datum
# -----------------------------------------------------------------------------
module_wl_datum_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Adjust Datum", class = "custom-h1"),

      tags$p(
        class = "clean-print",
        "To convert a sensor's depth readings to actual elevations using nearby ",
        "benchmarks, the primary method used is differential leveling. This process ",
        "relates the sensor's output (effective stage) to a fixed vertical reference ",
        "plane known as the gauge datum."
      ),

      tags$p(
        class = "clean-print", style = "font-weight: bold;",
        "The summary of the method involves the following steps:"
      ),

      tags$ol(
        class = "clean-print", style = "padding-left: 20px;",
        tags$li(
          tags$strong("Establish the Gauge Datum and Reference Network"),
          " \u2014 The nearby benchmarks serve as a permanent, fixed reference network ",
          "for the site. One of these benchmarks is assigned an elevation to define the ",
          "gauge datum, which provides a common surface to which all water levels will ",
          "be referred."
        ),
        tags$li(
          tags$strong("Obtain a Direct Water Level Measurement"),
          " \u2014 Using a surveying level (optical, digital, or a total station) and a ",
          "graduated leveling rod, a surveyor measures the vertical difference between ",
          "the known benchmark and the current water surface. The water level read on ",
          "the rod is added or subtracted to the surveyed elevation of the benchmark to ",
          "determine the actual reference water level (the true elevation of the water ",
          "surface). If the water surface is choppy, a temporary stilling well, such as ",
          "a bucket with holes in the bottom, can be used to stabilize the water for an ",
          "accurate reading."
        ),
        tags$li(
          tags$strong("Calculate the Sensor Reset Correction (SRC)"),
          " \u2014 Once the actual reference water level is calculated, it is compared to ",
          "the depth value recorded by the automated stage sensor at that exact coinciding ",
          "time. The difference between these two values is called the Sensor Reset ",
          "Correction (SRC), which is calculated using the formula: ",
          tags$code("SRC = Reference Water Level \u2013 Stage Sensor"), "."
        ),
        tags$li(
          tags$strong("Apply Corrections to the Sensor Data"),
          " \u2014 If the absolute difference between the surveyed water level and the ",
          "sensor reading is greater than 0.003 metres, the SRC must be applied to the ",
          "sensor's records. This offset effectively converts the sensor's raw depth ",
          "readings into true elevations tied to the benchmark's datum."
        )
      ),

      tags$p(
        class = "clean-print",
        "Additionally, if there is an established physical reference gauge in the water ",
        "(such as a staff gauge or electric tape gauge), it must also be leveled against ",
        "the benchmarks. If leveling reveals that the reference gauge has shifted ",
        "vertically, a Gauge Correction (GC) is calculated ",
        "(Surveyed Elevation \u2013 Established Elevation) and applied to the data to ",
        "ensure the records remain perfectly tied to the benchmark elevations."
      ),

      tags$p(
        class = "clean-print",
        style = "background-color: #e3f2fd; padding: 12px; border-radius: 5px;",
        "This section may require more extensive background reading before continuing ",
        "further. Please see the ",
        tags$a(
          href = "https://publications.gc.ca/collections/collection_2021/eccc/en37/En37-465-2019-eng.pdf",
          target = "_blank",
          style = "color: #1565c0; font-weight: bold;",
          "Manual of Hydrometric Data Computation and Publication Procedures"
        ),
        " and the associated ",
        tags$a(
          href = "https://www2.gov.bc.ca/assets/gov/environment/natural-resource-stewardship/nr-laws-policy/risc/man_bc_hydrometric_stand_v2.pdf",
          target = "_blank",
          style = "color: #1565c0; font-weight: bold;",
          "Manual of British Columbia Hydrometric Standards"
        ), "."
      ),

      tags$h2("Convert relative stage to absolute elevation using surveyed benchmarks",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Datum adjustment anchors water level data to an arbitrary but consistent ",
        "elevation reference. The first valid levelling visit's primary benchmark is ",
        "set to a user-specified elevation (default 100 m). A benchmark network is ",
        "built across all visits, and the water surface elevation at the anchor visit ",
        "determines a single constant offset applied to all logger readings. The result ",
        "is a new ", tags$code("wl_elev_m"), " column representing water level on the datum."
      ),

      # ---- Data readiness status ----
      uiOutput(ns("datum_status")),

      hr(),

      # ================================================================
      # Section 1: Benchmark Level Runs
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Benchmark Level Runs"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Benchmark format info (33%) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px; flex: 1;",
            tags$h4("Benchmark CSV Format", style = "margin-top: 0;"),
            tags$p(
              "The benchmark file contains levelling-run survey data, with one row ",
              "per visit per station."
            ),
            tags$p(style = "font-weight: bold; margin-bottom: 5px;",
              "Required columns:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(
                tags$code("site_station_code"), " \u2014 station identifier (must match ",
                "the station name of the time series)"
              ),
              tags$li(
                tags$code("visit_date"), " \u2014 date of levelling visit. Accepted ",
                "formats: ", tags$code("YYYY-MM-DD"), " (e.g. ", tags$code("2025-04-18"),
                "), ", tags$code("MM/DD/YYYY"), " (e.g. ", tags$code("4/18/2025"),
                "), or ", tags$code("DD/MM/YYYY"), "."
              ),
              tags$li(
                tags$code("visit_time"), " \u2014 time of levelling visit. Accepted ",
                "formats: ", tags$code("HH:MM"), " (e.g. ", tags$code("13:30"),
                ") or ", tags$code("HH:MM:SS"), "."
              ),
              tags$li(
                tags$code("primary_bm"), " \u2014 primary benchmark name (must match a ",
                "BM column, e.g. ", tags$code("BM1"), "). This indicates the Primary ",
                "Benchmark (PBM). The primary benchmark is the most stable mark in the ",
                "local reference network, and it serves as the foundational starting ",
                "point for the levelling circuit used to monitor the site's stability."
              ),
              tags$li(
                tags$code("grade"), " \u2014 Excellent, Good, OK, or Poor. This refers ",
                "to the quality grade or qualifier of the survey or the data. Hydrometric ",
                "data and levelling circuits are evaluated against specific survey ",
                "tolerances (such as a maximum allowable closure error) and graded to ",
                "indicate the validity, accuracy, and reliability of the records."
              ),
              tags$li(
                "One or more ", tags$code("BM*"), " / ", tags$code("TBM*"),
                " columns \u2014 survey readings (e.g. ", tags$code("BM1"), ", ",
                tags$code("BM2"), ", ", tags$code("BM3"), "). These columns represent ",
                "the individual benchmarks in the station's local reference network. A ",
                "station must have a minimum of three permanent, independent benchmarks ",
                "to ensure stability can be reliably verified over time, and so the gauge ",
                "datum can be recovered if one mark is destroyed. The numerical values ",
                "(e.g. 0.12, 0.15) represent their surveyed elevations in metres relative ",
                "to the gauge datum."
              ),
              tags$li(
                "One or more ", tags$code("water_elev_*"), " columns \u2014 water surface ",
                "readings (e.g. ", tags$code("water_elev_1"), ", ", tags$code("water_elev_2"),
                ", ", tags$code("water_elev_3"), "). These represent discrete, direct water ",
                "level measurements. If the water surface is surging or choppy due to ",
                "turbulence or wind, the surveyor is required to take several observations ",
                "(preferably at the crest and trough of the waves) and average the results ",
                "for greater accuracy. The closely clustered values in this file (e.g. 0.37, ",
                "0.36, 0.38 metres) are consistent with this practice of capturing multiple ",
                "readings of a fluctuating water surface. These elevations are derived from ",
                "the benchmarks (BM), not from the automated sensor. They represent direct ",
                "water level measurements that a surveyor manually obtains during a site ",
                "visit. To get these values, the surveyor uses the known surveyed elevation ",
                "of a reference point (like BM1) and measures the vertical distance from ",
                "that benchmark to the water surface. Because they are manual, direct ",
                "observations of the water surface, they act as the \"true\" reference water ",
                "level. Once these benchmark-derived water elevations are recorded, they are ",
                "compared against the automated stage sensor's logged values at that exact ",
                "time to check the sensor's performance and determine if a Sensor Reset ",
                "Correction (SRC) needs to be applied to the sensor data."
              )
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              downloadLink(ns("download_sample_bm"),
                label = tagList(icon("download"), " Download sample benchmark CSV"),
                style = "color: #1565c0;"
              )
            )
          )
        ),

        # ---- Right: Upload + Table (67%) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px;",
          fileInput(ns("benchmark_upload"),
            "Upload Custom Benchmark CSV (optional \u2014 demo data pre-loaded)",
            accept = ".csv", width = "100%"),
          uiOutput(ns("benchmark_validation")),
          DT::DTOutput(ns("benchmark_table")),
          tags$h4("Survey Elevations", style = "margin-top: 20px;"),
          tags$img(
            src = "img/elevation_survey.png",
            style = "max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 15px;",
            alt = "Elevation survey diagram"
          ),
          plotly::plotlyOutput(ns("benchmark_dotplot"), height = "400px")
        )
      ),

      hr(),

      # ================================================================
      # Section 2: Apply Datum
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Apply Datum"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Parameter explanation (33%) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px; flex: 1;",
            tags$h4("Parameters", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              tags$code("max_anchor_gap_h"), " \u2014 Maximum hours between the anchor ",
              "visit time and the nearest logger reading. Increase for loggers with ",
              "coarse time steps (e.g. 2-hour intervals). Default: 1 hour."
            ),
            tags$p(style = "font-size: 13px;",
              tags$code("bm_tolerance_m"), " \u2014 Threshold (metres) for benchmark ",
              "consistency across visits. A warning is issued if a benchmark's inferred ",
              "elevation differs by more than this amount. Default: 0.02 m."
            ),
            tags$p(style = "font-size: 13px;",
              tags$code("datum_elev_m"), " \u2014 The arbitrary elevation (metres) assigned ",
              "to the first valid visit's primary benchmark. Default: 100 m. Adjust if you ",
              "want a different datum origin (e.g. a known geodetic elevation)."
            ),
            tags$h4("Follow Along in the R Package", style = "margin-top: 15px;"),
            tags$code("wl_set_datum()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
              "Anchors first valid visit at 100.0 m, computes constant offset, ",
              "creates wl_elev_m column"
            )
          )
        ),

        # ---- Right: Station selector & controls (67%) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px;",
          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0;",
            tags$div(
              style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px; border: 2px solid #2e7d32; margin-bottom: 15px;",
              selectInput(ns("station_select"), "Select Station:",
                choices = NULL, width = "100%")
            ),
            fluidRow(
              column(4,
                numericInput(ns("max_anchor_gap_h"), "Max anchor gap (hours):",
                  value = 1, min = 0.1, max = 24, step = 0.5, width = "100%")
              ),
              column(4,
                numericInput(ns("bm_tolerance_m"), "BM tolerance (metres):",
                  value = 0.02, min = 0.001, max = 0.5, step = 0.005, width = "100%")
              ),
              column(4,
                numericInput(ns("datum_elev_m"), "Datum elevation (m):",
                  value = 100, min = 0, max = 10000, step = 1, width = "100%")
              )
            ),
            actionButton(ns("run_datum"), "Apply Datum",
              class = "btn-success btn-lg", width = "100%",
              icon = icon("play"),
              style = "color: #ffffff;"
            )
          )
        )
      ),

      hr(),

      # ================================================================
      # Section 3: Results (conditional)
      # ================================================================
      conditionalPanel(
        condition = sprintf("output['%s']", ns("show_results")),

        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Results"
          )
        ),

        # Plotly time series
        tags$h4("Stage vs Datum-Adjusted Elevation"),
        plotly::plotlyOutput(ns("datum_plot"), height = "450px"),

        hr(),

        # Summary
        tags$h4("Datum Summary"),
        verbatimTextOutput(ns("datum_summary")),

        hr(),

        # Benchmark network table
        tags$h4("Benchmark Network for Selected Station"),
        DT::DTOutput(ns("bm_network_table")),

        hr(),

        # ================================================================
        # Section 4: Export
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Export"
          )
        ),

        tags$div(
          style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
          # ---- Left: Export info (33%) ----
          tags$div(
            style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px; flex: 1;",
              tags$h4("Exported Columns", style = "margin-top: 0;"),
              tags$ul(
                style = "padding-left: 20px; font-size: 13px;",
                tags$li(tags$code("wl_elev_m"), " \u2014 water level on datum (metres)"),
                tags$li(tags$code("flag_wl_datum_applied"), " \u2014 datum was applied"),
                tags$li("All existing stage and flag columns are preserved")
              ),
              tags$p(style = "font-size: 13px; color: #666;",
                "The QAQC log records the datum offset, anchor visit, and benchmark used."
              )
            )
          ),
          # ---- Right: Download buttons (67%) ----
          tags$div(
            style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px;",
            tags$div(
              style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0;",
              downloadButton(ns("download_datum_csv"), "Download Datum-Adjusted CSV",
                class = "btn-primary", style = "width: 100%; margin-bottom: 10px; color: #ffffff;"),
              downloadButton(ns("download_datum_log"), "Download QAQC Log CSV",
                class = "btn-default", style = "width: 100%;")
            )
          )
        )
      ),

      .wireframe_footer()
    )
  )
}


module_wl_datum_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data    <- session$userData$rv_raw_data
    rv_qaqc_data   <- session$userData$rv_qaqc_data
    rv_clean_data  <- session$userData$rv_clean_data

    # Module-local state
    active_benchmarks <- reactiveVal(NULL)
    datum_result      <- reactiveVal(NULL)
    datum_log         <- reactiveVal(NULL)
    datum_warnings    <- reactiveVal(character(0))

    # Conditional panel visibility
    output$show_results <- reactive({ !is.null(datum_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    # ------ Initialize benchmarks from demo data ------
    observe({
      bm <- rv_raw_data$benchmarks
      if (!is.null(bm) && nrow(bm) > 0 && is.null(active_benchmarks())) {
        active_benchmarks(bm)
      }
    })

    # ------ Benchmark upload ------
    observeEvent(input$benchmark_upload, {
      req(input$benchmark_upload)
      tryCatch({
        bm <- read.csv(input$benchmark_upload$datapath, stringsAsFactors = FALSE)
        required_cols <- c("site_station_code", "visit_date", "visit_time",
                           "primary_bm", "grade")
        missing <- setdiff(required_cols, names(bm))
        if (length(missing) > 0) {
          showNotification(
            paste("Missing required columns:", paste(missing, collapse = ", ")),
            type = "error", duration = 8)
          return()
        }
        # Check for at least one BM/TBM and one water_elev column
        has_bm <- any(grepl("^(BM|TBM)", names(bm)))
        has_we <- any(grepl("^water_elev_", names(bm)))
        if (!has_bm || !has_we) {
          showNotification(
            "File must contain at least one BM*/TBM* column and one water_elev_* column",
            type = "error", duration = 8)
          return()
        }
        active_benchmarks(bm)
        showNotification("Benchmark data uploaded successfully", type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Error reading file:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Data readiness status ------
    output$datum_status <- renderUI({
      qc_data <- rv_qaqc_data$qc_waterlevel
      bm      <- active_benchmarks()

      has_qc <- !is.null(qc_data) && nrow(qc_data) > 0
      has_bm <- !is.null(bm) && nrow(bm) > 0

      if (has_qc && has_bm) {
        n_stns <- length(unique(qc_data$site_station_code))
        n_bm_stns <- length(unique(bm$site_station_code))
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Ready: %d QC'd station(s), %d station(s) in benchmark data",
                  n_stns, n_bm_stns)
        )
      } else {
        msgs <- character(0)
        if (!has_qc) msgs <- c(msgs, "No QC'd water level data \u2014 complete QC Water Level first")
        if (!has_bm) msgs <- c(msgs, "No benchmark data loaded")
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          paste(" ", paste(msgs, collapse = ". "), ".")
        )
      }
    })

    # ------ Benchmark validation banner ------
    output$benchmark_validation <- renderUI({
      bm <- active_benchmarks()
      if (is.null(bm) || nrow(bm) == 0) return(NULL)

      required_cols <- c("site_station_code", "visit_date", "visit_time",
                         "primary_bm", "grade")
      missing <- setdiff(required_cols, names(bm))
      has_bm_cols <- any(grepl("^(BM|TBM)", names(bm)))
      has_we_cols <- any(grepl("^water_elev_", names(bm)))
      valid_grades <- all(bm$grade %in% c("Excellent", "Good", "OK", "Poor"))

      if (length(missing) == 0 && has_bm_cols && has_we_cols && valid_grades) {
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Benchmark data valid: %d visits across %d station(s)",
                  nrow(bm), length(unique(bm$site_station_code)))
        )
      } else if (length(missing) > 0) {
        tags$div(
          style = "color: #721c24; background: #f8d7da; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          icon("times-circle"),
          paste(" Missing columns:", paste(missing, collapse = ", "))
        )
      } else {
        warns <- character(0)
        if (!has_bm_cols) warns <- c(warns, "no BM/TBM columns found")
        if (!has_we_cols) warns <- c(warns, "no water_elev_* columns found")
        if (!valid_grades) warns <- c(warns, "invalid grade values (expected: Excellent, Good, OK, Poor)")
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          paste(" Warning:", paste(warns, collapse = "; "))
        )
      }
    })

    # ------ Benchmark table ------
    output$benchmark_table <- DT::renderDT({
      bm <- active_benchmarks()
      req(bm, nrow(bm) > 0)
      DT::datatable(bm, options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    # ------ Benchmark dotplot ------
    output$benchmark_dotplot <- plotly::renderPlotly({
      bm <- active_benchmarks()
      req(bm, nrow(bm) > 0)

      bm_cols <- grep("^(BM|TBM)", names(bm), value = TRUE)
      we_cols <- grep("^water_elev_", names(bm), value = TRUE)

      # Build visit labels sorted chronologically
      visit_dt <- lubridate::parse_date_time(
        paste(bm$visit_date, bm$visit_time),
        orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "dmy HMS", "dmy HM"),
        tz = "UTC"
      )
      bm$visit_label <- paste0("Visit: ", format(visit_dt, "%Y-%m-%d %H:%M"))
      bm$visit_dt_sort <- visit_dt

      # Sort by datetime
      bm <- bm[order(bm$visit_dt_sort), ]
      visit_order <- unique(bm$visit_label)

      # Reshape BM columns to long format
      plot_data <- data.frame(
        visit_label = character(), value = numeric(),
        series = character(), stringsAsFactors = FALSE
      )

      for (i in seq_len(nrow(bm))) {
        vlabel <- bm$visit_label[i]
        # Benchmark points
        for (col in bm_cols) {
          val <- suppressWarnings(as.numeric(bm[[col]][i]))
          if (!is.na(val)) {
            plot_data <- rbind(plot_data, data.frame(
              visit_label = vlabel, value = val,
              series = col, stringsAsFactors = FALSE
            ))
          }
        }
        # Water elevation points
        for (col in we_cols) {
          val <- suppressWarnings(as.numeric(bm[[col]][i]))
          if (!is.na(val)) {
            plot_data <- rbind(plot_data, data.frame(
              visit_label = vlabel, value = val,
              series = col, stringsAsFactors = FALSE
            ))
          }
        }
      }

      req(nrow(plot_data) > 0)

      # Set visit_label as ordered factor for categorical x-axis
      plot_data$visit_label <- factor(plot_data$visit_label, levels = visit_order)

      # Assign colours: red, purple, green for BM1-3; cycle for extras
      bm_palette <- c("#d32f2f", "#7b1fa2", "#2e7d32", "#e65100", "#00838f",
                       "#4e342e", "#546e7a")
      we_colour <- "#1565c0"

      all_bm_series <- intersect(bm_cols, unique(plot_data$series))
      all_we_series <- intersect(we_cols, unique(plot_data$series))

      colour_map <- character()
      for (k in seq_along(all_bm_series)) {
        colour_map[all_bm_series[k]] <- bm_palette[((k - 1) %% length(bm_palette)) + 1]
      }
      for (k in seq_along(all_we_series)) {
        colour_map[all_we_series[k]] <- we_colour
      }

      p <- plotly::plot_ly()

      # Gauge Datum line at y = 0
      p <- p %>% plotly::add_lines(
        x = visit_order, y = rep(0, length(visit_order)),
        name = "Gauge Datum (y=0)",
        line = list(color = "#999999", dash = "dash", width = 1.5),
        hoverinfo = "name"
      )

      # Add lines + points for each series
      for (s in c(all_bm_series, all_we_series)) {
        sdata <- plot_data[plot_data$series == s, ]
        if (nrow(sdata) == 0) next

        is_we <- s %in% we_cols

        # Line connecting points across visits
        p <- p %>% plotly::add_lines(
          data = sdata,
          x = ~visit_label, y = ~value,
          name = s,
          line = list(color = colour_map[s], width = 1.5),
          text = ~paste0(series, ": ", value, " m"),
          hoverinfo = "text",
          showlegend = FALSE
        )

        # Points on top
        p <- p %>% plotly::add_markers(
          data = sdata,
          x = ~visit_label, y = ~value,
          name = s,
          marker = list(
            size = if (is_we) 8 else 10,
            color = colour_map[s],
            symbol = if (is_we) "diamond" else "circle"
          ),
          text = ~paste0(series, ": ", value, " m"),
          hoverinfo = "text"
        )
      }

      p %>% plotly::layout(
        xaxis = list(
          title = "",
          type = "category",
          categoryorder = "array",
          categoryarray = visit_order
        ),
        yaxis = list(title = "Elevation (m)"),
        legend = list(orientation = "h", x = 0, y = -0.15),
        margin = list(b = 80)
      )
    })

    # ------ Populate station selector ------
    observe({
      qc_data <- rv_qaqc_data$qc_waterlevel
      bm      <- active_benchmarks()
      req(qc_data, bm)

      qc_stations <- unique(qc_data$site_station_code)
      bm_stations <- unique(bm$site_station_code)
      common <- sort(intersect(qc_stations, bm_stations))

      if (length(common) > 0) {
        updateSelectInput(session, "station_select",
          choices = common, selected = common[1])
      } else {
        updateSelectInput(session, "station_select",
          choices = list("No matching stations" = ""),
          selected = "")
      }
    })

    # ------ Run datum ------
    observeEvent(input$run_datum, {
      station <- input$station_select
      req(station, station != "")
      qc_data <- rv_qaqc_data$qc_waterlevel
      bm <- active_benchmarks()
      req(qc_data, bm)

      tryCatch({
        showNotification("Applying datum adjustment...",
          type = "message", duration = NULL, id = "datum_progress")

        # Capture warnings
        caught_warnings <- character(0)
        result <- withCallingHandlers(
          wl_set_datum(
            input_data       = qc_data,
            select_station   = station,
            level_runs       = bm,
            log_root         = NULL,
            user             = NULL,
            max_anchor_gap_h = input$max_anchor_gap_h,
            bm_tolerance_m   = input$bm_tolerance_m,
            datum_elev_m     = input$datum_elev_m
          ),
          warning = function(w) {
            caught_warnings <<- c(caught_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )

        datum_warnings(caught_warnings)

        # Extract QAQC log
        log_attr <- attr(result, "qaqc_log")
        datum_log(log_attr)

        # Store results
        datum_result(result)
        rv_clean_data$waterlevel_datum <- result

        removeNotification("datum_progress")
        msg <- sprintf("Datum applied for %s", station)
        if (length(caught_warnings) > 0) {
          msg <- paste0(msg, " (with ", length(caught_warnings), " warning(s))")
        }
        showNotification(msg, type = "message", duration = 5)

      }, error = function(e) {
        removeNotification("datum_progress")
        showNotification(paste("Datum error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Selected station data ------
    selected_data <- reactive({
      req(input$station_select, datum_result())
      dplyr::filter(datum_result(), site_station_code == input$station_select)
    })

    # ------ Plotly dual-axis time series ------
    output$datum_plot <- plotly::renderPlotly({
      df <- selected_data()
      req(nrow(df) > 0, "wl_elev_m" %in% names(df))

      has_adj <- "waterlevel_m_adj" %in% names(df)
      stage_col <- if (has_adj) "waterlevel_m_adj" else "waterlevel_m"

      p <- plotly::plot_ly(df, x = ~timestamp) %>%
        plotly::add_lines(y = as.formula(paste0("~", stage_col)),
          name = "Stage (m)", line = list(color = "#90caf9"),
          yaxis = "y") %>%
        plotly::add_lines(y = ~wl_elev_m,
          name = "Elevation (m)", line = list(color = "#1565c0"),
          yaxis = "y2")

      # Add benchmark visit water elevations on the Elevation axis
      bm <- active_benchmarks()
      station <- input$station_select
      datum_elev <- input$datum_elev_m
      if (!is.null(bm) && !is.null(station) && nrow(bm) > 0) {
        stn_bm <- bm[bm$site_station_code == station, , drop = FALSE]
        if (nrow(stn_bm) > 0) {
          bm_cols_plot <- grep("^(BM|TBM)", names(stn_bm), value = TRUE)
          we_cols_plot <- grep("^water_elev_", names(stn_bm), value = TRUE)

          # Parse visit datetimes
          visit_dt <- lubridate::parse_date_time(
            paste(stn_bm$visit_date, stn_bm$visit_time),
            orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "dmy HMS", "dmy HM"),
            tz = "UTC"
          )

          # Rebuild benchmark network to get absolute elevations
          # (mirrors wl_set_datum logic)
          stn_bm <- stn_bm[order(visit_dt), ]
          visit_dt <- sort(visit_dt)

          # Coerce numeric
          for (nm in c(bm_cols_plot, we_cols_plot)) {
            stn_bm[[nm]] <- suppressWarnings(as.numeric(stn_bm[[nm]]))
          }
          stn_bm$primary_bm <- trimws(stn_bm$primary_bm)

          bm_abs_map <- list()
          first_valid <- NA
          for (i in seq_len(nrow(stn_bm))) {
            pbm_col <- stn_bm$primary_bm[i]
            pbm_raw <- stn_bm[[pbm_col]][i]
            we_vals <- unlist(stn_bm[i, we_cols_plot, drop = FALSE])
            if (!is.na(pbm_raw) && any(!is.na(we_vals))) {
              first_valid <- i
              break
            }
          }

          if (!is.na(first_valid)) {
            anchor_pbm_col <- stn_bm$primary_bm[first_valid]
            anchor_pbm_raw <- stn_bm[[anchor_pbm_col]][first_valid]
            bm_abs_map[[anchor_pbm_col]] <- datum_elev

            for (i in seq_len(nrow(stn_bm))) {
              pbm_col <- stn_bm$primary_bm[i]
              pbm_raw <- stn_bm[[pbm_col]][i]
              if (is.na(pbm_raw)) next

              pbm_abs <- bm_abs_map[[pbm_col]]
              if (is.null(pbm_abs)) {
                # Infer from known co-observed BM
                for (bc in bm_cols_plot) {
                  known <- bm_abs_map[[bc]]
                  bc_raw <- stn_bm[[bc]][i]
                  if (!is.null(known) && !is.na(bc_raw)) {
                    pbm_abs <- known - (bc_raw - pbm_raw)
                    break
                  }
                }
                if (!is.null(pbm_abs)) bm_abs_map[[pbm_col]] <- pbm_abs
              }
              if (is.null(pbm_abs)) next

              # Update all BMs on this visit
              for (bc in bm_cols_plot) {
                bc_raw <- stn_bm[[bc]][i]
                if (!is.na(bc_raw) && is.null(bm_abs_map[[bc]])) {
                  bm_abs_map[[bc]] <- pbm_abs + (bc_raw - pbm_raw)
                }
              }
            }

            # Build visit point data for water elevations and datum height
            visit_points <- data.frame(
              timestamp = as.POSIXct(character()), elev = numeric(),
              label = character(), stringsAsFactors = FALSE
            )

            for (i in seq_len(nrow(stn_bm))) {
              pbm_col <- stn_bm$primary_bm[i]
              pbm_raw <- stn_bm[[pbm_col]][i]
              pbm_abs <- bm_abs_map[[pbm_col]]
              if (is.na(pbm_raw) || is.null(pbm_abs)) next

              vdt <- visit_dt[i]

              # Water elevation points
              for (wc in we_cols_plot) {
                we_raw <- stn_bm[[wc]][i]
                if (!is.na(we_raw)) {
                  we_abs <- pbm_abs + (we_raw - pbm_raw)
                  visit_points <- rbind(visit_points, data.frame(
                    timestamp = vdt, elev = we_abs,
                    label = paste0(wc, ": ", round(we_abs, 3), " m"),
                    stringsAsFactors = FALSE
                  ))
                }
              }

              # Datum height (PBM absolute elevation)
              visit_points <- rbind(visit_points, data.frame(
                timestamp = vdt, elev = pbm_abs,
                label = paste0("Datum (", pbm_col, "): ", round(pbm_abs, 3), " m"),
                stringsAsFactors = FALSE
              ))
            }

            if (nrow(visit_points) > 0) {
              # Separate water elev points from datum points
              is_datum <- grepl("^Datum", visit_points$label)

              we_pts <- visit_points[!is_datum, ]
              datum_pts <- visit_points[is_datum, ]

              if (nrow(we_pts) > 0) {
                p <- p %>% plotly::add_markers(
                  data = we_pts,
                  x = ~timestamp, y = ~elev,
                  name = "Visit Water Elev.",
                  marker = list(size = 10, color = "#43a047",
                                symbol = "diamond", line = list(width = 1, color = "#1b5e20")),
                  text = ~label, hoverinfo = "text",
                  yaxis = "y2"
                )
              }

              if (nrow(datum_pts) > 0) {
                p <- p %>% plotly::add_markers(
                  data = datum_pts,
                  x = ~timestamp, y = ~elev,
                  name = "Datum (PBM)",
                  marker = list(size = 10, color = "#d32f2f",
                                symbol = "triangle-up", line = list(width = 1, color = "#b71c1c")),
                  text = ~label, hoverinfo = "text",
                  yaxis = "y2"
                )
              }
            }
          }
        }
      }

      p <- p %>% plotly::layout(
          xaxis = list(title = "Timestamp"),
          yaxis = list(title = "Stage (m)", side = "left"),
          yaxis2 = list(title = "Elevation (m)", overlaying = "y",
                        side = "right"),
          legend = list(orientation = "h", x = 0, y = 1.12),
          margin = list(r = 80)
        )

      p
    })

    # ------ Datum summary ------
    output$datum_summary <- renderPrint({
      df <- selected_data()
      log <- datum_log()
      warns <- datum_warnings()
      req(df, nrow(df) > 0)

      station <- input$station_select
      elev <- df$wl_elev_m[!is.na(df$wl_elev_m)]

      cat("Station:", station, "\n")

      # Extract offset from log if available
      if (!is.null(log) && nrow(log) > 0) {
        stn_log <- log[log$station == station, ]
        if (nrow(stn_log) > 0) {
          cat("QAQC Log Entry:\n")
          for (i in seq_len(nrow(stn_log))) {
            cat("  Action:", stn_log$action[i], "\n")
            if ("action_note" %in% names(stn_log)) {
              cat("  Note:", stn_log$action_note[i], "\n")
            } else if ("note" %in% names(stn_log)) {
              cat("  Note:", stn_log$note[i], "\n")
            }
          }
        }
      }

      if (length(elev) > 0) {
        cat("\nElevation range:", round(min(elev), 4), "to",
            round(max(elev), 4), "m\n")
        cat("Elevation mean:", round(mean(elev), 4), "m\n")
        cat("Records with datum:", sum(!is.na(df$wl_elev_m)), "of", nrow(df), "\n")
      }

      if (length(warns) > 0) {
        cat("\nWarnings:\n")
        for (w in warns) cat("  -", w, "\n")
      }
    })

    # ------ Benchmark network table for selected station ------
    output$bm_network_table <- DT::renderDT({
      bm <- active_benchmarks()
      station <- input$station_select
      req(bm, station)

      stn_bm <- bm[bm$site_station_code == station, ]
      req(nrow(stn_bm) > 0)

      DT::datatable(stn_bm, options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    # ------ Download: Sample benchmark CSV ------
    output$download_sample_bm <- downloadHandler(
      filename = function() { "sample_benchmarks.csv" },
      content = function(file) {
        bm <- rv_raw_data$benchmarks
        if (!is.null(bm) && nrow(bm) > 0) {
          write.csv(bm, file, row.names = FALSE)
        } else {
          # Minimal example
          example <- data.frame(
            site_station_code = "ALBR_ST_30",
            visit_date = "4/18/2025",
            visit_time = "13:30",
            primary_bm = "BM1",
            grade = "Excellent",
            BM1 = 0.12, BM2 = 0.19, BM3 = 0.08,
            water_elev_1 = 0.37, water_elev_2 = 0.36, water_elev_3 = 0.38,
            stringsAsFactors = FALSE
          )
          write.csv(example, file, row.names = FALSE)
        }
      }
    )

    # ------ Download: Datum-adjusted CSV ------
    output$download_datum_csv <- downloadHandler(
      filename = function() {
        station <- input$station_select
        paste0("wl_datum_", station, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- selected_data()
        req(nrow(df) > 0)

        export_cols <- c(
          "site_station_code", "timestamp",
          "waterlevel_m", "waterlevel_m_adj", "wl_elev_m",
          "watertemp_C", "watertemp_C_adj",
          "waterpress_kPa", "airpress_kPa", "airtemp_C"
        )
        flag_cols <- grep("^(edit_|flag_|neg_)", names(df), value = TRUE)
        export_cols <- intersect(c(export_cols, flag_cols), names(df))

        export_df <- df[, export_cols]
        export_df$timestamp <- format(export_df$timestamp, "%Y-%m-%d %H:%M:%S")
        write.csv(export_df, file, row.names = FALSE)
      }
    )

    # ------ Download: QAQC Log ------
    output$download_datum_log <- downloadHandler(
      filename = function() {
        station <- input$station_select
        paste0("wl_datum_log_", station, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        log <- datum_log()
        if (!is.null(log) && nrow(log) > 0) {
          write.csv(log, file, row.names = FALSE)
        } else {
          write.csv(
            data.frame(station = character(), metric = character(),
                       action = character(), note = character(),
                       stringsAsFactors = FALSE),
            file, row.names = FALSE
          )
        }
      }
    )

  })
}
