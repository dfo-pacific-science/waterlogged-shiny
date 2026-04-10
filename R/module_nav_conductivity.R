# =============================================================================
# Conductivity Workflow Modules
# =============================================================================
#
# Pipeline for HOBO U24 conductivity loggers:
#   1. Import Data  — bind_hobo_files() for conductivity loggers
#   2. Convert & QC — add_nearest_baro(), convert_conductivity_spc_uScm(),
#                     qc_conductivity()
#
# Note: Conductivity does not require barometric pressure compensation for
# its primary measurement, but baro data provides air temperature which
# helps with dry-sensor detection and is needed for some temp compensation
# workflows.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Import Data
# -----------------------------------------------------------------------------
module_cond_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # CSS keyframes for processing spinner animation
    tags$style(HTML("
      @keyframes cond-spin {
        0%   { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    ")),
    shinydashboard::box(
      width = 12,
      tags$h1("Import Conductivity Logger Data", class = "custom-h1"),
      tags$h2("Upload raw HOBO U24 conductivity logger files and align timestamps",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Import raw HOBO CSV files from conductivity loggers ",
        "(HOBO U24). These loggers record electrical conductivity (\u00b5S/cm) and water ",
        "temperature in \u00b0C. When both Low Range and Full Range channels are present, ",
        "the appropriate range is automatically selected (Low Range if valid and ",
        "\u2264 1000 \u00b5S/cm, otherwise Full Range)."
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(style = "margin: 0;",
          icon("info-circle"),
          " Barometric data is ", tags$strong("optional"), " for conductivity. ",
          "It provides air temperature for dry-sensor detection in the QC step, ",
          "but is not needed for the primary conductivity measurement. You may ",
          "add baro data later in ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_cond_convert']\").tab('show'); return false;",
            style = "color: #0d47a1; font-weight: bold;",
            "Convert to Spec. Cond."
          ), "."
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
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
              tags$li(tags$code("model"), " \u2014 logger model (e.g. HOBO_U24)"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 ",
                      tags$strong("must"), " be ", tags$code("\"conductivity\""),
                      " \u2014 this determines how the logger data is classified and processed"),
              tags$li(tags$code("status"), " \u2014 deployment status"),
              tags$li(tags$code("latitude"), " / ", tags$code("longitude"),
                      " \u2014 decimal degrees",
                      " (e.g. ", tags$code("49.2827"), ", ", tags$code("-123.1207"), ")")
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload your own metadata CSV, or use the pre-loaded sample. ",
              "The file must contain one row per deployment period per logger. ",
              "Include metadata for barometric stations as well if you plan to ",
              "use them later for dry-sensor detection."
            ),
            tags$p(style = "font-size: 12px; margin-top: 10px;",
              tags$a(
                href = "sample_metadata.csv",
                download = "sample_metadata.csv",
                icon("download"),
                " Download sample metadata (pre-loaded demo)",
                style = "color: #2e7d32;"
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
      # Row 2: Raw Conductivity Logger Data
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Raw Conductivity Logger Data"
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
              "conductivity loggers. You can select multiple files at once. ",
              "The CSV must contain a ", tags$code("Date Time"), " column in ",
              "HOBOware's default export format: ",
              tags$code("MM/DD/YYYY HH:MM:SS AM/PM"),
              " (e.g. ", tags$code("06/01/2025 02:30:00 PM"), ")."
            ),
            tags$ol(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Ensure metadata is loaded (above) so logger serial ",
                      "numbers can be matched to stations."),
              tags$li("Select one or more raw HOBO U24 CSV files."),
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
                href = "sample_conductivity_raw.csv",
                download = "sample_conductivity_raw.csv",
                icon("download"),
                " Download sample conductivity logger CSV (pre-loaded demo)",
                style = "color: #2e7d32;"
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
                   "Parse raw HOBO CSVs, auto-select conductivity range, match serial numbers to metadata")
          ),

          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Columns Produced", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px; margin-bottom: 0;",
              tags$li(tags$code("site_station_code"), " \u2014 station ID",
                      " (e.g. ", tags$code("TUMT_WE_40"), ")"),
              tags$li(tags$code("timestamp"), " \u2014 date-time in ",
                      tags$code("YYYY-MM-DD HH:MM:SS"), " format",
                      " (e.g. ", tags$code("2025-06-01 14:30:00"), ")"),
              tags$li(tags$code("conduct_uScm"), " \u2014 conductivity in \u00b5S/cm",
                      " (e.g. ", tags$code("245.8"), ")"),
              tags$li(tags$code("conduct_source"), " \u2014 range used: ",
                      tags$code("\"low\""), " or ", tags$code("\"full\"")),
              tags$li(tags$code("watertemp_C"), " \u2014 water temperature in \u00b0C",
                      " (e.g. ", tags$code("11.9"), ")"),
              tags$li(tags$code("sn"), " \u2014 logger serial number",
                      " (e.g. ", tags$code("21245027"), ")"),
              tags$li(tags$code("metric"), " \u2014 ", tags$code("\"conductivity\""))
            )
          )
        ),

        # ---- Right: File upload (narrow) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",

          # Raw HOBO files upload
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0; flex: 1;",
            tags$h4("Raw Conductivity Logger Files", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload one or more raw HOBO U24 CSV files, or use the ",
              "pre-loaded sample data."
            ),
            fileInput(ns("raw_files"), NULL,
              accept = ".csv",
              multiple = TRUE,
              placeholder = "Choose HOBO U24 CSV files...",
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
              style = "display: none; text-align: center; padding: 15px; margin-top: 10px; background-color: #e8f5e9; border-radius: 5px;",
              tags$div(
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #a5d6a7; border-top: 3px solid #2e7d32; border-radius: 50%; animation: cond-spin 0.8s linear infinite; vertical-align: middle;"
              ),
              tags$span(style = "margin-left: 10px; color: #2e7d32; font-weight: bold;",
                "Processing... binding HOBO files")
            ),
            uiOutput(ns("bind_summary"))
          )
        )
      ),

      # ================================================================
      # Row 3: Processed Barometric Data (Optional)
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Processed Barometric Data (Optional)"
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
              "Adding processed barometric data is ", tags$strong("optional"),
              " for conductivity. It provides air temperature which improves ",
              "dry-sensor detection in the QC step (flagging periods where the ",
              "air-water temperature difference is small)."
            ),
            tags$p(
              "This file is the output of the Barometric Pressure QAQC workflow. ",
              "You must run through the ",
              tags$a(
                href = "#",
                onclick = "$(\"a[href='#shiny-tab-t_baro_upload']\").tab('show'); return false;",
                style = "color: #2e7d32; font-weight: bold;",
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
                style = "color: #2e7d32;",
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
              style = "display: none; text-align: center; padding: 15px; margin-top: 10px; background-color: #e8f5e9; border-radius: 5px;",
              tags$div(
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #a5d6a7; border-top: 3px solid #2e7d32; border-radius: 50%; animation: cond-spin 0.8s linear infinite; vertical-align: middle;"
              ),
              tags$span(style = "margin-left: 10px; color: #2e7d32; font-weight: bold;",
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
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
              "Round timestamps to a regular interval so conductivity stations ",
              "align. Original timestamps are preserved in ",
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
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Review Raw Series"
        )
      ),

      tags$p(
        class = "clean-print",
        "Visually inspect the time series plots and summary statistics below ",
        "to look for anomalies such as spikes, flatlines, or gaps in the data. ",
        "The top panels show conductivity and water temperature. If processed ",
        "barometric data has been loaded, air pressure and air temperature panels ",
        "are also shown."
      ),

      # ---- Import Summary (full width) ----
      tags$h4("Import Summary"),
      DT::DTOutput(ns("import_summary")),

      tags$div(style = "margin-top: 15px;"),

      # ---- Preview Plot (full width, 2-4 panels depending on baro) ----
      tags$h4("Data Preview"),
      uiOutput(ns("preview_plot_ui")),

      # ---- (Optional) Filter Data by Date Range ----
      tags$div(
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
                       style = "color: #fff; background-color: #2e7d32; border-color: #2e7d32;")
        ))
      ),

      uiOutput(ns("filter_status")),

      # ---- Next step link ----
      tags$div(
        style = "margin-top: 25px; padding: 15px; background-color: #e8f5e9; border-radius: 5px; text-align: center;",
        tags$p(style = "margin: 0; font-size: 14px;",
          icon("arrow-right"),
          " Ready to proceed? Continue to ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_cond_convert']\").tab('show'); window.scrollTo(0,0); return false;",
            style = "color: #2e7d32; font-weight: bold;",
            "Convert to Spec. Cond."
          ),
          " to optionally add barometric data, convert to specific conductance, and run QAQC."
        )
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 1. Import Data — Server
# =============================================================================
module_cond_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data   <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data
    rv_qaqc_data  <- session$userData$rv_qaqc_data

    # ------ Initialize from demo data ------
    observe({
      data_list <- rv_raw_data$conductivity_data
      if (!is.null(data_list) && length(data_list) > 0 &&
          is.null(rv_bound_data$conductivity_bound)) {
        combined <- dplyr::bind_rows(data_list)
        if ("timestamp" %in% names(combined) && is.character(combined$timestamp)) {
          combined$timestamp <- .parse_timestamp(combined$timestamp)
        }
        rv_bound_data$conductivity_bound <- combined
      }
    })

    # ------ Convenience reactives ------
    cond_current <- reactive({
      rv_bound_data$conductivity_bound
    })

    baro_qc_current <- reactive({
      rv_qaqc_data$baro_qaqc
    })

    # ------ Data status display ------
    output$data_status <- renderUI({
      cond_df <- cond_current()

      if (is.null(cond_df) || nrow(cond_df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 8px;",
          icon("info-circle"),
          " No conductivity data loaded. Upload files below or use pre-loaded sample data."
        )
      } else {
        n_stations <- length(unique(cond_df$site_station_code))
        n_records <- format(nrow(cond_df), big.mark = ",")
        date_min <- format(min(cond_df$timestamp, na.rm = TRUE), "%Y-%m-%d")
        date_max <- format(max(cond_df$timestamp, na.rm = TRUE), "%Y-%m-%d")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 8px;",
          icon("check-circle"),
          sprintf(" Conductivity data loaded: %d station(s), %s records (%s to %s)",
                  n_stations, n_records, date_min, date_max)
        )
      }
    })

    # ------ Station map (conductivity + optionally baro, colored by metric) ------
    output$station_map <- renderLeaflet({
      metadata <- rv_raw_data$metadata
      req(metadata)

      # Filter to conductivity + barometric stations
      cond_baro_meta <- metadata[metadata$metric %in% c("conductivity", "barometric"), ]
      sites <- unique(cond_baro_meta[, c("site_station_code", "latitude", "longitude", "metric")])
      sites <- sites[!is.na(sites$latitude) & !is.na(sites$longitude), ]

      req(nrow(sites) > 0)

      # Split by metric for toggleable overlay groups
      cond_sites <- sites[sites$metric == "conductivity", ]
      baro_sites <- sites[sites$metric == "barometric", ]

      map <- leaflet(sites) %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo")

      if (nrow(cond_sites) > 0) {
        map <- map %>% addCircleMarkers(
          data = cond_sites,
          lng = ~longitude, lat = ~latitude,
          radius = 7, weight = 2,
          color = "#fff", fillColor = "#2e7d32", fillOpacity = 0.9,
          label = ~site_station_code,
          popup = ~paste0(
            "<strong>", site_station_code, "</strong><br>",
            "Metric: ", metric, "<br>",
            "Lat: ", round(latitude, 4), "<br>",
            "Lon: ", round(longitude, 4)
          ),
          group = "Conductivity",
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
          color = "#fff", fillColor = "#66bb6a", fillOpacity = 0.9,
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
          colors = c("#2e7d32", "#66bb6a"),
          labels = c("Conductivity", "Barometric"),
          title = "Metric"
        ) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Topo"),
          overlayGroups = c("Conductivity", "Barometric"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # ------ Station metadata table (both metrics) ------
    output$station_table <- DT::renderDT({
      metadata <- rv_raw_data$metadata
      req(metadata)

      cond_baro_meta <- metadata[metadata$metric %in% c("conductivity", "barometric"), ]
      req(nrow(cond_baro_meta) > 0)

      display_cols <- intersect(
        c("site_station_code", "timestamp_deploy", "timestamp_remove",
          "model", "sn", "metric", "status", "latitude", "longitude"),
        names(cond_baro_meta)
      )

      DT::datatable(cond_baro_meta[, display_cols],
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
      n_rows <- sum(metadata$metric %in% c("conductivity", "barometric"))
      height_px <- max(200, n_rows * 50 + 80)
      plotlyOutput(session$ns("deployment_timeline"),
        height = paste0(height_px, "px")) %>% withSpinner()
    })

    # ------ Deployment timeline (Gantt chart, colored by SN) ------
    output$deployment_timeline <- renderPlotly({
      metadata <- rv_raw_data$metadata
      req(metadata)

      cond_baro_meta <- metadata[metadata$metric %in% c("conductivity", "barometric"), ]
      req(nrow(cond_baro_meta) > 0)

      # Parse deploy/remove timestamps
      cond_baro_meta$deploy <- as.POSIXct(cond_baro_meta$timestamp_deploy,
        format = "%m/%d/%Y %H:%M", tz = "UTC")
      cond_baro_meta$remove <- as.POSIXct(cond_baro_meta$timestamp_remove,
        format = "%m/%d/%Y %H:%M", tz = "UTC")

      # Order by metric then station then deploy time
      cond_baro_meta <- cond_baro_meta[order(cond_baro_meta$metric,
                                              cond_baro_meta$site_station_code,
                                              cond_baro_meta$deploy), ]

      # Create a unique label per row for the y-axis
      cond_baro_meta$row_label <- paste0(
        cond_baro_meta$site_station_code, " (SN: ", cond_baro_meta$sn, ") [",
        seq_len(nrow(cond_baro_meta)), "]"
      )
      # Preserve order as factor
      cond_baro_meta$row_label <- factor(cond_baro_meta$row_label,
        levels = rev(cond_baro_meta$row_label))

      # Colour by unique serial number
      cond_baro_meta$sn_factor <- as.character(cond_baro_meta$sn)

      # Build colour palette
      unique_sns <- unique(cond_baro_meta$sn_factor)
      palette <- RColorBrewer::brewer.pal(max(3, length(unique_sns)), "Set2")
      sn_colors <- setNames(palette[seq_along(unique_sns)], unique_sns)

      # Build plot with one trace per SN (for legend grouping)
      p <- plot_ly()
      legend_shown <- character(0)

      for (i in seq_len(nrow(cond_baro_meta))) {
        row <- cond_baro_meta[i, ]
        show_legend <- !(row$sn_factor %in% legend_shown)
        if (show_legend) legend_shown <- c(legend_shown, row$sn_factor)

        metric_label <- ifelse(row$metric == "conductivity", "Conductivity", "Barometric")

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
                     categoryarray = rev(levels(cond_baro_meta$row_label))),
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

        # Validate: allow conductivity, barometric, and waterlevel
        valid_metrics <- c("conductivity", "barometric", "waterlevel", "dissolvedoxygen")
        invalid <- unique(metadata$metric[!metadata$metric %in% valid_metrics])
        if (length(invalid) > 0) {
          showNotification(
            paste0("Invalid metric values removed: ", paste(invalid, collapse = ", "),
                   ". Allowed values: ", paste(valid_metrics, collapse = ", "), "."),
            type = "warning", duration = 8
          )
          metadata <- metadata[metadata$metric %in% valid_metrics, ]
        }

        # Warn if no conductivity rows found after filtering
        n_cond <- sum(metadata$metric == "conductivity")
        if (n_cond == 0) {
          found_metrics <- unique(metadata$metric)
          showNotification(
            paste0("Warning: No rows with metric 'conductivity' found in metadata. ",
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
          " No processed baro data loaded (optional). Upload a processed baro CSV below, ",
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

    # ------ Update station selector for snap ------
    observe({
      df <- cond_current()
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
          rv_bound_data$conductivity_bound <- bound_data

          # Track bind results for summary display
          bound_stations <- unique(bound_data$site_station_code)
          meta_cond_stations <- unique(
            metadata$site_station_code[metadata$metric %in% c("conductivity")]
          )
          matched <- intersect(bound_stations, meta_cond_stations)
          unmatched_in_data <- setdiff(bound_stations, meta_cond_stations)

          rv_bind_result$success <- TRUE
          rv_bind_result$n_stations <- length(bound_stations)
          rv_bind_result$n_records <- nrow(bound_data)
          rv_bind_result$matched_stations <- matched
          rv_bind_result$unmatched_sns <- unmatched_in_data
          rv_bind_result$metadata_stations <- meta_cond_stations

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
      df <- cond_current()
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
          metric = "COND"
        )

        rv_bound_data$conductivity_bound <- result

        removeNotification("snap_progress")
        showNotification("Timestamps snapped.", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("snap_progress")
        showNotification(paste("Snap error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Import summary table ------
    output$import_summary <- DT::renderDT({
      df <- cond_current()
      req(df)

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `Start Date` = format(min(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
          `End Date` = format(max(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
          `Cond Min (uS/cm)` = round(min(conduct_uScm, na.rm = TRUE), 1),
          `Cond Max (uS/cm)` = round(max(conduct_uScm, na.rm = TRUE), 1),
          `Temp Min (C)` = round(min(watertemp_C, na.rm = TRUE), 1),
          `Temp Max (C)` = round(max(watertemp_C, na.rm = TRUE), 1),
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ------ Preview plot UI (dynamic height) ------
    output$preview_plot_ui <- renderUI({
      baro_df <- baro_qc_current()
      has_baro <- !is.null(baro_df) && nrow(baro_df) > 0
      plot_height <- if (has_baro) "1100px" else "650px"
      plotlyOutput(session$ns("preview_plot"), height = plot_height) %>% withSpinner()
    })

    # ------ Preview plot (2-4 panels: conductivity, water temp, +optional baro) ------
    output$preview_plot <- renderPlotly({
      cond_df <- cond_current()
      req(cond_df, nrow(cond_df) > 0)

      baro_df <- baro_qc_current()
      has_baro <- !is.null(baro_df) && nrow(baro_df) > 0

      cond_stations <- unique(cond_df$site_station_code)

      # Panel 1: Conductivity (uS/cm)
      p1 <- plot_ly()
      for (stn in cond_stations) {
        stn_data <- dplyr::filter(cond_df, site_station_code == stn)
        p1 <- p1 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~conduct_uScm,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- EC"),
          legendgroup = stn,
          hovertemplate = paste0(stn, ": %{y:.1f} \u00b5S/cm<extra></extra>")
        )
      }

      # Panel 2: Water temperature (C)
      p2 <- plot_ly()
      for (stn in cond_stations) {
        stn_data <- dplyr::filter(cond_df, site_station_code == stn)
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

      if (has_baro) {
        baro_stations <- unique(baro_df$site_station_code)

        # Determine which baro column to use (prefer _adj)
        baro_press_col <- if ("airpress_kPa_adj" %in% names(baro_df)) "airpress_kPa_adj" else "airpress_kPa"
        baro_temp_col  <- if ("airtemp_C_adj" %in% names(baro_df)) "airtemp_C_adj" else "airtemp_C"

        # Panel 3: Air pressure (kPa)
        p3 <- plot_ly()
        for (stn in baro_stations) {
          stn_data <- dplyr::filter(baro_df, site_station_code == stn)
          p3 <- p3 %>% add_trace(
            data = stn_data, x = ~timestamp,
            y = stats::as.formula(paste0("~", baro_press_col)),
            type = "scattergl", mode = "lines",
            line = list(width = 1),
            name = paste(stn, "- Air Press"),
            legendgroup = paste0("baro_", stn),
            hovertemplate = paste0(stn, ": %{y:.2f} kPa<extra></extra>")
          )
        }

        # Panel 4: Air temperature (C)
        p4 <- plot_ly()
        for (stn in baro_stations) {
          stn_data <- dplyr::filter(baro_df, site_station_code == stn)
          p4 <- p4 %>% add_trace(
            data = stn_data, x = ~timestamp,
            y = stats::as.formula(paste0("~", baro_temp_col)),
            type = "scattergl", mode = "lines",
            line = list(width = 1),
            name = paste(stn, "- Air Temp"),
            legendgroup = paste0("baro_", stn),
            showlegend = FALSE,
            hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
          )
        }

        subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE, titleY = TRUE) %>%
          layout(
            title = list(text = "Conductivity Data Preview (with Baro)",
                         font = list(size = 14)),
            xaxis4 = list(title = "Date"),
            yaxis  = list(title = "Conductivity (\u00b5S/cm)"),
            yaxis2 = list(title = "Water Temp (\u00b0C)"),
            yaxis3 = list(title = "Air Press (kPa)"),
            yaxis4 = list(title = "Air Temp (\u00b0C)"),
            hovermode = "x unified",
            legend = list(orientation = "h", y = -0.08),
            margin = list(b = 80)
          )
      } else {
        subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
          layout(
            title = list(text = "Conductivity Data Preview",
                         font = list(size = 14)),
            xaxis2 = list(title = "Date"),
            yaxis = list(title = "Conductivity (\u00b5S/cm)"),
            yaxis2 = list(title = "Water Temp (\u00b0C)"),
            hovermode = "x unified",
            legend = list(orientation = "h", y = -0.12),
            margin = list(b = 80)
          )
      }
    })

    # ---- Date Range Filter: auto-populate ----
    observe({
      cond <- rv_bound_data$conductivity_bound
      req(cond, nrow(cond) > 0)
      ts_range <- range(cond$timestamp, na.rm = TRUE)
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

      cond <- rv_bound_data$conductivity_bound
      if (!is.null(cond) && nrow(cond) > 0)
        rv_bound_data$conductivity_bound <- cond[cond$timestamp >= from_ts & cond$timestamp <= to_ts, ]

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
        n <- nrow(rv_bound_data$conductivity_bound)
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
# 2. Convert & QC
# -----------------------------------------------------------------------------
module_cond_convert_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Convert to Specific Conductance", class = "custom-h1"),
      tags$h2("Normalise raw conductivity to a standard reference temperature (25\u00b0C)",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Electrical conductivity varies with water temperature, warmer water ",
        "conducts better. Specific conductance (SPC) normalises readings to 25\u00b0C ",
        "using the EN 27888 standard, enabling comparison across seasons and sites. ",
        "A reading of 100 \u00b5S/cm at 10\u00b0C represents different ionic content ",
        "than 100 \u00b5S/cm at 25\u00b0C. SPC removes this ambiguity."
      ),

      tags$div(
        class = "clean-print",
        tags$h4("What Is Specific Conductance?", style = "margin-top: 10px;"),
        tags$p(
          "Electrical conductivity (EC) measures how well water conducts electricity ",
          "(\u00b5S/cm), but the measurement depends heavily on temperature. Specific ",
          "conductance (SPC) normalises EC to 25\u00b0C, removing temperature ambiguity ",
          "so that readings are comparable across seasons and sites."
        ),
        tags$h4("The ISO 27888 Method", style = "margin-top: 15px;"),
        tags$p(
          "The ISO 27888 temperature compensation table provides correction factors at ",
          "0.1\u00b0C resolution over the 0\u201335\u00b0C range. A 5th-order polynomial ",
          tags$em("f(T)"), " is fitted to these factors, then SPC = ", tags$em("f(T)"),
          " \u00d7 EC. At 25\u00b0C the factor equals 1.0. Below 25\u00b0C the factor ",
          "> 1 (increases the value). Above 25\u00b0C the factor < 1."
        )
      ),

      hr(),

      # ================================================================
      # Part A: Convert to Specific Conductance
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Part A: Convert to Specific Conductance"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Explanation (33%) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex; flex-direction: column;",
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("How It Works", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "Reads ", tags$code("conduct_uScm_adj"), " (preferred) or ",
              tags$code("conduct_uScm"), ", plus ", tags$code("watertemp_C_adj"),
              " or ", tags$code("watertemp_C"), "."
            ),
            tags$p(style = "font-size: 13px;",
              "Loads the ISO 27888 correction table from ",
              tags$code("inst/extdata/tempcomp27888.csv"), " and fits a 5th-order ",
              "polynomial: ", tags$em("f(T) = c0 + c1\u00b7T + c2\u00b7T\u00b2 + c3\u00b7T\u00b3 + c4\u00b7T\u2074 + c5\u00b7T\u2075"), "."
            ),
            tags$p(style = "font-size: 13px;",
              "Computes SPC = ", tags$em("f(T)"), " \u00d7 EC for each row. ",
              "Output column: ", tags$code("spc_uScm_adj"), ". ",
              "Where EC or temperature is NA, SPC is NA. Warns if >5% of ",
              "temperature values are missing."
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; flex: 1;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("convert_conductivity_spc_uScm()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Normalises raw conductivity to a reference temperature (25\u00b0C). ",
                   "Can run before or after QAQC \u2014 prefers ",
                   tags$code("*_adj"), " columns but falls back to raw.")
          )
        ),

        # ---- Right: Controls (67%) ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex; min-width: 0;",
          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1; min-width: 0; overflow-x: auto;",

            uiOutput(ns("convert_readiness")),

            actionButton(ns("run_convert"), "Convert to Specific Conductance",
              class = "btn-primary btn-lg", width = "100%",
              style = "color: #fff; background-color: #2e7d32; border-color: #2e7d32;",
              icon = icon("exchange-alt")
            ),

            uiOutput(ns("convert_result_summary")),

            # Results table (after conversion)
            conditionalPanel(
              condition = "output.show_results",
              ns = ns,
              tags$div(
                style = "margin-top: 20px; overflow-x: auto;",
                tags$h4("Conversion Summary"),
                DT::DTOutput(ns("convert_summary_table"))
              )
            )
          )
        )
      ),

      # ======== Conditional: Plot appears after conversion runs ========
      conditionalPanel(
        condition = "output.show_results",
        ns = ns,

        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "EC vs SPC Comparison"
          )
        ),

        tags$div(
          style = "margin-top: 15px;",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("spc_plot"), height = "650px"),
            type = getOption("spinner.type", 8)
          )
        )
      ),

      hr(),

      # ================================================================
      # Part B: Add Nearest Baro Logger (Optional)
      # ================================================================
      tags$div(
        style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-bottom: 20px; border-left: 4px solid #1565c0;",
        tags$p(style = "margin: 0 0 8px 0; font-weight: bold;", "This step is optional for conductivity."),
        tags$p(style = "margin: 0; font-size: 13px;",
          "Unlike water level and dissolved oxygen, conductivity does not require ",
          "barometric pressure data for its primary conversion. However, adding baro ",
          "data provides air temperature (", tags$code("airtemp_C"), "), which improves ",
          "dry-sensor detection in the QC step. Without baro data, the QC step can only ",
          "detect dry conditions by low conductivity threshold. With baro data, it can ",
          "also check whether air and water temperatures converge (indicating the sensor ",
          "is in air rather than water)."
        ),
        tags$p(style = "margin: 8px 0 0 0; font-size: 13px; color: #555;",
          "If you skip this step, the QC tab will still work \u2014 it just won't have ",
          "the air-water temperature convergence check available."
        )
      ),

      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Part B: Add Nearest Barometric Station (Optional)"
        )
      ),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("How Station Matching Works", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "The function uses ", tags$code("geosphere::distHaversine()"), " to ",
              "rank all barometric stations by horizontal distance (lat/lon) from ",
              "each conductivity site. It keeps the nearest ", tags$code("max_stations"),
              " (default 5) within an optional ", tags$code("max_km"), " radius."
            ),
            tags$p(style = "font-size: 13px;",
              "Barometric data is joined to conductivity data by an exact ",
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
                   "Matches each conductivity station to the nearest barometric station(s) ",
                   "by GPS distance and joins baro pressure/temperature by timestamp. ",
                   "Called with ", tags$code('metric = "conductivity"'), ". Adds ",
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
            selectizeInput(ns("baro_cond_stations"), NULL,
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
              "Leave station selector empty to process all conductivity stations. Select ",
              "specific stations to re-run with different settings \u2014 existing ",
              "results for other stations are preserved."
            ),

            actionButton(ns("run_add_baro"), "Add Baro Data (Optional)",
              class = "btn-primary btn-lg", width = "100%",
              icon = icon("cloud"),
              style = "color: #ffffff;"
            )
          ),

          # ---- Results (conditional) ----
          conditionalPanel(
            condition = "output.show_baro_results",
            ns = ns,

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

      # ---- Next step link ----
      tags$div(
        style = "margin-top: 25px; padding: 15px; background-color: #e8f5e9; border-radius: 5px; text-align: center;",
        tags$p(style = "margin: 0; font-size: 14px;",
          icon("arrow-right"),
          " Ready to proceed? Continue to ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_cond_qc']\").tab('show'); window.scrollTo(0,0); return false;",
            style = "color: #2e7d32; font-weight: bold;",
            "QC Conductivity"
          ),
          " to run automated quality checks and drift correction."
        )
      )
    ),
    .wireframe_footer()
  )
}

module_cond_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data       <- session$userData$rv_raw_data
    rv_bound_data     <- session$userData$rv_bound_data
    rv_qaqc_data      <- session$userData$rv_qaqc_data
    rv_converted_data <- session$userData$rv_converted_data

    # ---- Local reactive state ----
    convert_result <- reactiveVal(NULL)
    baro_result    <- reactiveVal(NULL)
    baro_messages  <- reactiveVal(NULL)

    # ---- Visibility toggles ----
    output$show_results <- reactive({ !is.null(convert_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    output$show_baro_results <- reactive({ !is.null(baro_result()) })
    outputOptions(output, "show_baro_results", suspendWhenHidden = FALSE)

    # =======================================================================
    # Part A: Convert to Specific Conductance — Server Logic
    # =======================================================================

    # ---- Data readiness banner ----
    output$convert_readiness <- renderUI({
      cond <- rv_bound_data$conductivity_bound
      has_data <- !is.null(cond) && nrow(cond) > 0
      if (has_data) {
        n_stations <- length(unique(cond$site_station_code))
        n_records <- format(nrow(cond), big.mark = ",")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
          icon("check-circle"),
          sprintf(" Conductivity data available: %d station(s), %s records.",
                  n_stations, n_records)
        )
      } else {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
          icon("exclamation-triangle"),
          " No conductivity data loaded. Go to Import Data first."
        )
      }
    })

    # ---- Run conversion ----
    observeEvent(input$run_convert, {
      cond <- rv_bound_data$conductivity_bound
      req(cond, nrow(cond) > 0)

      tryCatch({
        showNotification("Converting to specific conductance...",
          type = "message", duration = NULL, id = "spc_progress")

        result <- convert_conductivity_spc_uScm(
          input_data = cond
        )

        convert_result(result)
        rv_bound_data$conductivity_bound <- result
        rv_converted_data$conductivity_converted <- result

        removeNotification("spc_progress")

        n_spc <- if ("spc_uScm_adj" %in% names(result)) {
          sum(!is.na(result$spc_uScm_adj))
        } else 0L

        showNotification(
          sprintf("Conversion complete: %s SPC values computed.",
                  format(n_spc, big.mark = ",")),
          type = "message", duration = 5)

        output$convert_result_summary <- renderUI({
          tags$div(
            style = "margin-top: 15px; padding: 12px; background: #e8f5e9; border-radius: 5px; border-left: 4px solid #2e7d32;",
            icon("check-circle", style = "color: #2e7d32;"),
            sprintf(" Specific conductance computed: %s values. Column ",
                    format(n_spc, big.mark = ",")),
            tags$code("spc_uScm_adj"), " added to the dataset."
          )
        })

      }, error = function(e) {
        removeNotification("spc_progress")
        showNotification(paste("Conversion error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ---- Conversion summary table ----
    output$convert_summary_table <- DT::renderDT({
      req(convert_result())
      result <- convert_result()
      req("spc_uScm_adj" %in% names(result))

      # Prefer _adj columns but fall back to raw
      ec_col <- if ("conduct_uScm_adj" %in% names(result)) "conduct_uScm_adj" else "conduct_uScm"
      temp_col <- if ("watertemp_C_adj" %in% names(result)) "watertemp_C_adj" else "watertemp_C"

      summary_df <- result %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `EC Min (uS/cm)` = round(min(.data[[ec_col]], na.rm = TRUE), 1),
          `EC Max (uS/cm)` = round(max(.data[[ec_col]], na.rm = TRUE), 1),
          `SPC Min (uS/cm)` = round(min(spc_uScm_adj, na.rm = TRUE), 1),
          `SPC Max (uS/cm)` = round(max(spc_uScm_adj, na.rm = TRUE), 1),
          `Temp Min (C)` = round(min(.data[[temp_col]], na.rm = TRUE), 1),
          `Temp Max (C)` = round(max(.data[[temp_col]], na.rm = TRUE), 1),
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE)
    })

    # ---- EC vs SPC comparison plot ----
    output$spc_plot <- plotly::renderPlotly({
      req(convert_result())
      result <- convert_result()
      req("spc_uScm_adj" %in% names(result))

      stations <- unique(result$site_station_code)

      # Prefer _adj columns but fall back to raw
      ec_col <- if ("conduct_uScm_adj" %in% names(result)) "conduct_uScm_adj" else "conduct_uScm"
      temp_col <- if ("watertemp_C_adj" %in% names(result)) "watertemp_C_adj" else "watertemp_C"

      # Panel 1: EC raw
      p1 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(result, site_station_code == stn)
        p1 <- p1 %>% add_trace(
          data = stn_data, x = ~timestamp, y = stats::as.formula(paste0("~", ec_col)),
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- EC"),
          legendgroup = stn,
          hovertemplate = paste0(stn, " EC: %{y:.1f} \u00b5S/cm<extra></extra>")
        )
      }

      # Panel 2: SPC
      p2 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(result, site_station_code == stn)
        p2 <- p2 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~spc_uScm_adj,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- SPC"),
          legendgroup = stn,
          showlegend = FALSE,
          hovertemplate = paste0(stn, " SPC: %{y:.1f} \u00b5S/cm<extra></extra>")
        )
      }

      # Panel 3: Water temperature
      p3 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(result, site_station_code == stn)
        p3 <- p3 %>% add_trace(
          data = stn_data, x = ~timestamp, y = stats::as.formula(paste0("~", temp_col)),
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- Temp"),
          legendgroup = stn,
          showlegend = FALSE,
          hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
        )
      }

      subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "EC vs Specific Conductance", font = list(size = 14)),
          xaxis3 = list(title = "Date"),
          yaxis  = list(title = "EC (\u00b5S/cm)"),
          yaxis2 = list(title = "SPC (\u00b5S/cm)"),
          yaxis3 = list(title = "Water Temp (\u00b0C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.08),
          margin = list(b = 80)
        )
    })

    # =======================================================================
    # Part B: Add Nearest Baro (Optional) — Server Logic
    # =======================================================================

    # ---- Data readiness banner ----
    output$baro_readiness <- renderUI({
      cond_data <- rv_bound_data$conductivity_bound
      baro      <- rv_qaqc_data$baro_qaqc
      if (is.null(cond_data) || nrow(cond_data) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No conductivity data available. Import data in the Import tab first."
        )
      } else if (is.null(baro) || nrow(baro) == 0) {
        n_stns <- length(unique(cond_data$site_station_code))
        n_recs <- format(nrow(cond_data), big.mark = ",")
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("info-circle"),
          sprintf(" Conductivity data ready (%d station(s), %s records) but no QC'd barometric data found. ",
                  n_stns, n_recs),
          "This step is optional \u2014 you can skip to QC Conductivity if you don't need air-water temperature convergence checks."
        )
      } else {
        n_stns <- length(unique(cond_data$site_station_code))
        n_recs <- format(nrow(cond_data), big.mark = ",")
        n_baro <- length(unique(baro$site_station_code))
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Ready: %d conductivity station(s), %s records | %d baro station(s) available",
                  n_stns, n_recs, n_baro)
        )
      }
    })

    # ---- Update conductivity station selector ----
    observe({
      cond_data <- rv_bound_data$conductivity_bound
      if (!is.null(cond_data) && nrow(cond_data) > 0) {
        stns <- sort(unique(cond_data$site_station_code))
        updateSelectizeInput(session, "baro_cond_stations", choices = stns,
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
      cond_data <- convert_result()
      if (is.null(cond_data)) cond_data <- rv_bound_data$conductivity_bound
      baro_data <- rv_qaqc_data$baro_qaqc
      metadata  <- rv_raw_data$metadata

      if (is.null(cond_data) || nrow(cond_data) == 0) {
        showNotification("No conductivity data. Import data first.", type = "error", duration = 5)
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
        showNotification("Adding barometric data to conductivity stations...",
                         type = "message", duration = NULL, id = "cond_baro_progress")

        # Determine station subset
        selected_stns <- input$baro_cond_stations
        run_subset <- length(selected_stns) > 0
        input_data <- if (run_subset) {
          dplyr::filter(cond_data, site_station_code %in% selected_stns)
        } else {
          cond_data
        }

        # Deduplicate baro and conductivity data by (site_station_code, timestamp)
        n_baro_before <- nrow(baro_data)
        baro_data <- baro_data %>%
          dplyr::arrange(site_station_code, timestamp) %>%
          dplyr::filter(!duplicated(paste(site_station_code, timestamp), fromLast = TRUE))
        n_baro_dupes <- n_baro_before - nrow(baro_data)

        n_cond_before <- nrow(input_data)
        input_data <- input_data %>%
          dplyr::arrange(site_station_code, timestamp) %>%
          dplyr::filter(!duplicated(paste(site_station_code, timestamp), fromLast = TRUE))
        n_cond_dupes <- n_cond_before - nrow(input_data)

        if (n_baro_dupes > 0) cat("[add_baro_cond] Removed", n_baro_dupes, "duplicate baro rows\n")
        if (n_cond_dupes > 0) cat("[add_baro_cond] Removed", n_cond_dupes, "duplicate conductivity rows\n")

        cat("[add_baro_cond] Input data:", nrow(input_data), "rows,",
            length(unique(input_data$site_station_code)), "station(s)\n")
        cat("[add_baro_cond] Baro data:", nrow(baro_data), "rows,",
            length(unique(baro_data$site_station_code)), "station(s)\n")
        cat("[add_baro_cond] Metadata:", nrow(metadata), "rows\n")

        # Build baro selection arg
        baro_sel <- if (input$baro_selection == "auto") "auto" else input$baro_selection
        cat("[add_baro_cond] baro_selection:", baro_sel, "\n")

        # Build max_km (NA -> NULL)
        max_km_val <- input$max_km
        if (is.na(max_km_val)) max_km_val <- NULL
        cat("[add_baro_cond] max_km:", if (is.null(max_km_val)) "NULL" else max_km_val,
            "| max_stations:", input$max_stations, "\n")

        if (run_subset) {
          cat("[add_baro_cond] Running subset for station(s):", paste(selected_stns, collapse = ", "), "\n")
        } else {
          cat("[add_baro_cond] Running for ALL stations\n")
        }

        cat("[add_baro_cond] Calling add_nearest_baro() ...\n")
        t_start <- Sys.time()

        # Capture message() output for calibration diagnostics
        msgs <- character(0)
        result <- withCallingHandlers(
          add_nearest_baro(
            input_data          = input_data,
            metric              = "conductivity",
            baro_data           = baro_data,
            metadata            = metadata,
            max_km              = max_km_val,
            max_stations        = input$max_stations,
            baro_site_selection = baro_sel
          ),
          message = function(m) {
            cat("[add_baro_cond][msg]", conditionMessage(m))
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            cat("[add_baro_cond][warn]", conditionMessage(w), "\n")
            invokeRestart("muffleWarning")
          }
        )

        t_elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1)
        cat("[add_baro_cond] add_nearest_baro() returned in", t_elapsed, "sec\n")
        cat("[add_baro_cond] Result class:", paste(class(result), collapse = "/"), "\n")

        # add_nearest_baro returns a list; extract the data frame
        if (is.list(result) && !is.data.frame(result)) {
          cat("[add_baro_cond] Extracting data frame from list (length", length(result), ")\n")
          result <- result[[1]]
        }

        cat("[add_baro_cond] Result:", nrow(result), "rows,", ncol(result), "cols\n")
        cat("[add_baro_cond] Columns:", paste(names(result), collapse = ", "), "\n")

        # Merge-back: preserve existing results for non-selected stations
        if (run_subset && !is.null(baro_result())) {
          existing <- dplyr::filter(baro_result(),
                                    !(site_station_code %in% selected_stns))
          cat("[add_baro_cond] Merge-back: keeping", nrow(existing), "existing rows for other stations\n")
          result <- dplyr::bind_rows(existing, result) %>%
            dplyr::arrange(site_station_code, timestamp)
        }

        cat("[add_baro_cond] Storing result in baro_result() and rv_bound_data$conductivity_with_baro\n")
        baro_result(result)
        baro_messages(msgs)
        rv_bound_data$conductivity_with_baro <- result

        removeNotification("cond_baro_progress")
        n_stns <- length(unique(result$site_station_code))
        cat("[add_baro_cond] Done. Baro data added for", n_stns, "station(s)\n")
        showNotification(
          sprintf("Baro data added for %d conductivity station(s).", n_stns),
          type = "message", duration = 4
        )

      }, error = function(e) {
        cat("[add_baro_cond][ERROR]", conditionMessage(e), "\n")
        removeNotification("cond_baro_progress")
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

      # One row per conductivity station x baro station
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

      # Compute Haversine distance between each conductivity and baro station
      summary_df <- summary_df %>%
        dplyr::left_join(coords, by = "site_station_code") %>%
        dplyr::rename(cond_lat = latitude, cond_lon = longitude) %>%
        dplyr::left_join(coords, by = c("baro_site_stn_code" = "site_station_code")) %>%
        dplyr::rename(baro_lat = latitude, baro_lon = longitude) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          `Distance (km)` = if (!is.na(cond_lat) && !is.na(cond_lon) &&
                                !is.na(baro_lat) && !is.na(baro_lon)) {
            round(geosphere::distHaversine(
              c(cond_lon, cond_lat), c(baro_lon, baro_lat)
            ) / 1000, 1)
          } else {
            NA_real_
          }
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(
          `Cond Station`  = site_station_code,
          `Baro Station`  = baro_site_stn_code,
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

    # ---- Baro assignment review plot (2-panel: conductivity + baro kPa) ----
    output$baro_assign_plot <- plotly::renderPlotly({
      df <- selected_station_data()
      req(nrow(df) > 0, "airpress_kPa" %in% names(df))

      baro_colors <- c("#E53935", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1",
                       "#6D4C41", "#D81B60", "#1E88E5", "#FDD835", "#546E7A")

      # Prefer _adj columns but fall back to raw
      ec_col <- if ("conduct_uScm_adj" %in% names(df)) "conduct_uScm_adj" else
                if ("spc_uScm_adj" %in% names(df)) "spc_uScm_adj" else "conduct_uScm"

      # Panel 1: Conductivity
      p1 <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = df, x = ~timestamp, y = stats::as.formula(paste0("~", ec_col)),
          type = "scatter", mode = "lines",
          name = paste0("Conductivity (", ec_col, ")"),
          line = list(color = "#2e7d32", width = 1.5)
        ) %>%
        plotly::layout(yaxis = list(title = "Conductivity (\u00b5S/cm)"))

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

  })
}


# -----------------------------------------------------------------------------
# 3. QC Conductivity
# -----------------------------------------------------------------------------
module_cond_qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,

      # ---- Header + Description ----
      tags$h1("QC Conductivity", class = "custom-h1"),
      tags$h2("Automated quality checks, drift correction, and visual review",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Run automated quality checks on conductivity data. The function creates ",
        tags$code("conduct_uScm_adj"), " and ", tags$code("watertemp_C_adj"),
        " columns, applying corrections for temperature range exceedances, ",
        "near-zero temperatures, and spike detection. Flags are added for ice ",
        "and dry conditions for your review. Use drift correction with field ",
        "reference data, then manually remove bad periods as needed."
      ),

      tags$p(
        class = "clean-print",
        style = "background-color: #e3f2fd; padding: 12px; border-radius: 5px;",
        tags$strong("Note:"), " If specific conductivity (",
        tags$code("spconduct_uScm_adj"), ") is available (i.e. temperature ",
        "compensation has been applied in the previous step), the final visual ",
        "review plots and exports will use specific conductivity rather than raw ",
        "conductivity. Specific conductivity is the preferred metric for QAQC ",
        "review as it removes temperature effects and provides a more stable ",
        "signal for identifying sensor drift and anomalies."
      ),

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
              tags$li(tags$strong("OUTLIER_CO"), " \u2014 low limit (uScm) < conductivity < high limit (uScm) \u2192 set to NA"),
              tags$li(tags$strong("OUTLIER_WT"), " \u2014 water temp < -1\u00b0C \u2192 set to NA;", tags$br(), "     -1\u00b0C < water temp < 0\u00b0C \u2192 corrected to 0\u00b0C"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 water temp < ice flag threshold (\u00b0C) \u2192 flagged for review"),
              tags$li(tags$strong("FLAG_DRY"), " \u2014 conductivity < dry flag threshold (uScm) and (if available) air-water temperature difference < flag threshold (\u00b0C) \u2192 flagged for review"),
              tags$li(tags$strong("FLAG_SPIKE"), " \u2014 \u0394 conductivity > spike flag (uScm) \u2192 flagged for review")
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
            tags$code("qc_conductivity()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Runs automated range checks and spike detection on conductivity and ",
                   "water temperature. Outputs the data frame with ",
                   tags$code("conduct_uScm_adj"), ", ", tags$code("watertemp_C_adj"),
                   ", and ", tags$code("flag_co_*"), " columns."),
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
              style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px; border: 2px solid #2e7d32; margin-bottom: 15px;",
              selectInput(ns("station_select"), "Select Station:",
                          choices = NULL, width = "100%")
            ),
            # QAQC parameters
            tags$h4("QAQC Parameters"),
            tags$p(style = "font-size: 13px; color: #666; margin-bottom: 10px;",
                   icon("sliders-h"),
                   " Adjust the thresholds used by ", tags$code("qc_conductivity()"),
                   ". Default values are shown."
            ),

            fluidRow(

              # ---- Conductivity & Temperature side by side ----
              column(6,
                     tags$div(
                       style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px; background: #f9f9f9; height: 100%;",
                       tags$h4("Conductivity Thresholds", style = "margin-top: 0;"),
                       numericInput(ns("cond_low"), "Low (\u00b5S/cm):", value = 0, width = "100%"),
                       numericInput(ns("cond_high"), "High (\u00b5S/cm):", value = 2500, width = "100%")
                     )
              ),

              column(6,
                     tags$div(
                       style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px; background: #f9f9f9; height: 100%;",
                       tags$h4("Temperature Thresholds", style = "margin-top: 0;"),
                       numericInput(ns("temp_low"), "Low (\u00b0C):", value = -2, width = "100%"),
                       numericInput(ns("temp_high"), "High (\u00b0C):", value = 40, width = "100%")
                     )
              ),

              # ---- Spike / Flatline / Dry / Air–Water (2x2) ----
              column(12,
                     tags$div(
                       style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px; background: #f9f9f9; margin-top: 15px;",
                       tags$h4("Spike, Ice & Dry Thresholds", style = "margin-top: 0;"),

                       fluidRow(
                         column(6,
                                numericInput(ns("spike_thresh"), "Spike (\u00b5S/cm):", value = 200, width = "100%")
                         ),
                         column(6,
                                numericInput(ns("ice_thresh"), "Ice temp (\u00b0C):", value = 0.3, width = "100%")
                         )
                       ),
                       fluidRow(
                         column(6,
                                numericInput(ns("dry_thresh"), "Dry (\u00b5S/cm):", value = 10, width = "100%")
                         ),
                         column(6,
                                numericInput(ns("air_water_diff"), "Air–Water Diff (\u00b0C):", value = 2, width = "100%")
                         )
                       )
                     )
              )
            ),

            actionButton(ns("run_qaqc"), "Run Conductivity QAQC",
              class = "btn-primary btn-lg", width = "100%",
              style = "color: #fff; background-color: #2e7d32; border-color: #2e7d32;",
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
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "(Optional) Drift Correction: Warning Experimental"
          )
        ),

        tags$a(
          `data-toggle` = "collapse", href = paste0("#", ns("drift_collapse")),
          style = "display: block; margin-bottom: 10px; color: #2e7d32; font-weight: bold; cursor: pointer;",
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
                "Conductivity sensors can drift due to biofouling, electrode degradation, ",
                "and mineral deposits on the sensor. Field reference measurements taken ",
                "during site visits let you quantify and correct this drift. The correction ",
                "ramps linearly from zero at the start of each segment to the full ",
                "correction at the visit endpoint, then restarts at the next segment. ",
                "No correction is applied after the last visit."
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
                  "the reference/logger ratio at the segment end. Best when drift is ",
                  "proportional to reading magnitude. ", tags$code("conduct_uScm_adj"),
                  " is corrected."
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
                tags$li(tags$code("metric"), " \u2014 'conductivity' or 'cond'"),
                tags$li(tags$code("site_station_code"), " \u2014 station ID"),
                tags$li(tags$code("timestamp"), " \u2014 measurement time (see formats below)"),
                tags$li(tags$code("watertemp_C"), " \u2014 reference temperature"),
                tags$li(tags$code("value"), " \u2014 reference reading (\u00b5S/cm)"),
                tags$li(tags$code("unit"), " \u2014 'uS/cm', 'uScm', or '\u00b5S/cm'"),
                tags$li(tags$code("event_type"), " \u2014 'check', 'calibration', or 'ignore'"),
                tags$li(tags$code("calibration_datetime"), " \u2014 when sensor was recalibrated (same formats as timestamp; leave blank for check/ignore)")
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
                  style = "color: #2e7d32;"
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
                    style = "color: #ffffff; background-color: #2e7d32; border-color: #2e7d32;"),
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
            style = "margin-top: 20px; padding: 15px; background-color: #e8f5e9; border-radius: 5px;",
            fluidRow(
              column(4,
                tags$h5("Ice", style = "margin-top: 0; color: #2e7d32;"),
                tags$p(style = "font-size: 12px;",
                  "Sensor frozen or encased in ice. Sets conductivity and optionally ",
                  "temperature to NA."
                )
              ),
              column(4,
                tags$h5("Dry", style = "margin-top: 0; color: #2e7d32;"),
                tags$p(style = "font-size: 12px;",
                  "Sensor out of water. Sets conductivity values to NA; temperature ",
                  "may still be valid."
                )
              ),
              column(4,
                tags$h5("Disturbance", style = "margin-top: 0; color: #2e7d32;"),
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
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Station Summary Statistics"
          )
        ),

        uiOutput(ns("summary_station_label")),

        fluidRow(
          column(
            width = 4,
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("Conductivity (\u00b5S/cm) Summary", style = "margin-top: 0;"),
              verbatimTextOutput(ns("cond_summary"))
            )
          ),
          column(
            width = 4,
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("SPC (\u00b5S/cm @ 25\u00b0C) Summary", style = "margin-top: 0;"),
              verbatimTextOutput(ns("spc_summary"))
            )
          ),
          column(
            width = 4,
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("Temperature Summary", style = "margin-top: 0;"),
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
                "Download the QAQC'd conductivity data as CSV for the ",
                tags$strong("selected station"), ". The exported file ",
                "contains raw columns, adjusted columns, and all edit/flag ",
                "columns. The QAQC log records every automated and manual ",
                "adjustment applied."
              ),
              tags$p(style = "font-size: 13px; color: #666;",
                "Note: this is the final step in the conductivity pipeline. Select the ",
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
                    style = "width: 100%; color: #ffffff; background-color: #2e7d32; border-color: #2e7d32;")
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

module_cond_qc_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data       <- session$userData$rv_raw_data
    rv_bound_data     <- session$userData$rv_bound_data
    rv_converted_data <- session$userData$rv_converted_data
    rv_qaqc_data      <- session$userData$rv_qaqc_data

    # ---- Local reactive state ----
    cond_qc_result  <- reactiveVal(NULL)
    qaqc_log        <- reactiveVal(NULL)
    drift_ref_data  <- reactiveVal(NULL)
    selected_ranges <- reactiveVal(list())
    range_history   <- reactiveVal(list())

    # ---- Visibility toggles ----
    output$show_results  <- reactive({ !is.null(cond_qc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    output$show_ref_data <- reactive({ !is.null(drift_ref_data()) })
    outputOptions(output, "show_ref_data", suspendWhenHidden = FALSE)

    # ---- Input data reactive (CRITICAL FIX: picks most-processed version) ----
    cond_input_data <- reactive({
      d <- rv_converted_data$conductivity_converted
      if (!is.null(d) && nrow(d) > 0) return(d)
      d <- rv_bound_data$conductivity_with_baro
      if (!is.null(d) && nrow(d) > 0) return(d)
      d <- rv_bound_data$conductivity_bound
      if (!is.null(d) && nrow(d) > 0) return(d)
      NULL
    })

    # ---- Populate station selectors (FIX: use data frame, not list names) ----
    observe({
      df <- cond_input_data()
      req(df, nrow(df) > 0)
      stations <- sort(unique(df$site_station_code))
      stations <- stations[sapply(stations, function(s) {
        any(!is.na(df$conduct_uScm[df$site_station_code == s]))
      })]
      updateSelectInput(session, "station_select",
        choices = stations, selected = stations[1])
      updateSelectInput(session, "drift_station",
        choices = stations, selected = stations[1])
    })

    # ---- Data readiness status (FIX: use cond_input_data) ----
    output$qaqc_status <- renderUI({
      df <- cond_input_data()
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No conductivity data available. Complete Import and Convert steps first."
        )
      } else {
        n_stns <- length(unique(df$site_station_code))
        n_recs <- format(nrow(df), big.mark = ",")
        has_spc <- "spc_uScm_adj" %in% names(df)
        has_baro <- "airtemp_C" %in% names(df)
        status_parts <- sprintf(" Ready: %d station(s), %s records available for QAQC", n_stns, n_recs)
        if (has_spc) status_parts <- paste0(status_parts, " | SPC computed")
        if (has_baro) status_parts <- paste0(status_parts, " | Air temp available (improved dry detection)")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          status_parts
        )
      }
    })

    # ---- Run QAQC (FIX: use cond_input_data + merge pattern) ----
    observeEvent(input$run_qaqc, {
      station <- input$station_select
      req(station)
      input_df <- cond_input_data()
      req(input_df)

      tryCatch({
        showNotification("Running conductivity QAQC...",
          type = "message", duration = NULL, id = "cond_qc_progress")

        result <- qc_conductivity(
          input_data                    = input_df,
          select_station                = station,
          cond_high_limit_uScm          = input$cond_high,
          cond_low_limit_uScm           = input$cond_low,
          temp_low_limit_C              = input$temp_low,
          temp_high_limit_C             = input$temp_high,
          spike_flag_uScm               = input$spike_thresh,
          dry_flag_uScm                 = input$dry_thresh,
          temp_ice_flag_C               = input$ice_thresh,
          temp_air_water_diff_C         = input$air_water_diff
        )

        qaqc_log(attr(result, "qaqc_log"))

        # Merge: replace station rows in full dataset
        other_data <- dplyr::filter(input_df, site_station_code != station)
        full_data <- dplyr::bind_rows(other_data, result) %>%
          dplyr::arrange(site_station_code, timestamp)

        cond_qc_result(full_data)
        rv_qaqc_data$qc_conductivity <- full_data

        selected_ranges(list())
        range_history(list())

        removeNotification("cond_qc_progress")
        showNotification(
          sprintf("Conductivity QAQC complete for %s", station),
          type = "message", duration = 5)

      }, error = function(e) {
        removeNotification("cond_qc_progress")
        showNotification(paste("QAQC error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ---- Selected station data ----
    selected_data <- reactive({
      req(input$station_select, cond_qc_result())
      dplyr::filter(cond_qc_result(), site_station_code == input$station_select)
    })

    # ---- Station info header ----
    output$station_info <- renderUI({
      data <- selected_data()
      req(nrow(data) > 0)

      flag_cols <- c("FLAG_SPIKE", "FLAG_ICE", "FLAG_DRY", "OUTLIER_CO", "OUTLIER_WT")
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

    # ---- Flag summary table ----
    output$flag_summary <- DT::renderDT({
      req(cond_qc_result())
      data <- selected_data()
      req(nrow(data) > 0)

      safe_sum <- function(col) {
        if (col %in% names(data)) sum(as.logical(data[[col]]), na.rm = TRUE) else 0L
      }

      summary_df <- data.frame(
        Station = data$site_station_code[1],
        Records = nrow(data),
        FLAG_SPIKE = safe_sum("FLAG_SPIKE"),
        FLAG_ICE = safe_sum("FLAG_ICE"),
        FLAG_DRY = safe_sum("FLAG_DRY"),
        OUTLIER_CO = safe_sum("OUTLIER_CO"),
        OUTLIER_WT = safe_sum("OUTLIER_WT"),

        Cond_NA = sum(is.na(data$conduct_uScm_adj)),
        Temp_NA = sum(is.na(data$watertemp_C_adj)),
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

    # ---- Reference data upload (ENHANCED validation) ----
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

        # Filter to conductivity rows
        ref <- ref[grepl("^(conductivity|cond|ec)$",
                         trimws(tolower(ref$metric))), ]
        if (nrow(ref) == 0) {
          output$ref_upload_status <- renderUI({
            tags$div(
              style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-top: 10px;",
              icon("exclamation-triangle"),
              " No conductivity rows found in reference data. Check the 'metric' column."
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
            sprintf(" Reference data loaded: %d conductivity measurements across %d station(s)",
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

    # ---- Reference data preview ----
    output$ref_preview <- DT::renderDT({
      req(drift_ref_data())
      DT::datatable(drift_ref_data(),
        options = list(dom = "t", pageLength = 20, scrollX = TRUE),
        rownames = FALSE,
        selection = "none"
      )
    })

    # ---- Run drift correction ----
    observeEvent(input$run_drift, {
      station <- input$drift_station
      req(station)
      req(cond_qc_result())
      req(drift_ref_data())

      tryCatch({
        showNotification("Applying drift correction...",
          type = "message", duration = NULL, id = "cond_drift_progress")

        drift_messages <- character()
        result <- withCallingHandlers(
          wq_drift_correction(
            input_data     = cond_qc_result(),
            ref_data_path  = NULL,
            select_station = station,
            metric         = "cond",
            method         = input$drift_method,
            max_ref_gap_h  = input$max_ref_gap,
            ref_data       = drift_ref_data()
          ),
          message = function(m) {
            drift_messages <<- c(drift_messages, conditionMessage(m))
            invokeRestart("muffleMessage")
          }
        )

        drift_log <- attr(result, "qaqc_log")

        # wq_drift_correction() returns the full dataset (all stations) with
        # the selected station's rows updated in-place via rows_update(),
        # so no merge-back step is needed.
        cond_qc_result(result)
        rv_qaqc_data$qc_conductivity <- result

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

        removeNotification("cond_drift_progress")
        showNotification(sprintf("Drift correction applied for %s", station),
          type = "message", duration = 5)

        # Show summary
        n_drift <- if ("edit_cond_drift" %in% names(result)) {
          sum(result$edit_cond_drift, na.rm = TRUE)
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
        removeNotification("cond_drift_progress")
        showNotification(paste("Drift correction error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ---- QAQC Plot (3-4 panel time series) ----
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

    # ---- Parsed reference data for plotting (station-filtered) ----
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

      # Build panels dynamically
      panels <- list()

      # Panel 1: Conductivity (uS/cm)
      p1 <- plot_ly(data, x = ~timestamp, y = ~conduct_uScm,
              type = "scattergl", mode = "lines",
              line = list(color = "rgba(230,81,0,0.4)", width = 1),
              name = "Raw Cond",
              hovertemplate = "Raw: %{y:.1f} \u00b5S/cm<extra></extra>") %>%
        add_trace(y = ~conduct_uScm_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#2e7d32", width = 1),
              name = "Adj Cond",
              hovertemplate = "Adj: %{y:.1f} \u00b5S/cm<extra></extra>")

      # Overlay conductivity reference points
      if (!is.null(ref)) {
        cond_units <- c("uscm", "us/cm", "\u00b5s/cm")
        ref_cond <- ref[tolower(trimws(ref$unit)) %in% cond_units, ]
        if (nrow(ref_cond) > 0) {
          p1 <- p1 %>% add_trace(
            data = ref_cond, x = ~ts_parsed, y = ~value,
            type = "scatter", mode = "markers",
            inherit = FALSE,
            marker = list(color = "#d32f2f", size = 10, symbol = "diamond",
                         line = list(color = "#ffffff", width = 1.5)),
            name = "Field Ref (cond)",
            text = ~paste0(event_type, ": ", value, " \u00b5S/cm\n", timestamp),
            hovertemplate = "%{text}<extra></extra>"
          )
        }
      }
      panels <- c(panels, list(p1))

      # Panel 2: Water Temperature
      p2 <- plot_ly(data, x = ~timestamp, y = ~watertemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "rgba(97,155,138,0.4)", width = 1),
              name = "Raw Water Temp",
              hovertemplate = "Raw: %{y:.1f} \u00b0C<extra></extra>") %>%
        add_trace(y = ~watertemp_C_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#619b8a", width = 1),
              name = "Adj Water Temp",
              hovertemplate = "Adj: %{y:.1f} \u00b0C<extra></extra>")

      # Overlay temperature reference points
      if (!is.null(ref) && nrow(ref) > 0) {
        p2 <- p2 %>% add_trace(
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
      panels <- c(panels, list(p2))

      # Panel 3: Air Temperature (CONDITIONAL)
      if ("airtemp_C" %in% names(data)) {
        p3 <- plot_ly(data, x = ~timestamp, y = ~airtemp_C,
                type = "scattergl", mode = "lines",
                line = list(color = "#FDB840", width = 1),
                name = "Air Temp",
                hovertemplate = "Air: %{y:.1f} \u00b0C<extra></extra>")
        panels <- c(panels, list(p3))
      }

      n_panels <- length(panels)

      # Y-axis titles
      yaxis_titles <- c("Cond (\u00b5S/cm)", "Water Temp (\u00b0C)")
      if ("airtemp_C" %in% names(data)) yaxis_titles <- c(yaxis_titles, "Air Temp (\u00b0C)")

      p <- subplot(panels, nrows = n_panels, shareX = TRUE, titleY = TRUE)

      layout_args <- list(
        p = p,
        title = list(text = paste("Conductivity QAQC \u2014", station), font = list(size = 14)),
        xaxis = list(title = ""),
        yaxis = list(title = yaxis_titles[1], fixedrange = TRUE),
        yaxis2 = list(title = yaxis_titles[2], fixedrange = TRUE),
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.08),
        margin = list(b = 80)
      )

      if (n_panels >= 3) {
        layout_args$yaxis3 <- list(title = yaxis_titles[3], fixedrange = TRUE)
        layout_args[[paste0("xaxis", n_panels)]] <- list(title = "Date")
      } else {
        layout_args$xaxis2 <- list(title = "Date")
      }

      p <- do.call(layout, layout_args)

      p %>%
        htmlwidgets::onRender(drag_range_js) %>%
        htmlwidgets::onRender(sprintf("
          function(el, x) {
            var flagId = '%s';
            var modeId = '%s';
            if (!window._condSyncGuard) window._condSyncGuard = false;
            el.on('plotly_relayout', function(evt) {
              if (window._condSyncGuard) return;
              var mode = Shiny.shinyapp.$inputValues[modeId];
              if (mode === 'select') return;
              var keys = Object.keys(evt);
              var isShapeOnly = keys.every(function(k) {
                return k === 'shapes' || k.indexOf('shapes[') === 0;
              });
              if (isShapeOnly) return;
              var flagEl = document.getElementById(flagId);
              if (!flagEl || !flagEl.layout) return;
              window._condSyncGuard = true;
              if (evt['xaxis.autorange']) {
                Plotly.relayout(flagEl, {'xaxis.autorange': true}).then(function() { window._condSyncGuard = false; });
              } else {
                var x0 = evt['xaxis.range[0]'];
                var x1 = evt['xaxis.range[1]'];
                if (x0 && x1) {
                  Plotly.relayout(flagEl, {'xaxis.range[0]': x0, 'xaxis.range[1]': x1}).then(function() { window._condSyncGuard = false; });
                } else { window._condSyncGuard = false; }
              }
            });
            el.on('plotly_doubleclick', function() {
              var flagEl = document.getElementById(flagId);
              if (flagEl) {
                window._condSyncGuard = true;
                Plotly.relayout(flagEl, {'xaxis.autorange': true}).then(function() { window._condSyncGuard = false; });
              }
            });
          }
        ", session$ns("flag_plot"), session$ns("plot_mode")))
    })

    # ---- Flag review plot ----
    output$flag_plot <- renderPlotly({
      data <- selected_data()
      req(nrow(data) > 0)

      flag_cols <- c("FLAG_SPIKE", "FLAG_ICE", "FLAG_DRY", "OUTLIER_CO", "OUTLIER_WT")
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
            if (!window._condSyncGuard) window._condSyncGuard = false;
            el.on('plotly_relayout', function(evt) {
              if (window._condSyncGuard) return;
              var keys = Object.keys(evt);
              var isShapeOnly = keys.every(function(k) {
                return k === 'shapes' || k.indexOf('shapes[') === 0;
              });
              if (isShapeOnly) return;
              var mainEl = document.getElementById(mainId);
              if (!mainEl || !mainEl.layout) return;
              window._condSyncGuard = true;
              if (evt['xaxis.autorange']) {
                Plotly.relayout(mainEl, {'xaxis.autorange': true}).then(function() { window._condSyncGuard = false; });
              } else {
                var x0 = evt['xaxis.range[0]'];
                var x1 = evt['xaxis.range[1]'];
                if (x0 && x1) {
                  Plotly.relayout(mainEl, {'xaxis.range[0]': x0, 'xaxis.range[1]': x1}).then(function() { window._condSyncGuard = false; });
                } else { window._condSyncGuard = false; }
              }
            });
            el.on('plotly_doubleclick', function() {
              var mainEl = document.getElementById(mainId);
              if (mainEl) {
                window._condSyncGuard = true;
                Plotly.relayout(mainEl, {'xaxis.autorange': true}).then(function() { window._condSyncGuard = false; });
              }
            });
          }
        ", session$ns("qaqc_plot")))
    })

    # ---- Multi-range selection via drag ----
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
      df <- cond_qc_result()
      if (!is.null(df)) {
        attr(df, ".redraw") <- Sys.time()
        cond_qc_result(df)
      }
    })

    output$range_status <- renderUI({
      n <- length(selected_ranges())
      if (input$plot_mode == "select") {
        if (n == 0) {
          tags$span(style = "font-size: 13px; color: #666;",
                    "Drag across the plot to select a period")
        } else {
          tags$span(style = "font-size: 13px; color: #2e7d32; font-weight: bold;",
                    sprintf("%d range%s selected", n, if (n > 1) "s" else ""))
        }
      }
    })

    # ---- Helpers: timestamp parsing, snapping, merge ----
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
      updated <- merge_station_back(cond_qc_result(), station_data, station)
      selected_ranges(list())
      range_history(list())
      cond_qc_result(updated)
      rv_qaqc_data$qc_conductivity <- updated

      adj_log <- attr(station_data, "qaqc_log")
      if (!is.null(adj_log)) {
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

    # ---- Remove Selected Periods (adjust_logger_NA) ----
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
        station_data <- dplyr::filter(cond_qc_result(), site_station_code == station)
        snapped <- snap_ranges(ranges, station_data$timestamp)

        for (r in snapped) {
          station_data <- adjust_logger_NA(
            input_data       = station_data,
            select_station   = station,
            metric           = "conductivity",
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

    # ---- Summary stats ----
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

    output$cond_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      n_dup <- sum(duplicated(data$timestamp))
      cat("Records:", nrow(data), " | Duplicate timestamps:", n_dup, "\n\n")
      cat("--- Raw Conductivity (\u00b5S/cm) ---\n")
      print(round(summary(data$conduct_uScm), 1))
      cat("\n--- Adjusted Conductivity (\u00b5S/cm) ---\n")
      if ("conduct_uScm_adj" %in% names(data)) {
        print(round(summary(data$conduct_uScm_adj), 1))
      } else {
        cat("(column not yet created)\n")
      }
    })

    output$spc_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      if ("spc_uScm_adj" %in% names(data)) {
        cat("--- SPC (\u00b5S/cm @ 25\u00b0C) ---\n")
        print(round(summary(data$spc_uScm_adj), 1))
      } else {
        cat("SPC not yet computed.\n")
        cat("Run temperature compensation in the Convert tab first.\n")
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

    # ---- Download: Clean CSV ----
    output$download_csv <- downloadHandler(
      filename = function() {
        station <- input$station_select
        paste0("cond_qaqc_", station, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        data <- selected_data()
        req(nrow(data) > 0)

        export_cols <- c(
          "site_station_code", "timestamp",
          "conduct_uScm", "conduct_uScm_adj",
          "spc_uScm_adj",
          "watertemp_C", "watertemp_C_adj",
          "airpress_kPa", "airtemp_C"
        )
        flag_cols <- grep("^(edit_|flag_co_)", names(data), value = TRUE)
        export_cols <- intersect(c(export_cols, flag_cols), names(data))

        export_df <- data[, export_cols]
        export_df$timestamp <- format(export_df$timestamp, "%Y-%m-%d %H:%M:%S")
        write.csv(export_df, file, row.names = FALSE)
      }
    )

    # ---- Download: QAQC Log ----
    output$download_log <- downloadHandler(
      filename = function() {
        station <- input$station_select
        paste0("cond_qaqc_log_", station, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
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

    # ---- Download: Metadata ----
    output$download_metadata <- downloadHandler(
      filename = function() {
        paste0("cond_metadata_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        meta <- rv_raw_data$metadata
        if (!is.null(meta) && nrow(meta) > 0) {
          cond_meta <- meta[grepl("^(conductivity|cond)$",
                                  tolower(trimws(meta$metric))), ]
          write.csv(cond_meta, file, row.names = FALSE)
        } else {
          write.csv(data.frame(), file, row.names = FALSE)
        }
      }
    )

  })
}
