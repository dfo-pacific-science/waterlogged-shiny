# =============================================================================
# Conductivity Workflow Modules
# =============================================================================
#
# Pipeline for HOBO U24 conductivity loggers:
#   1. Import Data  — bind_hobo_files() for conductivity loggers
#   2. Convert & QC — add_nearest_baro(), conductivity_temp_compensation(),
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
        style = "border-top: 2px solid #e65100; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #e65100; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
              tags$li(tags$code("timestamp_deploy"), " / ", tags$code("timestamp_remove")),
              tags$li(tags$code("model"), " \u2014 logger model (e.g. HOBO_U24)"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 ", tags$code("\"conductivity\"")),
              tags$li(tags$code("status"), " \u2014 deployment status"),
              tags$li(tags$code("latitude"), " / ", tags$code("longitude"))
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload your own metadata CSV, or use the pre-loaded sample. ",
              "The file must contain one row per deployment period per logger. ",
              "Include metadata for barometric stations as well if you plan to ",
              "use them later for dry-sensor detection."
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
            ),
            tags$p(style = "font-size: 12px; margin-top: -5px;",
              tags$a(
                href = "sample_metadata.csv",
                download = "sample_metadata.csv",
                icon("download"),
                " Download sample metadata template",
                style = "color: #e65100;"
              )
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
        style = "border-top: 2px solid #e65100; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #e65100; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
              "conductivity loggers. You can select multiple files at once."
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
              tags$li(tags$code("site_station_code"), " \u2014 station ID"),
              tags$li(tags$code("timestamp"), " \u2014 date-time"),
              tags$li(tags$code("conduct_uScm"), " \u2014 conductivity (\u00b5S/cm)"),
              tags$li(tags$code("conduct_source"), " \u2014 range used (\"low\" or \"full\")"),
              tags$li(tags$code("watertemp_C"), " \u2014 water temperature"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 \"conductivity\"")
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
              style = "display: none; text-align: center; padding: 15px; margin-top: 10px; background-color: #fff3e0; border-radius: 5px;",
              tags$div(
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #ffcc80; border-top: 3px solid #e65100; border-radius: 50%; animation: cond-spin 0.8s linear infinite; vertical-align: middle;"
              ),
              tags$span(style = "margin-left: 10px; color: #e65100; font-weight: bold;",
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
        style = "border-top: 2px solid #e65100; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #e65100; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
                style = "color: #e65100; font-weight: bold;",
                "Barometric Pressure"
              ),
              " section first, or upload a previously exported processed baro CSV."
            ),
            tags$p(style = "font-weight: bold; margin-bottom: 5px;",
              "Required columns:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$code("site_station_code")),
              tags$li(tags$code("timestamp")),
              tags$li(tags$code("airpress_kPa")),
              tags$li(tags$code("airpress_kPa_adj")),
              tags$li(tags$code("airtemp_C")),
              tags$li(tags$code("airtemp_C_adj")),
              tags$li("Plus any ", tags$code("edit_*"), " / ", tags$code("flag_*"), " columns")
            ),
            tags$div(
              style = "background-color: #e8e8e8; padding: 10px; border-radius: 4px; margin-top: 10px;",
              tags$h5("Follow Along in the R package", style = "margin-top: 0; margin-bottom: 5px;"),
              tags$code("barometric_qaqc_all()"),
              tags$p(style = "font-size: 12px; color: #666; margin-top: 5px; margin-bottom: 0;",
                     "Run automated QAQC on baro data (produces the required columns)")
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
              style = "display: none; text-align: center; padding: 15px; margin-top: 10px; background-color: #fff3e0; border-radius: 5px;",
              tags$div(
                style = "display: inline-block; width: 24px; height: 24px; border: 3px solid #ffcc80; border-top: 3px solid #e65100; border-radius: 50%; animation: cond-spin 0.8s linear infinite; vertical-align: middle;"
              ),
              tags$span(style = "margin-left: 10px; color: #e65100; font-weight: bold;",
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
        style = "border-top: 2px solid #e65100; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #e65100; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
        style = "border-top: 2px solid #e65100; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #e65100; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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

      # ---- Next step link ----
      tags$div(
        style = "margin-top: 25px; padding: 15px; background-color: #fff3e0; border-radius: 5px; text-align: center;",
        tags$p(style = "margin: 0; font-size: 14px;",
          icon("arrow-right"),
          " Ready to proceed? Continue to ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_cond_convert']\").tab('show'); window.scrollTo(0,0); return false;",
            style = "color: #e65100; font-weight: bold;",
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
          color = "#fff", fillColor = "#e65100", fillOpacity = 0.9,
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
          colors = c("#e65100", "#FF8F00"),
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
          result <- waterlogged::bind_hobo_files(
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

        result <- waterlogged::resample_timestamps(
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
        "using the ISO 27888 standard, enabling comparison across seasons and sites. ",
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
        style = "border-top: 2px solid #e65100; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #e65100; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Part A: Convert to Specific Conductance"
        )
      ),

      tags$div(
        style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
        # ---- Left: Explanation (33%) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex; flex-direction: column;",
          tags$div(
            style = "background-color: #fff3e0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
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
            tags$code("conductivity_temp_compensation()"),
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
              style = "color: #fff; background-color: #e65100; border-color: #e65100;",
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
          style = "border-top: 2px solid #e65100; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #e65100; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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

        result <- waterlogged::conductivity_temp_compensation(
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
            style = "margin-top: 15px; padding: 12px; background: #fff3e0; border-radius: 5px; border-left: 4px solid #e65100;",
            icon("check-circle", style = "color: #e65100;"),
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

        cat("[add_baro_cond] Calling waterlogged::add_nearest_baro() ...\n")
        t_start <- Sys.time()

        # Capture message() output for calibration diagnostics
        msgs <- character(0)
        result <- withCallingHandlers(
          waterlogged::add_nearest_baro(
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
          line = list(color = "#e65100", width = 1.5)
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
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("QAQC Rules", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("Spikes"), " \u2014 conductivity change > threshold between readings \u2192 flagged"),
              tags$li(tags$strong("Ice"), " \u2014 water temp < 0.1\u00b0C \u2192 flagged"),
              tags$li(tags$strong("Temp range"), " \u2014 outside -2 to 40\u00b0C \u2192 set to NA"),
              tags$li(tags$strong("Dry sensor"), " \u2014 very low conductivity or air-water temp convergence \u2192 flagged"),
              tags$li(tags$strong("Near-zero temp"), " \u2014 -1 to 0\u00b0C corrected to 0; \u2264 -1\u00b0C \u2192 NA")
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

            # QAQC parameters
            tags$h4("QAQC Parameters"),
            tags$div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; margin-bottom: 15px;",
              tags$div(
                style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("temp_low"), "Temp Low (\u00b0C):", value = -2, width = "100%")
              ),
              tags$div(
                style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("temp_high"), "Temp High (\u00b0C):", value = 40, width = "100%")
              ),
              tags$div(
                style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("spike_thresh"), "Spike (\u00b5S/cm):", value = 200, width = "100%")
              ),
              tags$div(
                style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("dry_thresh"), "Dry Thresh (\u00b5S/cm):", value = 10, width = "100%")
              ),
              tags$div(
                style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
                numericInput(ns("air_water_diff"), "Air-Water Diff (\u00b0C):", value = 2, width = "100%")
              )
            ),

            tags$div(
              style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px; border: 2px solid #2e7d32; margin-bottom: 15px;",
              selectInput(ns("station_select"), "Select Station:",
                choices = NULL, width = "100%")
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

        # ---- QAQC review plot ----
        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "QAQC Review Plot"
          )
        ),
        tags$div(
          style = "margin-top: 15px;",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("qaqc_plot"), height = "600px"),
            type = getOption("spinner.type", 8)
          )
        ),

        # ================================================================
        # Section 2: Drift Correction
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Drift Correction"
          )
        ),

        tags$div(
          style = "display: flex; flex-wrap: wrap; margin: 0 -15px;",
          # ---- Left: Explanation (33%) ----
          tags$div(
            style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex; flex-direction: column;",
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("Why Drift Correction?", style = "margin-top: 0;"),
              tags$p(style = "font-size: 13px;",
                "Conductivity sensors can drift due to biofouling and electrode ",
                "degradation. Field reference measurements let you correct this drift."
              ),

              tags$h4("Event Type", style = "margin-top: 12px; margin-bottom: 6px;"),
              tags$ul(
                style = "padding-left: 20px; font-size: 13px;",
                tags$li(tags$strong("check"), " \u2014 comparison measurement only (no sensor reset)"),
                tags$li(tags$strong("calibration"), " \u2014 sensor was recalibrated; post-cal readings are trusted"),
                tags$li(tags$strong("ignore"), " \u2014 skip this measurement")
              ),

              tags$h4("Method", style = "margin-top: 12px; margin-bottom: 6px;"),
              tags$ul(
                style = "padding-left: 20px; font-size: 13px;",
                tags$li(tags$strong("Ratio"), " (multiplicative) \u2014 scales values by interpolated ratio"),
                tags$li(tags$strong("Offset"), " (additive) \u2014 shifts values by interpolated difference")
              ),

              tags$h4("Max Gap (hours)", style = "margin-top: 12px; margin-bottom: 6px;"),
              tags$p(style = "font-size: 13px;",
                "Maximum time difference between a reference timestamp and the nearest ",
                "logger reading. Events exceeding this are skipped. Default: 1 hour."
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
                tags$li(tags$code("timestamp"), " \u2014 measurement time"),
                tags$li(tags$code("watertemp_C"), " \u2014 reference temperature"),
                tags$li(tags$code("value"), " \u2014 reference reading (\u00b5S/cm)"),
                tags$li(tags$code("unit"), " \u2014 'uS/cm' or 'uScm'"),
                tags$li(tags$code("event_type"), " \u2014 'check', 'calibration', or 'ignore'"),
                tags$li(tags$code("calibration_datetime"), " \u2014 when sensor was recalibrated (blank for check/ignore)")
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
        tags$div(
          style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px;",
          tags$p(style = "font-size: 13px;",
            "Use ", tags$code("adjust_logger_NA()"), " to manually set adjusted ",
            "values to NA for periods affected by ice, dry conditions, or disturbance. ",
            "Select a time range from the QAQC plot above and specify a reason."
          ),
          tags$p(style = "font-size: 13px; color: #666; font-style: italic;",
            "Manual adjustment controls will be available here in a future update."
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_cond_qc_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data  <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data

    # ---- Local reactive state ----
    cond_qc_result <- reactiveVal(NULL)
    qaqc_log       <- reactiveVal(NULL)
    drift_ref_data <- reactiveVal(NULL)

    # ---- Visibility toggles ----
    output$show_results  <- reactive({ !is.null(cond_qc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    output$show_ref_data <- reactive({ !is.null(drift_ref_data()) })
    outputOptions(output, "show_ref_data", suspendWhenHidden = FALSE)

    # ---- Data status ----
    output$qaqc_status <- renderUI({
      cond <- rv_raw_data$conductivity_data
      has_data <- !is.null(cond) && length(cond) > 0
      if (has_data) {
        stations <- names(cond)
        tags$div(
          style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; border: 1px solid #a5d6a7;",
          icon("check-circle", style = "color: #2e7d32;"),
          sprintf(" Conductivity data loaded: %d station(s) \u2014 %s",
                  length(stations), paste(stations, collapse = ", "))
        )
      } else {
        tags$div(
          style = "background-color: #fff3cd; padding: 12px; border-radius: 5px; border: 1px solid #ffc107;",
          icon("exclamation-triangle", style = "color: #856404;"),
          " No conductivity data loaded. Go to Import Data first."
        )
      }
    })

    # ---- Populate station selector ----
    observe({
      cond <- rv_raw_data$conductivity_data
      if (!is.null(cond) && length(cond) > 0) {
        updateSelectInput(session, "station_select",
          choices = names(cond), selected = names(cond)[1])
      }
    })

    # ---- Run QAQC ----
    observeEvent(input$run_qaqc, {
      station <- input$station_select
      req(station)

      cond <- rv_raw_data$conductivity_data
      req(cond, cond[[station]])

      tryCatch({
        showNotification("Running conductivity QAQC...",
          type = "message", duration = NULL, id = "cond_qc_progress")

        # Combine all stations into single DF for qc_conductivity
        cond_df <- dplyr::bind_rows(cond, .id = "site_station_code")
        if (!"site_station_code" %in% names(cond_df) ||
            all(is.na(cond_df$site_station_code))) {
          cond_df$site_station_code <- station
        }

        result <- waterlogged::qc_conductivity(
          input_data                    = cond_df,
          select_station                = station,
          temp_low_limit_C              = input$temp_low,
          temp_high_limit_C             = input$temp_high,
          disturbance_threshold_uScm    = input$spike_thresh,
          dry_threshold_uScm            = input$dry_thresh,
          air_water_diff_threshold_C    = input$air_water_diff
        )

        log_rows <- attr(result, "qaqc_log")
        cond_qc_result(result)
        qaqc_log(log_rows)

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

    # ---- Flag summary table ----
    output$flag_summary <- DT::renderDT({
      req(cond_qc_result())
      result <- cond_qc_result()
      flag_cols <- grep("^flag_|^edit_", names(result), value = TRUE)
      if (length(flag_cols) == 0) return(NULL)

      counts <- sapply(flag_cols, function(col) sum(result[[col]], na.rm = TRUE))
      summary_df <- data.frame(
        Flag = names(counts),
        Count = unname(counts),
        stringsAsFactors = FALSE
      )
      summary_df <- summary_df[order(-summary_df$Count), ]

      DT::datatable(summary_df, rownames = FALSE,
        options = list(pageLength = 20, dom = "t"),
        class = "compact stripe")
    })

    # ---- QAQC review plot ----
    output$qaqc_plot <- plotly::renderPlotly({
      req(cond_qc_result())
      result <- cond_qc_result()
      station <- input$station_select

      waterlogged::plot_qaqc_timeseries(
        cond_data = result,
        select_station = station
      )
    })

    # ---- Drift: populate station selector ----
    observe({
      if (!is.null(cond_qc_result())) {
        stations <- unique(cond_qc_result()$site_station_code)
        updateSelectInput(session, "drift_station",
          choices = stations, selected = stations[1])
      }
    })

    # ---- Drift: upload reference data ----
    observeEvent(input$ref_upload, {
      req(input$ref_upload)
      tryCatch({
        ref <- read.csv(input$ref_upload$datapath, stringsAsFactors = FALSE)
        required <- c("metric", "site_station_code", "timestamp",
                       "watertemp_C", "value", "unit", "event_type",
                       "calibration_datetime")
        missing <- setdiff(required, names(ref))
        if (length(missing) > 0) {
          output$ref_upload_status <- renderUI({
            tags$div(
              style = "color: #c62828; margin-top: 8px;",
              icon("times-circle"),
              sprintf(" Missing columns: %s", paste(missing, collapse = ", "))
            )
          })
          drift_ref_data(NULL)
          return()
        }
        drift_ref_data(ref)
        output$ref_upload_status <- renderUI({
          tags$div(
            style = "color: #2e7d32; margin-top: 8px;",
            icon("check-circle"),
            sprintf(" Reference data loaded: %d rows", nrow(ref))
          )
        })
      }, error = function(e) {
        output$ref_upload_status <- renderUI({
          tags$div(
            style = "color: #c62828; margin-top: 8px;",
            icon("times-circle"),
            paste(" Error reading file:", conditionMessage(e))
          )
        })
        drift_ref_data(NULL)
      })
    })

    # ---- Drift: preview table ----
    output$ref_preview <- DT::renderDT({
      req(drift_ref_data())
      DT::datatable(drift_ref_data(),
        rownames = FALSE, options = list(pageLength = 10, dom = "tp"),
        class = "compact stripe")
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
          waterlogged::wq_drift_correction(
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
        cond_qc_result(result)

        # Append drift log
        if (!is.null(drift_log)) {
          existing <- qaqc_log()
          if (!is.null(existing)) {
            drift_log$ts_start <- as.character(drift_log$ts_start)
            drift_log$ts_end   <- as.character(drift_log$ts_end)
          }
          qaqc_log(dplyr::bind_rows(existing, drift_log))
        }

        removeNotification("cond_drift_progress")
        showNotification(sprintf("Drift correction applied for %s", station),
          type = "message", duration = 5)

        n_drift <- if ("edit_cond_drift" %in% names(result)) {
          sum(result$edit_cond_drift, na.rm = TRUE)
        } else 0L

        output$drift_result_summary <- renderUI({
          tags$div(
            style = "margin-top: 10px; padding: 10px; background: #e8f5e9; border-radius: 5px;",
            sprintf(" Drift correction complete for %s: %s records corrected.",
                    station, format(n_drift, big.mark = ",")),
            if (length(drift_messages) > 0) {
              tags$div(style = "margin-top: 5px; font-size: 12px; color: #555;",
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

  })
}
