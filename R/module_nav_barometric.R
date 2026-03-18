# =============================================================================
# Barometric Pressure Workflow Modules — Full Implementation
# =============================================================================
#
# Pipeline for HOBO U20 barometric pressure loggers (deployed in air):
#   1. Import Data     — bind_hobo_files() + resample_timestamps()
#   2. QC Barometric   — barometric_qaqc_all() + adjust_logger_NA() + export
#
# Pre-loaded with demo data from inst/extdata/ (ALBR_ST_30, DARL_ST_30).
#
# Data flow:
#   rv_raw_data$baro_data (named list, demo)
#     → rv_bound_data$baro_bound (combined DF, import tab)
#       → rv_qaqc_data$baro_qaqc (QC'd DF, QC tab)
# =============================================================================


# =============================================================================
# 1. Import Data — UI
# =============================================================================
module_baro_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Import Barometric Pressure Data", class = "custom-h1"),
      tags$h2("Upload raw HOBO U20 barometric logger files and align timestamps",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Import raw HOBO CSV files from barometric pressure loggers ",
        "(HOBO U20 deployed in air). These loggers record atmospheric ",
        "pressure (kPa) and air temperature (\u00b0C). Clean barometric data ",
        "is required before processing water level or dissolved oxygen data. ",
        "The HOBO U20 is a dual-channel logger (pressure + temperature). ",
        "When deployed in air, it measures atmospheric pressure. It is the ",
        "same hardware as the water level loggers, distinguished by ",
        "deployment context (air vs water). Typical pressure range: ",
        "85\u2013105 kPa."
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(style = "margin: 0;",
          tags$strong("Important:"), " Barometric data feeds into ",
          tags$strong("water level"), " (pressure compensation) and ",
          tags$strong("dissolved oxygen"), " (% saturation). Process and QC ",
          "barometric data before those workflows."
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
              tags$li(tags$code("timestamp_deploy"), " / ", tags$code("timestamp_remove")),
              tags$li(tags$code("model"), " \u2014 logger model (e.g. HOBO_U20)"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 must be ", tags$code("\"barometric\"")),
              tags$li(tags$code("status"), " \u2014 deployment status"),
              tags$li(tags$code("latitude"), " / ", tags$code("longitude"))
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "Upload your own metadata CSV, or use the pre-loaded sample. ",
              "The file must contain one row per deployment period per logger."
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
                style = "color: #1565c0;"
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
      # Row 2: Raw Logger Data
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
          "Raw Barometric Logger Data"
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
              "barometric loggers. You can select multiple files at once."
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
              tags$li(tags$code("site_station_code"), " \u2014 station ID"),
              tags$li(tags$code("timestamp"), " \u2014 date-time"),
              tags$li(tags$code("airpress_kPa"), " \u2014 absolute pressure"),
              tags$li(tags$code("airtemp_C"), " \u2014 air temperature"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 \"barometric\"")
            )
          )
        ),

        # ---- Right: File upload (narrow) ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex;",

          # Raw HOBO files upload
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0; flex: 1;",
            tags$h4("Raw Baro Logger Files", style = "margin-top: 0;"),
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
            uiOutput(ns("bind_summary"))
          )
        )
      ),

      # ================================================================
      # Row 3: Snap Timestamps
      # ================================================================
      tags$div(
        style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
        tags$span(
          style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
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
              "Round timestamps to a regular interval so multiple baro ",
              "stations align. Original timestamps are preserved in ",
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
      # Row 4: Review Raw Series
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
        "Any quality issues identified here will be addressed in the next step ",
        "using automated QAQC rules and manual adjustments. When you are ready, ",
        "proceed to ", tags$strong("QC Barometric"), " in the sidebar."
      ),

      # ---- Import Summary (full width) ----
      tags$h4("Import Summary"),
      DT::DTOutput(ns("import_summary")),

      tags$div(style = "margin-top: 15px;"),

      # ---- Preview Plot (full width) ----
      tags$h4("Data Preview"),
      plotlyOutput(ns("preview_plot"), height = "650px") %>% withSpinner(),

      # ---- Next step link ----
      tags$div(
        style = "margin-top: 25px; padding: 15px; background-color: #e3f2fd; border-radius: 5px; text-align: center;",
        tags$p(style = "margin: 0; font-size: 14px;",
          icon("arrow-right"),
          " Ready to proceed? Continue to ",
          tags$a(
            href = "#",
            onclick = "$(\"a[href='#shiny-tab-t_baro_qc']\").tab('show'); return false;",
            style = "color: #1565c0; font-weight: bold;",
            "QC Barometric"
          ),
          " to run automated quality control."
        )
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 1. Import Data — Server
# =============================================================================
module_baro_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data  <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data

    # ------ Initialize from demo data ------
    observe({
      data_list <- rv_raw_data$baro_data
      if (!is.null(data_list) && length(data_list) > 0 &&
          is.null(rv_bound_data$baro_bound)) {
        combined <- dplyr::bind_rows(data_list)
        if ("timestamp" %in% names(combined) && is.character(combined$timestamp)) {
          combined$timestamp <- .parse_timestamp(combined$timestamp)
        }
        rv_bound_data$baro_bound <- combined
      }
    })

    # ------ Current baro data reactive ------
    baro_current <- reactive({
      rv_bound_data$baro_bound
    })

    # ------ Data status display ------
    output$data_status <- renderUI({
      df <- baro_current()
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px;",
          icon("info-circle"),
          " No barometric data loaded. Upload files above or use pre-loaded sample data."
        )
      } else {
        n_stations <- length(unique(df$site_station_code))
        n_records <- format(nrow(df), big.mark = ",")
        date_min <- format(min(df$timestamp, na.rm = TRUE), "%Y-%m-%d")
        date_max <- format(max(df$timestamp, na.rm = TRUE), "%Y-%m-%d")
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px;",
          icon("check-circle"),
          sprintf(" Data loaded: %d station(s), %s records (%s to %s)",
                  n_stations, n_records, date_min, date_max)
        )
      }
    })

    # ------ Station map ------
    output$station_map <- renderLeaflet({
      metadata <- rv_raw_data$metadata
      req(metadata)

      # Filter to barometric stations with valid coords
      baro_meta <- metadata[metadata$metric == "barometric", ]
      baro_sites <- unique(baro_meta[, c("site_station_code", "latitude", "longitude")])
      baro_sites <- baro_sites[!is.na(baro_sites$latitude) & !is.na(baro_sites$longitude), ]

      req(nrow(baro_sites) > 0)

      leaflet(baro_sites) %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = 7, weight = 2,
          color = "#fff", fillColor = "#1565C0", fillOpacity = 0.9,
          label = ~site_station_code,
          popup = ~paste0(
            "<strong>", site_station_code, "</strong><br>",
            "Lat: ", round(latitude, 4), "<br>",
            "Lon: ", round(longitude, 4)
          )
        ) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Topo"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })

    # ------ Station metadata table ------
    output$station_table <- DT::renderDT({
      metadata <- rv_raw_data$metadata
      req(metadata)

      baro_meta <- metadata[metadata$metric == "barometric", ]
      req(nrow(baro_meta) > 0)

      display_cols <- intersect(
        c("site_station_code", "timestamp_deploy", "timestamp_remove",
          "model", "sn", "metric", "status", "latitude", "longitude"),
        names(baro_meta)
      )

      DT::datatable(baro_meta[, display_cols],
        options = list(
          dom = "tip",
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # ------ Deployment timeline (dynamic height) ------
    output$deployment_timeline_ui <- renderUI({
      metadata <- rv_raw_data$metadata
      req(metadata)
      n_rows <- sum(metadata$metric == "barometric")
      height_px <- max(200, n_rows * 50 + 80)
      plotlyOutput(session$ns("deployment_timeline"),
        height = paste0(height_px, "px")) %>% withSpinner()
    })

    # ------ Deployment timeline (Gantt chart) ------
    output$deployment_timeline <- renderPlotly({
      metadata <- rv_raw_data$metadata
      req(metadata)

      baro_meta <- metadata[metadata$metric == "barometric", ]
      req(nrow(baro_meta) > 0)

      # Parse deploy/remove timestamps
      baro_meta$deploy <- as.POSIXct(baro_meta$timestamp_deploy,
        format = "%m/%d/%Y %H:%M", tz = "UTC")
      baro_meta$remove <- as.POSIXct(baro_meta$timestamp_remove,
        format = "%m/%d/%Y %H:%M", tz = "UTC")

      # Order by station then deploy time
      baro_meta <- baro_meta[order(baro_meta$site_station_code, baro_meta$deploy), ]

      # Create a unique label per row for the y-axis
      baro_meta$row_label <- paste0(
        baro_meta$site_station_code, " (SN: ", baro_meta$sn, ") [",
        seq_len(nrow(baro_meta)), "]"
      )
      # Preserve order as factor
      baro_meta$row_label <- factor(baro_meta$row_label,
        levels = rev(baro_meta$row_label))

      # Colour by unique serial number
      baro_meta$sn_factor <- as.character(baro_meta$sn)

      # Build colour palette
      unique_sns <- unique(baro_meta$sn_factor)
      palette <- RColorBrewer::brewer.pal(max(3, length(unique_sns)), "Set2")
      sn_colors <- setNames(palette[seq_along(unique_sns)], unique_sns)

      # Build plot with one trace per SN (for legend grouping)
      p <- plot_ly()
      legend_shown <- character(0)

      for (i in seq_len(nrow(baro_meta))) {
        row <- baro_meta[i, ]
        show_legend <- !(row$sn_factor %in% legend_shown)
        if (show_legend) legend_shown <- c(legend_shown, row$sn_factor)

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
                     categoryarray = rev(levels(baro_meta$row_label))),
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

        rv_raw_data$metadata <- metadata
        showNotification("Metadata updated.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error reading metadata:", conditionMessage(e)),
          type = "error", duration = 5)
      })
    })

    # ------ Update station selector for snap ------
    observe({
      df <- baro_current()
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
      meta_file <- input$meta_file
      raw_files <- input$raw_files

      # Resolve metadata
      if (!is.null(meta_file)) {
        metadata <- read.csv(meta_file$datapath, stringsAsFactors = FALSE)
      } else {
        metadata <- rv_raw_data$metadata
      }

      if (is.null(metadata)) {
        showNotification("No metadata available. Upload a metadata CSV first.",
          type = "error", duration = 5)
        return()
      }

      # If raw files were uploaded, bind them
      if (!is.null(raw_files)) {
        tryCatch({
          showNotification("Binding HOBO files...", type = "message",
            duration = NULL, id = "bind_progress")

          result <- waterlogged::bind_hobo_files(
            file_paths = raw_files$datapath,
            metadata = metadata
          )

          bound_data <- result[[1]]

          # Update shared reactive
          rv_bound_data$baro_bound <- bound_data

          # Also update metadata if uploaded
          if (!is.null(meta_file)) {
            rv_raw_data$metadata <- metadata
          }

          # Track bind results for summary display
          bound_stations <- unique(bound_data$site_station_code)
          meta_baro_stations <- unique(
            metadata$site_station_code[metadata$metric %in% c("barometric")]
          )
          matched <- intersect(bound_stations, meta_baro_stations)
          unmatched_in_data <- setdiff(bound_stations, meta_baro_stations)

          rv_bind_result$success <- TRUE
          rv_bind_result$n_stations <- length(bound_stations)
          rv_bind_result$n_records <- nrow(bound_data)
          rv_bind_result$matched_stations <- matched
          rv_bind_result$unmatched_sns <- unmatched_in_data
          rv_bind_result$metadata_stations <- meta_baro_stations

          removeNotification("bind_progress")
          n_rows <- format(nrow(bound_data), big.mark = ",")
          n_stns <- length(bound_stations)
          showNotification(
            sprintf("Import complete: %s records from %d station(s)", n_rows, n_stns),
            type = "message", duration = 5
          )
        }, error = function(e) {
          removeNotification("bind_progress")
          rv_bind_result$success <- FALSE
          showNotification(paste("Import error:", conditionMessage(e)),
            type = "error", duration = 8)
        })
      } else {
        # No raw files — just update metadata if uploaded
        if (!is.null(meta_file)) {
          rv_raw_data$metadata <- metadata
          showNotification("Metadata updated.", type = "message", duration = 3)
        } else {
          showNotification(
            "No files selected. Using pre-loaded sample data.",
            type = "warning", duration = 3
          )
        }
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
      df <- baro_current()
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
          metric = "BARO"
        )

        rv_bound_data$baro_bound <- result

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
      df <- baro_current()
      req(df)

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `Start Date` = format(min(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
          `End Date` = format(max(timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M"),
          `Press Min (kPa)` = round(min(airpress_kPa, na.rm = TRUE), 2),
          `Press Max (kPa)` = round(max(airpress_kPa, na.rm = TRUE), 2),
          `Temp Min (C)` = round(min(airtemp_C, na.rm = TRUE), 1),
          `Temp Max (C)` = round(max(airtemp_C, na.rm = TRUE), 1),
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ------ Preview plot ------
    output$preview_plot <- renderPlotly({
      df <- baro_current()
      req(df, nrow(df) > 0)

      stations <- unique(df$site_station_code)

      # Pressure panel
      p1 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(df, site_station_code == stn)
        p1 <- p1 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~airpress_kPa,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- Pressure"),
          hovertemplate = paste0(stn, ": %{y:.2f} kPa<extra></extra>")
        )
      }

      # Temperature panel
      p2 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(df, site_station_code == stn)
        p2 <- p2 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~airtemp_C,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- Temp"),
          showlegend = FALSE,
          hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
        )
      }

      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "Barometric Data Preview", font = list(size = 14)),
          xaxis2 = list(title = "Date"),
          yaxis = list(title = "Pressure (kPa)"),
          yaxis2 = list(title = "Air Temp (\u00b0C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15),
          margin = list(b = 80)
        )
    })

  })
}


# =============================================================================
# 2. QC Barometric — UI
# =============================================================================
module_baro_qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("QC Barometric Pressure", class = "custom-h1"),
      tags$h2("Automated quality control for barometric pressure and air temperature",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "This page has two stages. First, run the ", tags$strong("Automated QAQC"),
        " to apply range checks, spike detection, and flatline detection to all ",
        "barometric stations at once. Flagged readings are set to NA in the ",
        "adjusted columns while raw data is preserved. Then, in the ",
        tags$strong("Station Detail & Manual Adjustments"), " section, review each ",
        "station's time series and use the Select Range tool to manually remove ",
        "any additional bad periods that the automated checks missed. Finally, ",
        "export the clean data."
      ),

      # ---- Data readiness status ----
      uiOutput(ns("qaqc_status")),

      hr(),

      # ================================================================
      # Section 1: Run QAQC
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
        # ---- Left: QAQC explanation ----
        tags$div(
          style = "flex: 0 0 33.333%; max-width: 33.333%; padding: 0 15px; display: flex; flex-direction: column;",
          tags$div(
            style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("QAQC Rules", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "The automated QAQC applies the following checks to each station:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("edit_press_high/low"), " \u2014 pressure outside ",
                      "range \u2192 adj set to NA"),
              tags$li(tags$strong("edit_temp_range"), " \u2014 temp outside range ",
                      "\u2192 adj set to NA"),
              tags$li(tags$strong("edit_spike"), " \u2014 sudden pressure jump ",
                      "\u2192 flagged (value kept)"),
              tags$li(tags$strong("edit_flatline"), " \u2014 consecutive identical ",
                      "values \u2192 flagged (value kept)")
            ),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 10px;",
              "Spike and flatline flags mark rows for review but do ",
              tags$strong("not"), " set values to NA."
            )
          ),

          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("barometric_qaqc_all()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Runs all QAQC checks on every barometric station at once")
          ),

          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; flex: 1;",
            tags$h4("Columns Produced", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px; margin-bottom: 0;",
              tags$li(tags$code("airpress_kPa_adj"), " \u2014 adjusted pressure"),
              tags$li(tags$code("airtemp_C_adj"), " \u2014 adjusted temperature"),
              tags$li(tags$code("edit_press_high"), ", ",
                      tags$code("edit_press_low")),
              tags$li(tags$code("edit_temp_range"), ", ",
                      tags$code("edit_spike"), ", ",
                      tags$code("edit_flatline"))
            )
          )
        ),

        # ---- Right: QAQC parameters & run button ----
        tags$div(
          style = "flex: 0 0 66.667%; max-width: 66.667%; padding: 0 15px; display: flex;",
          tags$div(
            style = "background-color: #fafafa; padding: 20px; border-radius: 5px; border: 1px solid #e0e0e0; flex: 1;",

            # Station scope selector
            tags$h4("Stations to Process", style = "margin-top: 0;"),
            selectizeInput(ns("qaqc_stations"), NULL,
              choices = NULL, multiple = TRUE,
              width = "100%",
              options = list(
                placeholder = "All stations (default)",
                plugins = list("remove_button")
              )
            ),
            tags$p(style = "font-size: 12px; color: #666; margin-top: -5px; margin-bottom: 15px;",
              "Leave empty to run on all stations. Select specific stations to ",
              "re-run QAQC with different thresholds \u2014 existing results for ",
              "other stations will be preserved."
            ),

            fluidRow(
              column(6,
                tags$h4("Pressure Thresholds", style = "margin-top: 0;"),
                numericInput(ns("pressure_low"), "Low pressure limit (kPa):",
                  value = 85, min = 50, max = 110, step = 1),
                numericInput(ns("pressure_high"), "High pressure limit (kPa):",
                  value = 105, min = 90, max = 120, step = 1)
              ),
              column(6,
                tags$h4("Temperature Thresholds", style = "margin-top: 0;"),
                numericInput(ns("temp_low"), "Low temperature limit (\u00b0C):",
                  value = -40, min = -60, max = 0, step = 1),
                numericInput(ns("temp_high"), "High temperature limit (\u00b0C):",
                  value = 50, min = 20, max = 80, step = 1)
              )
            ),
            fluidRow(
              column(6,
                tags$h4("Spike & Flatline", style = "margin-top: 0;"),
                numericInput(ns("spike_threshold"), "Spike threshold (kPa):",
                  value = 1.5, min = 0.1, max = 10, step = 0.1),
                numericInput(ns("flatline_n"), "Flatline window (consecutive readings):",
                  value = 6, min = 2, max = 50, step = 1)
              ),
              column(6,
                tags$div(
                  style = "margin-top: 30px;",
                  actionButton(ns("run_qaqc"), "Run Barometric QAQC",
                    class = "btn-success btn-lg", width = "100%",
                    icon = icon("play"),
                    style = "color: #ffffff;"
                  )
                )
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
        # Section 2: QAQC Results
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "QAQC Results"
          )
        ),

        tags$p(
          class = "clean-print",
          "Review the flag summary below to see how many records were flagged ",
          "by each rule. Select a station to view the detailed time series with ",
          "raw vs adjusted values and flag markers."
        ),

        # ---- Flag Summary (full width) ----
        tags$h4("Flag Summary by Station"),
        DT::DTOutput(ns("flag_summary")),


        # ================================================================
        # Section 3: Station Detail & Manual Adjustments
        # ================================================================
        tags$div(
          style = "border-top: 2px solid #1565c0; margin: 25px 0 20px 0; position: relative;",
          tags$span(
            style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #1565c0; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
            "Station Detail & Manual Adjustments"
          )
        ),

        # Station selector, plot, and controls — single container
        tags$div(
          style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #e0e0e0;",

          # Station selector row
          fluidRow(
            column(4,
              tags$div(
                style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; border: 2px solid #1565c0;",
                selectInput(ns("station_select"), "Select Station:",
                  choices = NULL, width = "100%")
              )
            ),
            column(8, uiOutput(ns("station_info")))
          ),

          # QAQC plot
          tags$div(style = "margin-top: 15px;",
            plotlyOutput(ns("qaqc_plot"), height = "550px") %>% withSpinner()
          ),

          # Controls — centered, stacked vertically
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
              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
              actionButton(ns("undo_range"), "Undo",
                class = "btn-default btn-sm", icon = icon("undo")),
              actionButton(ns("clear_ranges"), "Clear Ranges",
                class = "btn-default btn-sm", icon = icon("times")),
              actionButton(ns("apply_ranges"), "Remove Selected Periods",
                class = "btn-warning btn-sm", icon = icon("eraser"),
                style = "color: #ffffff;")
            ),
            uiOutput(ns("range_status"))
          ),

          # Manual adjustment explanation
          tags$div(
            style = "margin-top: 20px; padding: 15px; background-color: #f0f0f0; border-radius: 5px;",
            fluidRow(
              column(
                width = 6,
                tags$h4("Remove Bad Periods", style = "margin-top: 0;"),
                tags$p(style = "font-size: 13px;",
                  "If the automated QAQC missed a bad period, switch to ",
                  tags$strong("Select Range"), " mode above and drag across the ",
                  "plot to highlight the period to remove. You can select multiple ",
                  "ranges, then click ", tags$strong("Remove Selected Periods"),
                  " to set the adjusted values to NA. You can optionally keep air ",
                  "temperature values even when pressure is removed."
                )
              ),
              column(
                width = 6,
                tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
                tags$code("adjust_logger_NA()"),
                tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                       "Set adjusted columns to NA for a specified time range")
              )
            )
          )
        ),

        # ================================================================
        # Section 4: Station Summary Statistics
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
            width = 6,
            tags$div(
              style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              tags$h4("Pressure Summary (raw vs adjusted)", style = "margin-top: 0;"),
              verbatimTextOutput(ns("pressure_summary"))
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
        # Section 5: Export Clean Data
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
                "Download the QAQC'd barometric data as CSV. The exported file ",
                "contains data for ", tags$strong("all stations"), " combined, ",
                "including raw columns, adjusted columns, and all edit flags. The ",
                "QAQC log records a summary of every check applied."
              ),
              tags$p(style = "font-size: 13px; color: #666;",
                "Note: the summary statistics above show one station at a time, ",
                "but the CSV export includes every station processed in this session."
              ),
              tags$p(style = "font-size: 13px; color: #666;",
                "The clean barometric data will also be available to downstream ",
                "workflows (water level, dissolved oxygen) that require barometric ",
                "pressure for compensation and conversion."
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
        ),

        # ---- Next step links ----
        tags$div(
          style = "margin-top: 25px; padding: 15px; background-color: #e3f2fd; border-radius: 5px; text-align: center;",
          tags$p(style = "margin: 0; font-size: 14px;",
            icon("arrow-right"),
            " Ready to proceed? Download timeseries and process ",
            tags$a(
              href = "#",
              onclick = "$(\"a[href='#shiny-tab-t_wl_upload']\").tab('show'); return false;",
              style = "color: #1565c0; font-weight: bold;",
              "Water Level Data"
            ),
            " or ",
            tags$a(
              href = "#",
              onclick = "$(\"a[href='#shiny-tab-t_do_upload']\").tab('show'); return false;",
              style = "color: #1565c0; font-weight: bold;",
              "Dissolved Oxygen Data"
            ),
            "."
          )
        )
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 2. QC Barometric — Server
# =============================================================================
module_baro_qc_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data  <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data
    rv_qaqc_data <- session$userData$rv_qaqc_data

    # Local reactive for QAQC result and log
    baro_qc_result <- reactiveVal(NULL)
    qaqc_log <- reactiveVal(NULL)

    # Conditional panel visibility
    output$show_results <- reactive({ !is.null(baro_qc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    # ------ Populate QAQC station selector from bound data ------
    observe({
      df <- rv_bound_data$baro_bound
      if (!is.null(df) && nrow(df) > 0) {
        stations <- sort(unique(df$site_station_code))
        updateSelectizeInput(session, "qaqc_stations",
          choices = stations, selected = character(0), server = FALSE)
      }
    })

    # ------ Data readiness status ------
    output$qaqc_status <- renderUI({
      df <- rv_bound_data$baro_bound
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No barometric data available. Import data in the previous tab first."
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

    # ------ Run QAQC ------
    observeEvent(input$run_qaqc, {
      baro_data <- rv_bound_data$baro_bound
      metadata <- rv_raw_data$metadata

      if (is.null(baro_data) || nrow(baro_data) == 0) {
        showNotification("No barometric data. Import data first.",
          type = "error", duration = 5)
        return()
      }
      if (is.null(metadata)) {
        showNotification("No metadata available.",
          type = "error", duration = 5)
        return()
      }

      # Determine which stations to process
      selected_stns <- input$qaqc_stations
      run_subset <- length(selected_stns) > 0

      if (run_subset) {
        # Filter input data and metadata to selected stations only
        baro_data <- dplyr::filter(baro_data,
          site_station_code %in% selected_stns)
        metadata <- dplyr::filter(metadata,
          site_station_code %in% selected_stns |
          !(metric %in% c("barometric", "baro")))
        if (nrow(baro_data) == 0) {
          showNotification("No data for selected station(s).",
            type = "error", duration = 5)
          return()
        }
      }

      tryCatch({
        n_stns <- length(if (run_subset) selected_stns
          else unique(baro_data$site_station_code))
        showNotification(
          sprintf("Running barometric QAQC on %d station(s)...", n_stns),
          type = "message", duration = NULL, id = "qaqc_progress")

        result <- waterlogged::barometric_qaqc_all(
          baro_data           = baro_data,
          metadata            = metadata,
          path_to_output_folder = NULL,
          log_root            = NULL,
          pressure_low_kpa    = input$pressure_low,
          pressure_high_kpa   = input$pressure_high,
          temp_low_limit      = input$temp_low,
          temp_high_limit     = input$temp_high,
          spike_threshold_kpa = input$spike_threshold,
          flatline_n          = input$flatline_n
        )

        # Merge with existing results for non-selected stations
        if (run_subset && !is.null(baro_qc_result())) {
          existing <- dplyr::filter(baro_qc_result(),
            !(site_station_code %in% selected_stns))
          result <- dplyr::bind_rows(existing, result) %>%
            dplyr::arrange(site_station_code, timestamp)
        }

        # Store result and log
        baro_qc_result(result)
        rv_qaqc_data$baro_qaqc <- result
        qaqc_log(attr(result, "qaqc_log"))

        removeNotification("qaqc_progress")
        msg <- if (run_subset) {
          sprintf("Barometric QAQC complete for %s!", paste(selected_stns, collapse = ", "))
        } else {
          "Barometric QAQC complete for all stations!"
        }
        showNotification(msg, type = "message", duration = 3)

        # Reset select-range state
        selected_ranges(list())
        range_history(list())

      }, error = function(e) {
        removeNotification("qaqc_progress")
        showNotification(paste("QAQC Error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Flag summary table ------
    output$flag_summary <- DT::renderDT({
      req(baro_qc_result())
      df <- baro_qc_result()

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `High Pressure` = sum(edit_press_high, na.rm = TRUE),
          `Low Pressure` = sum(edit_press_low, na.rm = TRUE),
          `Temp Range` = sum(edit_temp_range, na.rm = TRUE),
          Spike = sum(edit_spike, na.rm = TRUE),
          Flatline = sum(edit_flatline, na.rm = TRUE),
          `Pressure NA (adj)` = sum(is.na(airpress_kPa_adj)),
          `Temp NA (adj)` = sum(is.na(airtemp_C_adj)),
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE,
        selection = "single"
      )
    })

    # ------ Update station selectors ------
    # Only update choices when the station list changes; preserve current selection.
    prev_stations <- reactiveVal(character(0))
    observe({
      req(baro_qc_result())
      stations <- unique(baro_qc_result()$site_station_code)
      if (!identical(stations, isolate(prev_stations()))) {
        prev_stations(stations)
        current <- isolate(input$station_select)
        sel <- if (!is.null(current) && current %in% stations) current else stations[1]
        updateSelectInput(session, "station_select",
          choices = stations, selected = sel)
      }
    })

    # ------ Selected station data ------
    selected_data <- reactive({
      req(input$station_select, baro_qc_result())
      dplyr::filter(baro_qc_result(), site_station_code == input$station_select)
    })

    # ------ Station info ------
    output$station_info <- renderUI({
      data <- selected_data()
      req(nrow(data) > 0)

      n_flags <- sum(
        data$edit_press_high, data$edit_press_low,
        data$edit_temp_range, data$edit_spike, data$edit_flatline,
        na.rm = TRUE
      )
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

    # ------ QAQC Plot ------
    # JS callback: intercepts drag in Select Range mode, resets zoom instantly
    # in the browser, and sends the captured range to Shiny.
    drag_range_js <- sprintf("
      function(el, x) {
        var _resetting = false;
        var _savedRange = null;

        el.on('plotly_relayout', function(evt) {
          if (_resetting) return;

          // Ignore relayout events that only update shapes (from plotlyProxy)
          var keys = Object.keys(evt);
          var isShapeOnly = keys.every(function(k) {
            return k === 'shapes' || k.indexOf('shapes[') === 0;
          });
          if (isShapeOnly) return;

          // Ignore autorange events (from restore or double-click reset)
          if (evt['xaxis.autorange'] || evt['yaxis.autorange']) {
            _savedRange = null;
            return;
          }

          var x0 = evt['xaxis.range[0]'];
          var x1 = evt['xaxis.range[1]'];
          if (!x0 || !x1) return;

          var mode = Shiny.shinyapp.$inputValues['%s'];
          if (mode !== 'select') {
            // In zoom mode, remember the current range
            _savedRange = { x0: x0, x1: x1 };
            return;
          }

          // Select mode: send the dragged range to Shiny
          Shiny.setInputValue('%s', {
            x0: String(x0), x1: String(x1), nonce: Math.random()
          });

          // Restore the view that was active before the drag
          _resetting = true;
          var restore = _savedRange
            ? { 'xaxis.range[0]': _savedRange.x0, 'xaxis.range[1]': _savedRange.x1 }
            : { 'xaxis.autorange': true };
          Plotly.relayout(el, restore).then(function() { _resetting = false; });
        });

        // Double-click resets zoom — clear saved range
        el.on('plotly_doubleclick', function() { _savedRange = null; });
      }
    ", session$ns("plot_mode"), session$ns("drag_range"))

    output$qaqc_plot <- renderPlotly({
      data <- selected_data()
      req(nrow(data) > 0)
      station <- input$station_select

      # Pressure panel
      p1 <- plot_ly(data, x = ~timestamp, y = ~airpress_kPa,
              type = "scattergl", mode = "lines",
              line = list(color = "#90CAF9", width = 1),
              name = "Raw Pressure",
              hovertemplate = "Raw: %{y:.2f} kPa<extra></extra>") %>%
        add_trace(y = ~airpress_kPa_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#1565C0", width = 1),
              name = "Adjusted Pressure",
              hovertemplate = "Adj: %{y:.2f} kPa<extra></extra>")

      # Pressure flag markers
      high_flags <- data[which(data$edit_press_high), ]
      low_flags  <- data[which(data$edit_press_low), ]
      spike_flags <- data[which(data$edit_spike), ]
      flat_flags  <- data[which(data$edit_flatline), ]

      if (nrow(high_flags) > 0) {
        p1 <- p1 %>% add_trace(data = high_flags, x = ~timestamp, y = ~airpress_kPa,
          type = "scatter", mode = "markers",
          marker = list(color = "#D32F2F", size = 5, symbol = "circle"),
          line = list(width = 0),
          name = "High Pressure",
          hovertemplate = "HIGH: %{y:.2f} kPa<extra></extra>")
      }
      if (nrow(low_flags) > 0) {
        p1 <- p1 %>% add_trace(data = low_flags, x = ~timestamp, y = ~airpress_kPa,
          type = "scatter", mode = "markers",
          marker = list(color = "#F57C00", size = 5, symbol = "circle"),
          line = list(width = 0),
          name = "Low Pressure",
          hovertemplate = "LOW: %{y:.2f} kPa<extra></extra>")
      }
      if (nrow(spike_flags) > 0) {
        p1 <- p1 %>% add_trace(data = spike_flags, x = ~timestamp, y = ~airpress_kPa,
          type = "scatter", mode = "markers",
          marker = list(color = "#7B1FA2", size = 5, symbol = "diamond"),
          line = list(width = 0),
          name = "Spike",
          hovertemplate = "SPIKE: %{y:.2f} kPa<extra></extra>")
      }
      if (nrow(flat_flags) > 0) {
        p1 <- p1 %>% add_trace(data = flat_flags, x = ~timestamp, y = ~airpress_kPa,
          type = "scatter", mode = "markers",
          marker = list(color = "#00897B", size = 5, symbol = "square"),
          line = list(width = 0),
          name = "Flatline",
          hovertemplate = "FLAT: %{y:.2f} kPa<extra></extra>")
      }

      # Temperature panel
      p2 <- plot_ly(data, x = ~timestamp, y = ~airtemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "#FFCC80", width = 1),
              name = "Raw Temperature",
              hovertemplate = "Raw: %{y:.1f} \u00b0C<extra></extra>") %>%
        add_trace(y = ~airtemp_C_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#E65100", width = 1),
              name = "Adjusted Temperature",
              hovertemplate = "Adj: %{y:.1f} \u00b0C<extra></extra>")

      temp_flags <- data[which(data$edit_temp_range), ]
      if (nrow(temp_flags) > 0) {
        p2 <- p2 %>% add_trace(data = temp_flags, x = ~timestamp, y = ~airtemp_C,
          type = "scatter", mode = "markers",
          marker = list(color = "#FBC02D", size = 5, symbol = "circle"),
          line = list(width = 0),
          name = "Temp Range",
          hovertemplate = "TEMP: %{y:.1f} \u00b0C<extra></extra>")
      }

      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = paste("Barometric QAQC -", station), font = list(size = 14)),
          xaxis = list(title = ""),
          xaxis2 = list(title = "Date"),
          yaxis = list(title = "Pressure (kPa)", fixedrange = TRUE),
          yaxis2 = list(title = "Temperature (\u00b0C)", fixedrange = TRUE),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15),
          margin = list(b = 80)
        ) %>%
        htmlwidgets::onRender(drag_range_js)
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

    output$pressure_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      n_dup <- sum(duplicated(data$timestamp))
      cat("Records:", nrow(data), " | Duplicate timestamps:", n_dup, "\n\n")
      cat("--- Raw ---\n")
      print(round(summary(data$airpress_kPa), 1))
      cat("\n--- Adjusted ---\n")
      print(round(summary(data$airpress_kPa_adj), 1))
    })

    output$temp_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      cat("--- Raw ---\n")
      print(round(summary(data$airtemp_C), 1))
      cat("\n--- Adjusted ---\n")
      print(round(summary(data$airtemp_C_adj), 1))
    })

    # ------ Multi-range selection via drag ------
    selected_ranges <- reactiveVal(list())
    range_history   <- reactiveVal(list())

    # Helper: parse plotly timestamp strings robustly
    # Plotly sends timestamps in various formats (with/without fractional seconds,
    # date-only at edges, or epoch ms). Try multiple parsers to handle all cases.
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

      # Save history for undo, add range, merge
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

    # Reset ranges on station change (plot re-renders from selected_data change)
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
      # Touch baro_qc_result to force renderPlotly (fresh widget, no shapes)
      df <- baro_qc_result()
      if (!is.null(df)) {
        attr(df, ".redraw") <- Sys.time()
        baro_qc_result(df)
      }
    })

    # Apply: open confirmation modal before removing selected ranges
    observeEvent(input$apply_ranges, {
      ranges <- selected_ranges()
      req(length(ranges) > 0)

      n <- length(ranges)
      showModal(modalDialog(
        title = sprintf("Remove %d Selected Period%s", n, if (n > 1) "s" else ""),
        tags$p(
          sprintf("Station: %s", input$station_select)
        ),
        tags$ul(
          lapply(ranges, function(r) {
            tags$li(paste(r[1], "to", r[2]))
          })
        ),
        tags$div(
          tags$label("Note:"),
          tags$input(type = "text", id = session$ns("modal_note"),
            value = "disturbance", readonly = "readonly",
            class = "form-control",
            style = "background-color: #e9ecef; cursor: not-allowed;")
        ),
        checkboxInput(session$ns("modal_keep_temp"),
          "Keep air temperature (keep_temp = TRUE)", value = FALSE),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("modal_confirm"), "Confirm Removal",
            class = "btn-warning", icon = icon("eraser"),
            style = "color: #ffffff;")
        ),
        easyClose = TRUE
      ))
    })

    # Confirm: apply adjust_logger_NA to all selected ranges and close modal
    observeEvent(input$modal_confirm, {
      ranges <- selected_ranges()
      req(length(ranges) > 0)
      qc_data <- baro_qc_result()
      req(qc_data)

      station <- input$station_select
      req(station)

      note <- input$modal_note
      if (is.null(note) || nchar(trimws(note)) == 0) {
        showNotification("Enter a note describing the reason for removal.",
          type = "error", duration = 3)
        return()
      }

      removeModal()

      tryCatch({
        station_data <- dplyr::filter(qc_data, site_station_code == station)
        other_data   <- dplyr::filter(qc_data, site_station_code != station)

        # Snap range endpoints to nearest actual timestamps in data
        snapped <- snap_ranges(ranges, station_data$timestamp)

        for (r in snapped) {
          station_data <- waterlogged::adjust_logger_NA(
            input_data        = station_data,
            select_station    = station,
            metric            = "BARO",
            reason_to_adjust  = "disturbance",
            timestamp_start   = r[1],
            timestamp_end     = r[2],
            keep_temp         = input$modal_keep_temp,
            manual_note       = paste0(note, " (", r[1], " to ", r[2], ")"),
            log_root          = NULL
          )
        }

        updated <- dplyr::bind_rows(other_data, station_data) %>%
          dplyr::arrange(site_station_code, timestamp)

        # Clear range state and update data — baro_qc_result change is the
        # sole trigger for renderPlotly (creates a fresh widget, no shapes)
        selected_ranges(list())
        range_history(list())
        baro_qc_result(updated)
        rv_qaqc_data$baro_qaqc <- updated

        n <- length(ranges)
        showNotification(
          sprintf("Removed %d selected period%s for %s", n, if (n > 1) "s" else "", station),
          type = "message", duration = 5
        )

      }, error = function(e) {
        showNotification(paste("Adjustment error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
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


    # ------ Download: Clean CSV ------
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("baro_qaqc_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- baro_qc_result()
        req(df)

        # Select relevant columns for export
        export_cols <- c(
          "site_station_code", "timestamp",
          "airpress_kPa", "airpress_kPa_adj",
          "airtemp_C", "airtemp_C_adj"
        )
        # Add any edit/flag columns
        flag_cols <- grep("^(edit_|flag_)", names(df), value = TRUE)
        export_cols <- intersect(c(export_cols, flag_cols), names(df))

        export_df <- df[, export_cols]
        # Force consistent timestamp format (midnight would drop 00:00:00)
        export_df$timestamp <- format(export_df$timestamp, "%Y-%m-%d %H:%M:%S")
        write.csv(export_df, file, row.names = FALSE)
      }
    )

    # ------ Download: QAQC Log ------
    output$download_log <- downloadHandler(
      filename = function() {
        paste0("baro_qaqc_log_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        log <- qaqc_log()
        if (!is.null(log) && nrow(log) > 0) {
          write.csv(log, file, row.names = FALSE)
        } else {
          # Write empty log with expected columns
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
        paste0("baro_station_metadata_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        meta <- rv_raw_data$metadata
        if (!is.null(meta) && nrow(meta) > 0) {
          baro_meta <- meta[meta$metric %in% c("barometric", "baro"), ]
          write.csv(baro_meta, file, row.names = FALSE)
        } else {
          write.csv(data.frame(), file, row.names = FALSE)
        }
      }
    )

  })
}
