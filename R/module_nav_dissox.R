# =============================================================================
# Dissolved Oxygen Workflow Modules — Full Implementation
# =============================================================================
#
# Pipeline for HOBO U26 dissolved oxygen loggers:
#   1. Import Data        — bind_hobo_files() + resample_timestamps()
#   2. Add Nearest Baro   — add_nearest_baro(metric="DO")
#   3. Convert Metrics    — convert_do_mgl_percsat()
#   4. QC & Export        — qc_dissox() + wq_drift_correction() + adjust_logger_NA() + export
#
# Pre-loaded with demo data from inst/extdata/ (COOK_WE_40).
#
# Data flow:
#   rv_raw_data$do_data (named list, demo)
#     -> rv_bound_data$do_bound (combined DF, import tab)
#       -> rv_bound_data$do_with_baro (with baro joined, baro tab)
#         -> rv_converted_data$do_converted (with %sat, convert tab)
#           -> rv_qaqc_data$do_qaqc (QC'd DF, QC tab)
# =============================================================================


# =============================================================================
# 1. Import Data — UI
# =============================================================================
module_do_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Import Dissolved Oxygen Data", class = "custom-h1"),
      tags$h2("Upload raw HOBO U26 DO logger files and align timestamps",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Import raw HOBO CSV files from dissolved oxygen loggers ",
        "(HOBO U26). These loggers record DO concentration (mg/L) and water ",
        "temperature (\u00b0C). Barometric pressure data is required in the ",
        "next step for percent saturation conversion."
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(style = "margin: 0;",
          tags$strong("Important:"), " Process and QC your ",
          tags$strong("barometric"), " data first. It is needed to convert ",
          "DO from mg/L to percent saturation (%sat) and for dry-sensor detection."
        )
      ),

      hr(),

      # ---- Data status ----
      uiOutput(ns("data_status")),

      hr(),

      fluidRow(
        # ---- Left info panel ----
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("About HOBO U26 DO Loggers", style = "margin-top: 0;"),
            tags$p(
              "The HOBO U26 uses an optical fluorescence sensor to measure ",
              "dissolved oxygen. It records concentration in mg/L directly. ",
              "To get percent saturation, atmospheric pressure must be added ",
              "in the Convert step."
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Optical DO sensor (not electrochemical)"),
              tags$li("Records DO mg/L + water temperature"),
              tags$li("Requires baro data for %sat conversion"),
              tags$li("Field references used for drift correction")
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Functions Used", style = "margin-top: 0;"),
            tags$code("bind_hobo_files()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Parse raw HOBO CSVs, match serial numbers to metadata"),
            tags$code("resample_timestamps()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Snap timestamps to regular grid for baro join")
          ),

          tags$div(
            style = "background-color: #fff3e0; padding: 15px; border-radius: 5px;",
            tags$h4("Columns Produced", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px; margin-bottom: 0;",
              tags$li(tags$code("site_station_code"), " \u2014 station ID"),
              tags$li(tags$code("timestamp"), " \u2014 date-time"),
              tags$li(tags$code("do_mgl"), " \u2014 DO concentration (mg/L)"),
              tags$li(tags$code("watertemp_C"), " \u2014 water temperature (\u00b0C)"),
              tags$li(tags$code("sn"), " \u2014 logger serial number"),
              tags$li(tags$code("metric"), " \u2014 \"dissolvedoxygen\"")
            )
          )
        ),

        # ---- Right main panel ----
        column(
          width = 8,

          # Metadata upload
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0;",
            tags$h4("Metadata", style = "margin-top: 0;"),
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

          # Sample metadata preview
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; overflow-x: auto;",
            tags$h4("Sample Metadata (DO entries)", style = "margin-top: 0;"),
            tags$pre(
              style = "font-size: 11px; margin: 0; white-space: pre; overflow-x: auto;",
"site_station_code,timestamp_deploy,timestamp_remove,model,sn,metric,status,latitude,longitude
GOOS_WE_30,4/14/2025 12:00,10/21/2025 16:00,HOBO_U26,21155021,dissolvedoxygen,logging,49.347,-123.000633
COOK_WE_40,4/14/2025 12:00,10/21/2025 17:00,HOBO_U26,21473863,dissolvedoxygen,logging,49.33973,-123.001504"
            )
          ),

          # Station map
          tags$div(
            style = "margin-bottom: 0;",
            tags$h4("Station Locations", style = "margin-top: 0;"),
            leafletOutput(ns("station_map"), height = "280px")
          ),

          # ---- Visual separator between metadata and raw files ----
          tags$div(
            style = "border-top: 2px solid #2e7d32; margin: 25px 0 20px 0; position: relative;",
            tags$span(
              style = "position: absolute; top: -10px; left: 20px; background: white; padding: 0 10px; font-size: 12px; color: #2e7d32; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;",
              "Raw Logger Data"
            )
          ),

          # Raw HOBO files upload
          tags$div(
            style = "background-color: #fafafa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #e0e0e0;",
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
              width = "100%"
            )
          ),

          hr(),

          # ---- Snap Timestamps ----
          tags$div(
            style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Snap Logger Timestamps (Optional)", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "Round timestamps to a regular interval so DO data aligns ",
              "with barometric data for the pressure join. Original ",
              "timestamps preserved in ", tags$code("timestamp_raw"), "."
            ),
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
              icon = icon("clock")
            )
          ),

          hr(),

          # ---- Import Summary ----
          tags$h4("Import Summary"),
          DT::DTOutput(ns("import_summary")),

          tags$div(style = "margin-top: 15px;"),

          # ---- Preview Plot ----
          tags$h4("Data Preview"),
          plotlyOutput(ns("preview_plot"), height = "650px") %>% withSpinner()
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

    # ------ Current DO data reactive ------
    do_current <- reactive({
      rv_bound_data$do_bound
    })

    # ------ Data status display ------
    output$data_status <- renderUI({
      df <- do_current()
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px;",
          icon("info-circle"),
          " No DO data loaded. Upload files above or use pre-loaded sample data."
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

      do_meta <- metadata[metadata$metric == "dissolvedoxygen", ]
      do_sites <- unique(do_meta[, c("site_station_code", "latitude", "longitude")])
      do_sites <- do_sites[!is.na(do_sites$latitude) & !is.na(do_sites$longitude), ]

      req(nrow(do_sites) > 0)

      leaflet(do_sites) %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = 7, weight = 2,
          color = "#fff", fillColor = "#2e7d32", fillOpacity = 0.9,
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

    # ------ Update station selector for snap ------
    observe({
      df <- do_current()
      if (!is.null(df)) {
        stations <- unique(df$site_station_code)
        choices <- c("All Stations" = "all", setNames(stations, stations))
        updateSelectInput(session, "snap_station", choices = choices)
      }
    })

    # ------ Bind & Import button ------
    observeEvent(input$bind_btn, {
      meta_file <- input$meta_file
      raw_files <- input$raw_files

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

      if (!is.null(raw_files)) {
        tryCatch({
          showNotification("Binding HOBO files...", type = "message",
            duration = NULL, id = "do_bind_progress")

          result <- waterlogged::bind_hobo_files(
            file_paths = raw_files$datapath,
            metadata = metadata
          )

          bound_data <- result[[1]]
          rv_bound_data$do_bound <- bound_data

          if (!is.null(meta_file)) {
            rv_raw_data$metadata <- metadata
          }

          removeNotification("do_bind_progress")
          n_rows <- format(nrow(bound_data), big.mark = ",")
          n_stns <- length(unique(bound_data$site_station_code))
          showNotification(
            sprintf("Import complete: %s records from %d station(s)", n_rows, n_stns),
            type = "message", duration = 5
          )
        }, error = function(e) {
          removeNotification("do_bind_progress")
          showNotification(paste("Import error:", conditionMessage(e)),
            type = "error", duration = 8)
        })
      } else {
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

    # ------ Snap timestamps button ------
    observeEvent(input$snap_btn, {
      df <- do_current()
      req(df)

      tryCatch({
        showNotification("Snapping timestamps...", type = "message",
          duration = NULL, id = "do_snap_progress")

        station_arg <- if (input$snap_station == "all") NULL else input$snap_station

        result <- waterlogged::resample_timestamps(
          input_data = df,
          interval = input$snap_interval,
          align = input$snap_align,
          select_station = station_arg,
          metric = "DO"
        )

        rv_bound_data$do_bound <- result

        removeNotification("do_snap_progress")
        showNotification("Timestamps snapped.", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("do_snap_progress")
        showNotification(paste("Snap error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
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

    # ------ Preview plot ------
    output$preview_plot <- renderPlotly({
      df <- do_current()
      req(df, nrow(df) > 0)

      stations <- unique(df$site_station_code)

      # DO panel
      p1 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(df, site_station_code == stn)
        p1 <- p1 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~do_mgl,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- DO"),
          hovertemplate = paste0(stn, ": %{y:.2f} mg/L<extra></extra>")
        )
      }

      # Temperature panel
      p2 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(df, site_station_code == stn)
        p2 <- p2 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~watertemp_C,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- Temp"),
          showlegend = FALSE,
          hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
        )
      }

      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "Dissolved Oxygen Data Preview", font = list(size = 14)),
          xaxis2 = list(title = "Date"),
          yaxis = list(title = "DO (mg/L)"),
          yaxis2 = list(title = "Water Temp (\u00b0C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15),
          margin = list(b = 80)
        )
    })

  })
}


# =============================================================================
# 2. Add Nearest Baro — UI
# =============================================================================
module_do_baro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Add Nearest Barometric Station", class = "custom-h1"),
      tags$h2("Attach barometric pressure data to DO records",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Attach barometric pressure data to each DO timestamp. ",
        "Atmospheric pressure is needed for the Weiss equation that ",
        "calculates percent saturation in the next step. Air temperature ",
        "also helps detect dry-sensor conditions."
      ),

      # ---- Data readiness ----
      uiOutput(ns("baro_status")),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Why Baro for DO?", style = "margin-top: 0;"),
            tags$p(
              "Oxygen saturation depends on the partial pressure of oxygen in ",
              "the atmosphere. Atmospheric pressure is needed for the Weiss ",
              "equation that calculates %sat."
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "Air temperature also helps detect dry-sensor conditions ",
              "(when water temp converges with air temp)."
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("add_nearest_baro()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Matches each DO station to the nearest barometric station(s) by GPS ",
                   "distance and joins baro pressure/temperature by timestamp. Outputs ",
                   "the data frame with added ", tags$code("airpress_kPa"), ", ",
                   tags$code("airtemp_C"), ", and ", tags$code("baro_site_stn_code"),
                   " columns.")
          )
        ),
        column(
          width = 8,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            fluidRow(
              column(4,
                selectInput(ns("baro_selection"), "Baro Station:",
                  choices = c("Auto (nearest)" = "auto"),
                  width = "100%"
                )
              ),
              column(4,
                numericInput(ns("max_km"), "Max Distance (km):",
                  value = 30, min = 1, max = 200, step = 5)
              ),
              column(4,
                tags$div(
                  style = "margin-top: 25px;",
                  actionButton(ns("add_baro_btn"), "Add Baro Data",
                    class = "btn-primary btn-lg", width = "100%",
                    icon = icon("cloud")
                  )
                )
              )
            )
          ),

          # Baro match results (conditional)
          conditionalPanel(
            condition = "output.show_baro_results",
            ns = ns,
            tags$div(
              style = "margin-bottom: 15px;",
              uiOutput(ns("baro_match_info")),
              DT::DTOutput(ns("baro_summary"))
            )
          )
        )
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 2. Add Nearest Baro — Server
# =============================================================================
module_do_baro_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data   <- session$userData$rv_raw_data
    rv_bound_data <- session$userData$rv_bound_data
    rv_qaqc_data  <- session$userData$rv_qaqc_data

    # Local reactive for step tracking
    baro_added <- reactiveVal(FALSE)

    # Conditional panel visibility
    output$show_baro_results <- reactive({ baro_added() })
    outputOptions(output, "show_baro_results", suspendWhenHidden = FALSE)

    # ------ Data readiness ------
    output$baro_status <- renderUI({
      df <- rv_bound_data$do_bound
      baro <- rv_qaqc_data$baro_qaqc
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No DO data available. Import data in the Import tab first."
        )
      } else if (is.null(baro) || nrow(baro) == 0) {
        n_stns <- length(unique(df$site_station_code))
        n_recs <- format(nrow(df), big.mark = ",")
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          sprintf(" DO data ready (%d station(s), %s records) but no QC'd barometric data found. Process baro data first, or add baro manually.", n_stns, n_recs)
        )
      } else {
        n_stns <- length(unique(df$site_station_code))
        n_recs <- format(nrow(df), big.mark = ",")
        n_baro <- length(unique(baro$site_station_code))
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Ready: %d DO station(s), %s records | %d baro station(s) available",
                  n_stns, n_recs, n_baro)
        )
      }
    })

    # ------ Update baro station selector ------
    observe({
      baro <- rv_qaqc_data$baro_qaqc
      if (!is.null(baro)) {
        baro_stns <- unique(baro$site_station_code)
        choices <- c("Auto (nearest)" = "auto", setNames(baro_stns, baro_stns))
        updateSelectInput(session, "baro_selection", choices = choices)
      }
    })

    # ------ Add Baro button ------
    observeEvent(input$add_baro_btn, {
      do_data <- rv_bound_data$do_bound
      baro_data <- rv_qaqc_data$baro_qaqc
      metadata <- rv_raw_data$metadata

      if (is.null(do_data) || nrow(do_data) == 0) {
        showNotification("No DO data. Import data first.", type = "error", duration = 5)
        return()
      }
      if (is.null(metadata)) {
        showNotification("No metadata available.", type = "error", duration = 5)
        return()
      }

      tryCatch({
        showNotification("Adding barometric data...", type = "message",
          duration = NULL, id = "do_baro_progress")

        baro_sel <- if (input$baro_selection == "auto") "auto" else input$baro_selection

        result <- waterlogged::add_nearest_baro(
          input_data = do_data,
          metric = "DO",
          max_km = input$max_km,
          baro_site_selection = baro_sel,
          metadata = metadata,
          baro_data = baro_data
        )

        rv_bound_data$do_with_baro <- result[[1]]
        baro_added(TRUE)

        removeNotification("do_baro_progress")
        showNotification("Barometric data added successfully.", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("do_baro_progress")
        showNotification(paste("Baro join error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Baro match info ------
    output$baro_match_info <- renderUI({
      df <- rv_bound_data$do_with_baro
      req(df)
      n_with_baro <- sum(!is.na(df$airpress_kPa))
      n_total <- nrow(df)
      pct <- round(100 * n_with_baro / n_total, 1)
      baro_stns <- if ("baro_site_stn_code" %in% names(df)) {
        paste(unique(na.omit(df$baro_site_stn_code)), collapse = ", ")
      } else {
        "unknown"
      }

      tags$div(
        style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
        icon("check-circle"),
        sprintf(" Baro coverage: %s/%s records (%.1f%%) | Matched station(s): %s",
                format(n_with_baro, big.mark = ","),
                format(n_total, big.mark = ","),
                pct, baro_stns)
      )
    })

    # ------ Baro summary table ------
    output$baro_summary <- DT::renderDT({
      df <- rv_bound_data$do_with_baro
      req(df, "airpress_kPa" %in% names(df))

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `Baro Coverage` = paste0(round(100 * sum(!is.na(airpress_kPa)) / dplyr::n(), 1), "%"),
          `Press Min (kPa)` = round(min(airpress_kPa, na.rm = TRUE), 2),
          `Press Max (kPa)` = round(max(airpress_kPa, na.rm = TRUE), 2),
          `Air Temp Min (C)` = round(min(airtemp_C, na.rm = TRUE), 1),
          `Air Temp Max (C)` = round(max(airtemp_C, na.rm = TRUE), 1),
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

  })
}


# =============================================================================
# 3. Convert Data Metrics — UI
# =============================================================================
module_do_convert_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Convert Data Metrics", class = "custom-h1"),
      tags$h2("Convert DO from mg/L to percent air saturation",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Use barometric pressure (added in the previous step) along with ",
        "water temperature to convert DO from mg/L to percent air ",
        "saturation (%sat). Percent saturation accounts for altitude and ",
        "temperature, making it the preferred metric for comparison."
      ),

      # ---- Data readiness ----
      uiOutput(ns("convert_status")),

      hr(),

      tags$h3("Convert DO to Percent Saturation",
              style = "color: #2e7d32; margin-bottom: 15px;"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("The Weiss Equation", style = "margin-top: 0;"),
            tags$p(
              "Using the Weiss (1970/1974) oxygen solubility equation:"
            ),
            tags$ol(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Calculate theoretical max DO at the given temp and pressure"),
              tags$li("Divide measured DO by the theoretical max"),
              tags$li("Multiply by 100 to get percent")
            ),
            tags$p(style = "font-size: 13px; margin-top: 10px; color: #666;",
              "~100% = well-aerated. >100% = supersaturation (algae). ",
              "<50% = oxygen depletion."
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("convert_do_mgl_percsat()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Converts dissolved oxygen from mg/L to percent air saturation using ",
                   "the Weiss equation with water temperature and barometric pressure. ",
                   "Outputs the data frame with a new ", tags$code("do_percsat"), " column.")
          )
        ),
        column(
          width = 8,
          actionButton(ns("convert_btn"), "Convert to % Saturation",
            class = "btn-success btn-lg", width = "100%",
            icon = icon("play")
          ),

          # Convert results (conditional)
          conditionalPanel(
            condition = "output.show_convert_results",
            ns = ns,

            tags$div(style = "margin-top: 15px;"),
            tags$h4("Conversion Summary"),
            DT::DTOutput(ns("convert_summary")),

            tags$div(style = "margin-top: 15px;"),
            tags$h4("DO Conversion Plot"),
            plotlyOutput(ns("convert_plot"), height = "650px") %>% withSpinner()
          )
        )
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 3. Convert Data Metrics — Server
# =============================================================================
module_do_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_bound_data     <- session$userData$rv_bound_data
    rv_converted_data <- session$userData$rv_converted_data

    # Local reactive for step tracking
    convert_done <- reactiveVal(FALSE)

    # Conditional panel visibility
    output$show_convert_results <- reactive({ convert_done() })
    outputOptions(output, "show_convert_results", suspendWhenHidden = FALSE)

    # ------ Data readiness ------
    output$convert_status <- renderUI({
      df <- rv_bound_data$do_with_baro
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No DO data with barometric pressure available. Complete the Add Nearest Baro step first."
        )
      } else if (!"airpress_kPa" %in% names(df)) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " DO data found but missing airpress_kPa column. Run Add Nearest Baro first."
        )
      } else {
        n_stns <- length(unique(df$site_station_code))
        n_recs <- format(nrow(df), big.mark = ",")
        pct_baro <- round(100 * sum(!is.na(df$airpress_kPa)) / nrow(df), 1)
        tags$div(
          style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("check-circle"),
          sprintf(" Ready: %d station(s), %s records, %.1f%% baro coverage",
                  n_stns, n_recs, pct_baro)
        )
      }
    })

    # ------ Convert to %sat button ------
    observeEvent(input$convert_btn, {
      df <- rv_bound_data$do_with_baro

      if (is.null(df) || nrow(df) == 0) {
        showNotification("Add barometric data first (Add Nearest Baro tab).", type = "error", duration = 5)
        return()
      }

      if (!"airpress_kPa" %in% names(df)) {
        showNotification("No airpress_kPa column. Run Add Nearest Baro first.", type = "error", duration = 5)
        return()
      }

      tryCatch({
        showNotification("Converting to % saturation...", type = "message",
          duration = NULL, id = "do_convert_progress")

        result <- waterlogged::convert_do_mgl_percsat(
          input_data = df,
          path_to_output_folder = NULL
        )

        rv_converted_data$do_converted <- result
        convert_done(TRUE)

        removeNotification("do_convert_progress")
        showNotification("DO conversion complete!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("do_convert_progress")
        showNotification(paste("Conversion error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Conversion summary table ------
    output$convert_summary <- DT::renderDT({
      df <- rv_converted_data$do_converted
      req(df, "do_percsat" %in% names(df))

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `DO Min (mg/L)` = round(min(do_mgl, na.rm = TRUE), 2),
          `DO Max (mg/L)` = round(max(do_mgl, na.rm = TRUE), 2),
          `DO Mean (mg/L)` = round(mean(do_mgl, na.rm = TRUE), 2),
          `%sat Min` = round(min(do_percsat, na.rm = TRUE), 1),
          `%sat Max` = round(max(do_percsat, na.rm = TRUE), 1),
          `%sat Mean` = round(mean(do_percsat, na.rm = TRUE), 1),
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ------ Conversion plot ------
    output$convert_plot <- renderPlotly({
      df <- rv_converted_data$do_converted
      req(df, nrow(df) > 0, "do_percsat" %in% names(df))

      stations <- unique(df$site_station_code)

      # DO mg/L panel
      p1 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(df, site_station_code == stn)
        p1 <- p1 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~do_mgl,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- DO mg/L"),
          hovertemplate = paste0(stn, ": %{y:.2f} mg/L<extra></extra>")
        )
      }

      # DO %sat panel
      p2 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(df, site_station_code == stn)
        p2 <- p2 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~do_percsat,
          type = "scattergl", mode = "lines",
          line = list(width = 1, color = "#2e7d32"),
          name = paste(stn, "- %sat"),
          hovertemplate = paste0(stn, ": %{y:.1f} %sat<extra></extra>")
        )
      }

      # Water temp panel
      p3 <- plot_ly()
      for (stn in stations) {
        stn_data <- dplyr::filter(df, site_station_code == stn)
        p3 <- p3 %>% add_trace(
          data = stn_data, x = ~timestamp, y = ~watertemp_C,
          type = "scattergl", mode = "lines",
          line = list(width = 1),
          name = paste(stn, "- Temp"),
          showlegend = FALSE,
          hovertemplate = paste0(stn, ": %{y:.1f} \u00b0C<extra></extra>")
        )
      }

      subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "DO Conversion Results", font = list(size = 14)),
          xaxis3 = list(title = "Date"),
          yaxis = list(title = "DO (mg/L)"),
          yaxis2 = list(title = "DO (%sat)"),
          yaxis3 = list(title = "Water Temp (\u00b0C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.1),
          margin = list(b = 80)
        )
    })

  })
}


# =============================================================================
# 4. QC & Export — UI
# =============================================================================
module_do_qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("QC Dissolved Oxygen", class = "custom-h1"),
      tags$h2("Automated quality control, drift correction, and export",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Run automated QAQC on dissolved oxygen data using ",
        tags$code("qc_dissox()"), ". This flags sensor errors, negative values, ",
        "high values, ice, and dry periods. Optionally apply drift correction ",
        "using field reference data."
      ),

      # ---- Data readiness ----
      uiOutput(ns("qc_status")),

      hr(),

      # ======== Section A: Run QAQC ========
      tags$h3("Run DO QAQC", class = "custom-h2"),
      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Automatic QAQC Rules", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("do_error"), " \u2014 sensor error values"),
              tags$li(tags$strong("do_neg_between"), " \u2014 DO between -1 and 0 mg/L (corrected to 0)"),
              tags$li(tags$strong("do_neg_lt_minus1"), " \u2014 DO < -1 mg/L (set to NA)"),
              tags$li(tags$strong("do_high"), " \u2014 DO unreasonably high"),
              tags$li(tags$strong("temp_flag_ice"), " \u2014 water temp < 0.1\u00b0C"),
              tags$li(tags$strong("temp_neg_between"), " \u2014 temp -1 to 0\u00b0C (corrected to 0)"),
              tags$li(tags$strong("temp_neg_leq_minus1"), " \u2014 temp \u2264 -1\u00b0C (set to NA)"),
              tags$li(tags$strong("flag_do_dry"), " \u2014 dry sensor detection")
            ),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 10px;",
              "No user-tunable parameters \u2014 thresholds are built into the function."
            )
          ),
          tags$div(
            style = "background-color: #fff3cd; padding: 15px; border-radius: 5px;",
            tags$h4("Output Columns", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 12px; margin-bottom: 0;",
              tags$li(tags$code("do_mgl_adj"), " \u2014 adjusted DO (mg/L)"),
              tags$li(tags$code("do_percsat_adj"), " \u2014 adjusted DO (%sat)"),
              tags$li(tags$code("watertemp_C_do_adj"), " \u2014 adjusted water temp"),
              tags$li("8 flag columns (see left)")
            )
          )
        ),

        column(
          width = 8,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            fluidRow(
              column(6,
                selectInput(ns("qc_station"), "Station to QC:",
                  choices = NULL, width = "100%"
                )
              ),
              column(6,
                tags$div(
                  style = "margin-top: 25px;",
                  actionButton(ns("run_qaqc"), "Run DO QAQC",
                    class = "btn-success btn-lg", width = "100%",
                    icon = icon("play")
                  )
                )
              )
            ),
            tags$p(style = "font-size: 12px; color: #666;",
              "QAQC is run one station at a time using ", tags$code("qc_dissox()"), "."
            )
          )
        )
      ),

      hr(),

      # ======== Section B: Results (conditional) ========
      conditionalPanel(
        condition = "output.show_results",
        ns = ns,

        # ---- Flag Summary ----
        tags$h3("QAQC Results", class = "custom-h2"),
        tags$h4("Flag Summary"),
        DT::DTOutput(ns("flag_summary")),

        # Flag legend
        tags$div(
          style = "display: flex; gap: 8px; margin: 15px 0; flex-wrap: wrap;",
          tags$div(style = "padding: 5px 10px; background: #28a745; color: white; border-radius: 4px; font-size: 12px;", "Good"),
          tags$div(style = "padding: 5px 10px; background: #D32F2F; color: white; border-radius: 4px; font-size: 12px;", "DO Error"),
          tags$div(style = "padding: 5px 10px; background: #F57C00; color: white; border-radius: 4px; font-size: 12px;", "DO Negative"),
          tags$div(style = "padding: 5px 10px; background: #FBC02D; color: black; border-radius: 4px; font-size: 12px;", "DO High"),
          tags$div(style = "padding: 5px 10px; background: #1565C0; color: white; border-radius: 4px; font-size: 12px;", "Ice"),
          tags$div(style = "padding: 5px 10px; background: #795548; color: white; border-radius: 4px; font-size: 12px;", "Dry Sensor")
        ),

        hr(),

        # ---- Station Detail ----
        tags$h3("Station Detail", class = "custom-h2"),
        tags$div(
          style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
          fluidRow(
            column(4,
              selectInput(ns("station_select"), "Select Station:",
                choices = NULL, width = "100%")
            ),
            column(8, uiOutput(ns("station_info")))
          )
        ),

        fluidRow(
          column(4,
            tags$h4("DO Summary (raw vs adjusted)"),
            verbatimTextOutput(ns("do_summary")),
            tags$h4("Temperature Summary"),
            verbatimTextOutput(ns("temp_summary"))
          ),
          column(8,
            plotlyOutput(ns("qaqc_plot"), height = "650px") %>% withSpinner()
          )
        ),

        hr(),

        # ---- Drift Correction ----
        tags$h3("Drift Correction (Optional)", class = "custom-h2"),
        tags$div(
          style = "border-left: 4px solid #1565c0; padding: 15px 20px; margin-bottom: 20px; background: #f8f9fa; border-radius: 0 5px 5px 0;",

          tags$p(style = "font-size: 13px;",
            "Use ", tags$code("wq_drift_correction()"), " with field reference ",
            "data to correct sensor drift in DO readings over time."
          ),

          fluidRow(
            column(3,
              selectInput(ns("drift_station"), "Station:",
                choices = NULL, width = "100%")
            ),
            column(3,
              selectInput(ns("drift_method"), "Method:",
                choices = c("Ratio (multiplicative)" = "ratio",
                            "Offset (additive)" = "offset"),
                selected = "ratio", width = "100%"
              )
            ),
            column(3,
              numericInput(ns("drift_max_gap"), "Max Ref Gap (hours):",
                value = 1, min = 0.5, max = 24, step = 0.5)
            ),
            column(3,
              tags$div(
                style = "margin-top: 25px;",
                actionButton(ns("drift_btn"), "Apply Drift Correction",
                  class = "btn-info", icon = icon("chart-line"))
              )
            )
          ),

          conditionalPanel(
            condition = "output.show_drift_result",
            ns = ns,
            uiOutput(ns("drift_result_info"))
          )
        ),

        hr(),

        # ---- Manual Adjustments ----
        tags$h3("Manual Adjustments", class = "custom-h2"),
        tags$div(
          style = "border-left: 4px solid #e65100; padding: 15px 20px; margin-bottom: 20px; background: #f8f9fa; border-radius: 0 5px 5px 0;",

          tags$p(style = "font-size: 13px;",
            "Use ", tags$code("adjust_logger_NA()"), " to manually set ",
            "DO adjusted values to NA for bad periods."
          ),

          fluidRow(
            column(3,
              selectInput(ns("adj_station"), "Station:",
                choices = NULL, width = "100%")
            ),
            column(2,
              selectInput(ns("adj_reason"), "Reason:",
                choices = c("Ice" = "ice", "Dry" = "dry", "Disturbance" = "disturbance"),
                width = "100%"
              )
            ),
            column(3,
              textInput(ns("adj_start"), "Start Timestamp:",
                placeholder = "YYYY-MM-DD HH:MM:SS")
            ),
            column(2,
              textInput(ns("adj_end"), "End Timestamp:",
                placeholder = "YYYY-MM-DD HH:MM:SS")
            ),
            column(2,
              textInput(ns("adj_note"), "Note:",
                placeholder = "Reason")
            )
          ),
          fluidRow(
            column(6,
              checkboxInput(ns("adj_keep_temp"), "Keep water temperature (keep_temp = TRUE)",
                value = FALSE)
            ),
            column(6,
              actionButton(ns("adj_btn"), "Remove Period",
                class = "btn-warning", icon = icon("eraser"))
            )
          )
        ),

        hr(),

        # ---- Export ----
        tags$h3("Export Clean Data", class = "custom-h2"),
        tags$p(style = "font-size: 13px; color: #666;",
          "Download the QC'd dissolved oxygen data as CSV. Includes raw ",
          "columns, adjusted columns, and all flag/edit columns."
        ),
        fluidRow(
          column(4,
            downloadButton(ns("download_csv"), "Download Clean CSV",
              class = "btn-primary", style = "width: 100%;")
          ),
          column(4,
            downloadButton(ns("download_log"), "Download QAQC Log",
              class = "btn-info", style = "width: 100%;")
          )
        )
      )
    ),
    .wireframe_footer()
  )
}


# =============================================================================
# 4. QC & Export — Server
# =============================================================================
module_do_qc_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv_raw_data       <- session$userData$rv_raw_data
    rv_converted_data <- session$userData$rv_converted_data
    rv_qaqc_data      <- session$userData$rv_qaqc_data

    # Local reactives
    do_qc_result <- reactiveVal(NULL)
    qaqc_log <- reactiveVal(NULL)
    drift_applied <- reactiveVal(FALSE)

    # Conditional panel visibility
    output$show_results <- reactive({ !is.null(do_qc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    output$show_drift_result <- reactive({ drift_applied() })
    outputOptions(output, "show_drift_result", suspendWhenHidden = FALSE)

    # ------ Data readiness ------
    output$qc_status <- renderUI({
      df <- rv_converted_data$do_converted
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          " No converted DO data available. Complete the Convert tab first (add baro + convert to %sat)."
        )
      } else {
        has_percsat <- "do_percsat" %in% names(df)
        n_stns <- length(unique(df$site_station_code))
        n_recs <- format(nrow(df), big.mark = ",")
        if (has_percsat) {
          tags$div(
            style = "color: #155724; background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
            icon("check-circle"),
            sprintf(" Ready: %d station(s), %s records with DO mg/L + %%sat",
                    n_stns, n_recs)
          )
        } else {
          tags$div(
            style = "color: #856404; background: #fff3cd; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
            icon("exclamation-triangle"),
            sprintf(" DO data has %s records but no do_percsat column. Run conversion first.", n_recs)
          )
        }
      }
    })

    # ------ Update station selectors ------
    observe({
      df <- rv_converted_data$do_converted
      if (!is.null(df)) {
        stations <- unique(df$site_station_code)
        updateSelectInput(session, "qc_station", choices = stations, selected = stations[1])
      }
    })

    # ------ Run QAQC ------
    observeEvent(input$run_qaqc, {
      df <- rv_converted_data$do_converted
      station <- input$qc_station

      if (is.null(df) || nrow(df) == 0) {
        showNotification("No converted DO data. Complete the Convert tab first.",
          type = "error", duration = 5)
        return()
      }
      if (is.null(station) || station == "") {
        showNotification("Select a station.", type = "error", duration = 3)
        return()
      }

      tryCatch({
        showNotification("Running DO QAQC...", type = "message",
          duration = NULL, id = "do_qaqc_progress")

        result <- waterlogged::qc_dissox(
          input_data = df,
          select_station = station,
          log_root = NULL,
          user = NULL
        )

        # If we already have QC results for other stations, merge
        existing <- do_qc_result()
        if (!is.null(existing)) {
          other_data <- dplyr::filter(existing, site_station_code != station)
          station_result <- dplyr::filter(result, site_station_code == station)
          combined <- dplyr::bind_rows(other_data, station_result) %>%
            dplyr::arrange(site_station_code, timestamp)
          do_qc_result(combined)
          rv_qaqc_data$do_qaqc <- combined
        } else {
          do_qc_result(result)
          rv_qaqc_data$do_qaqc <- result
        }

        qaqc_log(attr(result, "qaqc_log"))

        removeNotification("do_qaqc_progress")
        showNotification(
          sprintf("DO QAQC complete for %s!", station),
          type = "message", duration = 3
        )
      }, error = function(e) {
        removeNotification("do_qaqc_progress")
        showNotification(paste("QAQC Error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Flag summary table ------
    output$flag_summary <- DT::renderDT({
      req(do_qc_result())
      df <- do_qc_result()

      # Build summary with available flag columns
      flag_cols <- c("do_error", "do_neg_between", "do_neg_lt_minus1", "do_high",
                     "temp_flag_ice", "temp_neg_between", "temp_neg_leq_minus1",
                     "flag_do_dry")
      available_flags <- intersect(flag_cols, names(df))

      summary_df <- df %>%
        dplyr::group_by(site_station_code) %>%
        dplyr::summarise(
          Records = dplyr::n(),
          `DO Error` = if ("do_error" %in% available_flags) sum(do_error, na.rm = TRUE) else 0L,
          `DO Neg (-1 to 0)` = if ("do_neg_between" %in% available_flags) sum(do_neg_between, na.rm = TRUE) else 0L,
          `DO Neg (< -1)` = if ("do_neg_lt_minus1" %in% available_flags) sum(do_neg_lt_minus1, na.rm = TRUE) else 0L,
          `DO High` = if ("do_high" %in% available_flags) sum(do_high, na.rm = TRUE) else 0L,
          `Ice` = if ("temp_flag_ice" %in% available_flags) sum(temp_flag_ice, na.rm = TRUE) else 0L,
          `Dry` = if ("flag_do_dry" %in% available_flags) sum(flag_do_dry, na.rm = TRUE) else 0L,
          `DO NA (adj)` = if ("do_mgl_adj" %in% names(df)) sum(is.na(do_mgl_adj)) else 0L,
          .groups = "drop"
        )

      DT::datatable(summary_df,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE),
        rownames = FALSE,
        selection = "single"
      )
    })

    # ------ Update station selectors after QC ------
    observe({
      req(do_qc_result())
      stations <- unique(do_qc_result()$site_station_code)
      updateSelectInput(session, "station_select", choices = stations, selected = stations[1])
      updateSelectInput(session, "adj_station", choices = stations, selected = stations[1])
      updateSelectInput(session, "drift_station", choices = stations, selected = stations[1])
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

      flag_cols <- intersect(
        c("do_error", "do_neg_between", "do_neg_lt_minus1", "do_high",
          "temp_flag_ice", "flag_do_dry"),
        names(data)
      )
      n_flags <- sum(sapply(flag_cols, function(col) sum(data[[col]], na.rm = TRUE)))
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
    output$qaqc_plot <- renderPlotly({
      data <- selected_data()
      req(nrow(data) > 0)
      station <- input$station_select

      # DO panel (raw vs adjusted)
      p1 <- plot_ly(data, x = ~timestamp, y = ~do_mgl,
              type = "scattergl", mode = "lines",
              line = list(color = "#90CAF9", width = 1),
              name = "Raw DO (mg/L)",
              hovertemplate = "Raw: %{y:.2f} mg/L<extra></extra>")

      if ("do_mgl_adj" %in% names(data)) {
        p1 <- p1 %>% add_trace(y = ~do_mgl_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#1565C0", width = 1),
              name = "Adj DO (mg/L)",
              hovertemplate = "Adj: %{y:.2f} mg/L<extra></extra>")
      }

      # Add flag markers to DO panel
      if ("do_error" %in% names(data)) {
        err <- data[which(data$do_error), ]
        if (nrow(err) > 0) {
          p1 <- p1 %>% add_trace(data = err, x = ~timestamp, y = ~do_mgl,
            type = "scattergl", mode = "markers",
            marker = list(color = "#D32F2F", size = 5, symbol = "circle"),
            name = "DO Error",
            hovertemplate = "ERROR: %{y:.2f} mg/L<extra></extra>")
        }
      }
      if ("do_high" %in% names(data)) {
        hi <- data[which(data$do_high), ]
        if (nrow(hi) > 0) {
          p1 <- p1 %>% add_trace(data = hi, x = ~timestamp, y = ~do_mgl,
            type = "scattergl", mode = "markers",
            marker = list(color = "#FBC02D", size = 5, symbol = "circle"),
            name = "DO High",
            hovertemplate = "HIGH: %{y:.2f} mg/L<extra></extra>")
        }
      }
      if ("flag_do_dry" %in% names(data)) {
        dry <- data[which(data$flag_do_dry), ]
        if (nrow(dry) > 0) {
          p1 <- p1 %>% add_trace(data = dry, x = ~timestamp, y = ~do_mgl,
            type = "scattergl", mode = "markers",
            marker = list(color = "#795548", size = 5, symbol = "diamond"),
            name = "Dry Sensor",
            hovertemplate = "DRY: %{y:.2f} mg/L<extra></extra>")
        }
      }

      # %sat panel
      p2 <- plot_ly()
      if ("do_percsat" %in% names(data)) {
        p2 <- plot_ly(data, x = ~timestamp, y = ~do_percsat,
                type = "scattergl", mode = "lines",
                line = list(color = "#A5D6A7", width = 1),
                name = "Raw %sat",
                hovertemplate = "Raw: %{y:.1f} %%sat<extra></extra>")
        if ("do_percsat_adj" %in% names(data)) {
          p2 <- p2 %>% add_trace(y = ~do_percsat_adj,
                type = "scattergl", mode = "lines",
                line = list(color = "#2e7d32", width = 1),
                name = "Adj %sat",
                hovertemplate = "Adj: %{y:.1f} %%sat<extra></extra>")
        }
      }

      # Temperature panel
      p3 <- plot_ly(data, x = ~timestamp, y = ~watertemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "#FFCC80", width = 1),
              name = "Raw Water Temp",
              hovertemplate = "Raw: %{y:.1f} \u00b0C<extra></extra>")

      if ("watertemp_C_do_adj" %in% names(data)) {
        p3 <- p3 %>% add_trace(y = ~watertemp_C_do_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#E65100", width = 1),
              name = "Adj Water Temp",
              hovertemplate = "Adj: %{y:.1f} \u00b0C<extra></extra>")
      }

      if ("temp_flag_ice" %in% names(data)) {
        ice <- data[which(data$temp_flag_ice), ]
        if (nrow(ice) > 0) {
          p3 <- p3 %>% add_trace(data = ice, x = ~timestamp, y = ~watertemp_C,
            type = "scattergl", mode = "markers",
            marker = list(color = "#1565C0", size = 5, symbol = "circle"),
            name = "Ice",
            hovertemplate = "ICE: %{y:.1f} \u00b0C<extra></extra>")
        }
      }

      subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = paste("DO QAQC -", station), font = list(size = 14)),
          xaxis3 = list(title = "Date"),
          yaxis = list(title = "DO (mg/L)"),
          yaxis2 = list(title = "DO (%sat)"),
          yaxis3 = list(title = "Water Temp (\u00b0C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.1),
          margin = list(b = 80)
        )
    })

    # ------ Summary stats ------
    output$do_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      cat("--- Raw DO (mg/L) ---\n")
      print(summary(data$do_mgl))
      if ("do_mgl_adj" %in% names(data)) {
        cat("\n--- Adjusted DO (mg/L) ---\n")
        print(summary(data$do_mgl_adj))
      }
      if ("do_percsat" %in% names(data)) {
        cat("\n--- Raw %sat ---\n")
        print(summary(data$do_percsat))
      }
      if ("do_percsat_adj" %in% names(data)) {
        cat("\n--- Adjusted %sat ---\n")
        print(summary(data$do_percsat_adj))
      }
    })

    output$temp_summary <- renderPrint({
      data <- selected_data()
      req(nrow(data) > 0)
      cat("--- Raw Water Temp ---\n")
      print(summary(data$watertemp_C))
      if ("watertemp_C_do_adj" %in% names(data)) {
        cat("\n--- Adjusted Water Temp ---\n")
        print(summary(data$watertemp_C_do_adj))
      }
    })

    # ------ Drift Correction ------
    observeEvent(input$drift_btn, {
      qc_data <- do_qc_result()
      ref_data <- rv_raw_data$field_references
      station <- input$drift_station

      if (is.null(qc_data)) {
        showNotification("Run QAQC first.", type = "error", duration = 3)
        return()
      }
      if (is.null(ref_data) || nrow(ref_data) == 0) {
        showNotification("No field reference data available.", type = "error", duration = 5)
        return()
      }
      if (is.null(station) || station == "") {
        showNotification("Select a station.", type = "error", duration = 3)
        return()
      }

      # Filter ref data for DO at this station
      do_refs <- ref_data[ref_data$metric == "do" & ref_data$site_station_code == station, ]
      if (nrow(do_refs) == 0) {
        showNotification(
          sprintf("No DO field references found for %s.", station),
          type = "warning", duration = 5)
        return()
      }

      tryCatch({
        showNotification("Applying drift correction...", type = "message",
          duration = NULL, id = "do_drift_progress")

        result <- waterlogged::wq_drift_correction(
          input_data = qc_data,
          select_station = station,
          metric = "DO",
          method = input$drift_method,
          max_ref_gap_h = input$drift_max_gap,
          log_root = NULL,
          ref_data = do_refs
        )

        # Merge back
        other_data <- dplyr::filter(qc_data, site_station_code != station)
        station_result <- dplyr::filter(result, site_station_code == station)
        combined <- dplyr::bind_rows(other_data, station_result) %>%
          dplyr::arrange(site_station_code, timestamp)

        do_qc_result(combined)
        rv_qaqc_data$do_qaqc <- combined
        drift_applied(TRUE)

        removeNotification("do_drift_progress")
        showNotification(
          sprintf("Drift correction applied to %s.", station),
          type = "message", duration = 3
        )
      }, error = function(e) {
        removeNotification("do_drift_progress")
        showNotification(paste("Drift error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Drift result info ------
    output$drift_result_info <- renderUI({
      req(drift_applied())
      df <- do_qc_result()
      req(df)

      n_corrected <- if ("edit_do_drift" %in% names(df)) {
        sum(df$edit_do_drift, na.rm = TRUE)
      } else {
        0
      }

      tags$div(
        style = "color: #155724; background: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("check-circle"),
        sprintf(" Drift correction applied. %s records adjusted.",
                format(n_corrected, big.mark = ","))
      )
    })

    # ------ Manual adjustment ------
    observeEvent(input$adj_btn, {
      qc_data <- do_qc_result()
      req(qc_data)

      station <- input$adj_station
      reason <- input$adj_reason
      ts_start <- input$adj_start
      ts_end <- input$adj_end
      note <- input$adj_note

      if (is.null(station) || station == "") {
        showNotification("Select a station.", type = "error", duration = 3)
        return()
      }
      if (nchar(ts_start) == 0 || nchar(ts_end) == 0) {
        showNotification("Enter start and end timestamps.", type = "error", duration = 3)
        return()
      }
      if (nchar(note) == 0) {
        showNotification("Enter a note describing the adjustment.", type = "error", duration = 3)
        return()
      }

      tryCatch({
        station_data <- dplyr::filter(qc_data, site_station_code == station)
        other_data <- dplyr::filter(qc_data, site_station_code != station)

        adjusted <- waterlogged::adjust_logger_NA(
          input_data = station_data,
          select_station = station,
          metric = "DO",
          reason_to_adjust = reason,
          timestamp_start = ts_start,
          timestamp_end = ts_end,
          keep_temp = input$adj_keep_temp,
          manual_note = note,
          log_root = NULL
        )

        updated <- dplyr::bind_rows(other_data, adjusted) %>%
          dplyr::arrange(site_station_code, timestamp)

        do_qc_result(updated)
        rv_qaqc_data$do_qaqc <- updated

        showNotification(
          sprintf("Removed %s to %s for %s (%s)", ts_start, ts_end, station, reason),
          type = "message", duration = 5
        )
      }, error = function(e) {
        showNotification(paste("Adjustment error:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    # ------ Download: Clean CSV ------
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("do_qaqc_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- do_qc_result()
        req(df)

        export_cols <- c(
          "site_station_code", "timestamp",
          "do_mgl", "do_mgl_adj",
          "do_percsat", "do_percsat_adj",
          "watertemp_C", "watertemp_C_do_adj",
          "airpress_kPa", "airtemp_C"
        )
        flag_cols <- grep("^(edit_|flag_|do_error|do_neg|do_high|temp_flag|temp_neg)", names(df), value = TRUE)
        export_cols <- intersect(c(export_cols, flag_cols), names(df))

        export_df <- df[, export_cols]
        export_df$timestamp <- format(export_df$timestamp, "%Y-%m-%d %H:%M:%S")
        write.csv(export_df, file, row.names = FALSE)
      }
    )

    # ------ Download: QAQC Log ------
    output$download_log <- downloadHandler(
      filename = function() {
        paste0("do_qaqc_log_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        log <- qaqc_log()
        if (!is.null(log) && nrow(log) > 0) {
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

  })
}
