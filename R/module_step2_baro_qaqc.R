#' Step 2: Barometric QAQC Module - UI
module_step2_baro_qaqc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 2: Barometric QAQC", class = "custom-h1"),
      tags$h2("Clean barometric data before water level compensation", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Barometric pressure data must be quality-controlled before it can be used to
          compensate water level and dissolved oxygen readings. Errors in barometric data
          (spikes, flatlines, or out-of-range values) will propagate directly into all
          downstream water level and DO calculations."
        ),
        tags$p(
          "This step applies automated QAQC checks to all barometric stations at once.
          Flagged readings are replaced with NA in the adjusted columns (airpress_kPa_adj,
          airtemp_C_adj), while the original raw values are preserved. The edit flag columns
          (edit_press_high, edit_press_low, etc.) record which check flagged each reading."
        )
      ),

      hr(),

      # Section A: QAQC Parameters
      tags$h3("QAQC Parameters", class = "custom-h2"),
      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("About the thresholds", style = "margin-top: 0;"),
            tags$p(
              "The QAQC process applies five checks to each barometric station. Readings
              that fail any check are flagged and their adjusted values set to NA."
            ),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Pressure range:"), " Flags readings outside the low/high
                      pressure bounds. Typical atmospheric pressure ranges from ~85 kPa
                      (high elevation) to ~105 kPa (sea level)."),
              tags$li(tags$strong("Temperature range:"), " Flags air temperature readings
                      outside expected limits."),
              tags$li(tags$strong("Spike detection:"), " Flags sudden pressure jumps that exceed
                      the threshold between consecutive readings."),
              tags$li(tags$strong("Flatline detection:"), " Flags runs of identical pressure
                      readings that suggest the logger stopped responding.")
            )
          )
        ),
        column(
          width = 8,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px;",
            fluidRow(
              column(
                width = 6,
                tags$h4("Pressure Thresholds", style = "margin-top: 0;"),
                numericInput(ns("pressure_low"), "Low pressure limit (kPa):",
                  value = 85, min = 50, max = 110, step = 1
                ),
                numericInput(ns("pressure_high"), "High pressure limit (kPa):",
                  value = 105, min = 90, max = 120, step = 1
                )
              ),
              column(
                width = 6,
                tags$h4("Temperature Thresholds", style = "margin-top: 0;"),
                numericInput(ns("temp_low"), "Low temperature limit (C):",
                  value = -40, min = -60, max = 0, step = 1
                ),
                numericInput(ns("temp_high"), "High temperature limit (C):",
                  value = 50, min = 20, max = 80, step = 1
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                tags$h4("Spike & Flatline Detection", style = "margin-top: 0;"),
                numericInput(ns("spike_threshold"), "Spike threshold (kPa):",
                  value = 1.5, min = 0.1, max = 10, step = 0.1
                ),
                numericInput(ns("flatline_n"), "Flatline window (consecutive readings):",
                  value = 6, min = 2, max = 50, step = 1
                )
              ),
              column(
                width = 6,
                tags$div(
                  style = "margin-top: 30px;",
                  actionButton(ns("run_qaqc"), "Run Barometric QAQC",
                    class = "btn-primary btn-lg", width = "100%",
                    icon = icon("play")
                  ),
                  tags$div(
                    style = "margin-top: 15px;",
                    uiOutput(ns("qaqc_status"))
                  )
                )
              )
            )
          )
        )
      ),

      hr(),

      # Section B: Results (visible only after QAQC runs)
      conditionalPanel(
        condition = "output.show_results",
        ns = ns,

        tags$h3("QAQC Results", class = "custom-h2"),

        # Flag Summary Table
        tags$h4("Flag Summary by Station"),
        tags$p(
          "This table shows the number of readings flagged by each check per station.
          Click a row to select that station for detailed inspection below.",
          class = "clean-print"
        ),
        DT::DTOutput(ns("flag_summary_table")),

        hr(),

        # Station Detail
        tags$h3("Station Detail", class = "custom-h2"),
        tags$div(
          style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
          fluidRow(
            column(
              width = 4,
              selectInput(ns("station_select"), "Select Station:",
                choices = NULL, width = "100%"
              )
            ),
            column(
              width = 8,
              uiOutput(ns("station_info"))
            )
          )
        ),

        fluidRow(
          # Left column - flag legend and summary stats
          column(
            width = 4,
            tags$div(
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
              tags$h4("Flag Legend", style = "margin-top: 0;"),
              tags$ul(
                style = "padding-left: 20px; list-style: none;",
                tags$li(tags$span(style = "color: #D32F2F;", "\u25CF"), " High pressure"),
                tags$li(tags$span(style = "color: #F57C00;", "\u25CF"), " Low pressure"),
                tags$li(tags$span(style = "color: #FBC02D;", "\u25CF"), " Temperature out of range"),
                tags$li(tags$span(style = "color: #7B1FA2;", "\u25CF"), " Spike detected"),
                tags$li(tags$span(style = "color: #00897B;", "\u25CF"), " Flatline detected")
              )
            ),
            tags$h4("Pressure Summary (raw vs adjusted)"),
            verbatimTextOutput(ns("station_pressure_summary")),
            tags$h4("Temperature Summary (raw vs adjusted)"),
            verbatimTextOutput(ns("station_temp_summary"))
          ),

          # Right column - plots
          column(
            width = 8,
            plotlyOutput(ns("baro_qaqc_plot"), height = "550px") %>% withSpinner()
          )
        )
      ),

      # Next step callout
      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next:"),
          " Proceed to Step 3: Convert Metrics to use the cleaned barometric data for
          water level and dissolved oxygen compensation.",
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

#' Step 2: Barometric QAQC Module - Server
module_step2_baro_qaqc_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Local reactive to store QAQC result for display
    baro_qaqc_result <- reactiveVal(NULL)

    # Reactive flag to drive conditionalPanel visibility
    output$show_results <- reactive({ !is.null(baro_qaqc_result()) })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    # Convenience accessors
    rv_raw_data <- session$userData$rv_raw_data
    rv_qaqc_data <- session$userData$rv_qaqc_data

    # Pre-run status showing data availability
    output$qaqc_status <- renderUI({
      baro_list <- rv_raw_data$baro_data
      metadata <- rv_raw_data$metadata

      if (is.null(baro_list) || length(baro_list) == 0) {
        tags$div(
          style = "color: #D32F2F; font-weight: bold;",
          icon("exclamation-triangle"),
          " No barometric data loaded. Please load data in the Upload step first."
        )
      } else if (is.null(metadata)) {
        tags$div(
          style = "color: #D32F2F; font-weight: bold;",
          icon("exclamation-triangle"),
          " No metadata loaded. Please load metadata in the Upload step first."
        )
      } else {
        n_stations <- length(baro_list)
        n_records <- sum(sapply(baro_list, nrow))
        tags$div(
          style = "color: #1B5E20; font-weight: bold;",
          icon("check-circle"),
          sprintf(" Ready: %d station(s), %s records",
            n_stations, format(n_records, big.mark = ","))
        )
      }
    })

    # Main QAQC action
    observeEvent(input$run_qaqc, {
      baro_list <- rv_raw_data$baro_data
      metadata <- rv_raw_data$metadata

      # Validate data availability
      if (is.null(baro_list) || length(baro_list) == 0) {
        showNotification("No barometric data available. Please load data first.",
          type = "error", duration = 5)
        return()
      }
      if (is.null(metadata)) {
        showNotification("No metadata available. Please load metadata first.",
          type = "error", duration = 5)
        return()
      }

      # Combine named list into single data frame
      baro_combined <- dplyr::bind_rows(baro_list)

      tryCatch({
        showNotification("Running barometric QAQC...", type = "message",
          duration = NULL, id = "baro_qaqc_progress")

        result <- waterlogged::barometric_qaqc_all(
          baro_data           = baro_combined,
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

        # Store result locally and in shared reactive
        baro_qaqc_result(result)
        rv_qaqc_data$baro_qaqc <- result

        # Results panel visibility is driven by conditionalPanel
        # watching output$show_results (reacts to baro_qaqc_result)

        removeNotification("baro_qaqc_progress")
        showNotification("Barometric QAQC complete!", type = "message", duration = 3)

      }, error = function(e) {
        removeNotification("baro_qaqc_progress")
        showNotification(
          paste("QAQC Error:", conditionMessage(e)),
          type = "error", duration = 8
        )
      })
    })

    # Flag summary table
    output$flag_summary_table <- DT::renderDT({
      req(baro_qaqc_result())
      df <- baro_qaqc_result()

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
        options = list(
          dom = "t",
          pageLength = 50,
          scrollX = TRUE
        ),
        rownames = FALSE,
        selection = "single"
      )
    })

    # Update station selector when results are available
    observe({
      req(baro_qaqc_result())
      stations <- unique(baro_qaqc_result()$site_station_code)
      updateSelectInput(session, "station_select",
        choices = stations, selected = stations[1])
    })

    # Reactive filtered data for selected station
    selected_station_data <- reactive({
      req(input$station_select, baro_qaqc_result())
      dplyr::filter(baro_qaqc_result(), site_station_code == input$station_select)
    })

    # Station info display
    output$station_info <- renderUI({
      req(input$station_select)
      data <- selected_station_data()
      req(nrow(data) > 0)

      n_records <- nrow(data)
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
          tags$strong("Records: "), format(n_records, big.mark = ","), " | ",
          tags$strong("Flags: "), format(n_flags, big.mark = ","), " | ",
          tags$strong("Date Range: "), date_range
        )
      )
    })

    # QAQC before/after plot
    output$baro_qaqc_plot <- renderPlotly({
      data <- selected_station_data()
      req(nrow(data) > 0)
      station <- input$station_select

      # Pressure subplot — first trace directly in plot_ly() to anchor data + axes
      p1 <- plot_ly(data, x = ~timestamp, y = ~airpress_kPa,
              type = "scattergl", mode = "lines",
              line = list(color = "#90CAF9", width = 1),
              name = "Raw Pressure",
              hovertemplate = "Raw: %{y:.2f} kPa<extra></extra>") %>%
        add_trace(data = data, x = ~timestamp, y = ~airpress_kPa_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#1565C0", width = 1),
              name = "Adjusted Pressure",
              hovertemplate = "Adj: %{y:.2f} kPa<extra></extra>")

      # Add flag markers for pressure (use which() to safely exclude NA indices)
      high_flags <- data[which(data$edit_press_high), ]
      low_flags <- data[which(data$edit_press_low), ]
      spike_flags <- data[which(data$edit_spike), ]
      flat_flags <- data[which(data$edit_flatline), ]

      if (nrow(high_flags) > 0) {
        p1 <- p1 %>% add_trace(data = high_flags, x = ~timestamp, y = ~airpress_kPa,
          type = "scattergl", mode = "markers",
          marker = list(color = "#D32F2F", size = 5, symbol = "circle"),
          name = "High Pressure Flag",
          hovertemplate = "HIGH: %{y:.2f} kPa<extra></extra>")
      }
      if (nrow(low_flags) > 0) {
        p1 <- p1 %>% add_trace(data = low_flags, x = ~timestamp, y = ~airpress_kPa,
          type = "scattergl", mode = "markers",
          marker = list(color = "#F57C00", size = 5, symbol = "circle"),
          name = "Low Pressure Flag",
          hovertemplate = "LOW: %{y:.2f} kPa<extra></extra>")
      }
      if (nrow(spike_flags) > 0) {
        p1 <- p1 %>% add_trace(data = spike_flags, x = ~timestamp, y = ~airpress_kPa,
          type = "scattergl", mode = "markers",
          marker = list(color = "#7B1FA2", size = 5, symbol = "diamond"),
          name = "Spike Flag",
          hovertemplate = "SPIKE: %{y:.2f} kPa<extra></extra>")
      }
      if (nrow(flat_flags) > 0) {
        p1 <- p1 %>% add_trace(data = flat_flags, x = ~timestamp, y = ~airpress_kPa,
          type = "scattergl", mode = "markers",
          marker = list(color = "#00897B", size = 5, symbol = "square"),
          name = "Flatline Flag",
          hovertemplate = "FLAT: %{y:.2f} kPa<extra></extra>")
      }

      # Temperature subplot — first trace directly in plot_ly()
      p2 <- plot_ly(data, x = ~timestamp, y = ~airtemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "#FFCC80", width = 1),
              name = "Raw Temperature",
              hovertemplate = "Raw: %{y:.2f} C<extra></extra>") %>%
        add_trace(data = data, x = ~timestamp, y = ~airtemp_C_adj,
              type = "scattergl", mode = "lines",
              line = list(color = "#E65100", width = 1),
              name = "Adjusted Temperature",
              hovertemplate = "Adj: %{y:.2f} C<extra></extra>")

      # Temperature flag markers (use which() to safely exclude NA indices)
      temp_flags <- data[which(data$edit_temp_range), ]
      if (nrow(temp_flags) > 0) {
        p2 <- p2 %>% add_trace(data = temp_flags, x = ~timestamp, y = ~airtemp_C,
          type = "scattergl", mode = "markers",
          marker = list(color = "#FBC02D", size = 5, symbol = "circle"),
          name = "Temp Range Flag",
          hovertemplate = "TEMP: %{y:.2f} C<extra></extra>")
      }

      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(
            text = paste("Barometric QAQC -", station),
            font = list(size = 14)
          ),
          xaxis = list(title = "", tickangle = 0, nticks = 8),
          xaxis2 = list(title = "Date", tickangle = 0, nticks = 8),
          yaxis = list(title = "Pressure (kPa)"),
          yaxis2 = list(title = "Temperature (C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15),
          margin = list(b = 80)
        )
    })

    # Pressure summary stats
    output$station_pressure_summary <- renderPrint({
      data <- selected_station_data()
      req(nrow(data) > 0)
      cat("--- Raw ---\n")
      print(summary(data$airpress_kPa))
      cat("\n--- Adjusted ---\n")
      print(summary(data$airpress_kPa_adj))
    })

    # Temperature summary stats
    output$station_temp_summary <- renderPrint({
      data <- selected_station_data()
      req(nrow(data) > 0)
      cat("--- Raw ---\n")
      print(summary(data$airtemp_C))
      cat("\n--- Adjusted ---\n")
      print(summary(data$airtemp_C_adj))
    })
  })
}
