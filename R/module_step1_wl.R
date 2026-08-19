#' Step 1: Water Level Review Module - UI
module_step1_wl_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Water Level Series", class = "custom-h1"),
      tags$h2("Review continuous water level pressure data", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Water level data is collected from HOBO U20 loggers deployed underwater.
          The logger measures absolute pressure, which includes both the pressure from
          the water column above the sensor AND the atmospheric pressure. This is why
          we need barometric data to calculate true water depth."
        ),
        tags$p(
          "Before applying barometric compensation, it is important to review the raw
          pressure data to identify any periods where the data quality may be compromised.
          Common issues include the logger being out of water (dry), ice formation affecting
          readings, or physical disturbance of the logger."
        )
      ),

      # Station selector at top
      tags$div(
        style = "background-color: #e8f5e9; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
        fluidRow(
          column(
            width = 4,
            selectInput(ns("station_select"), "Select Station:",
              choices = NULL,
              width = "100%"
            )
          ),
          column(
            width = 8,
            uiOutput(ns("station_info"))
          )
        )
      ),

      fluidRow(
        # Left column - guidance and context
        column(
          width = 4,
          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What to look for", style = "margin-top: 0;"),
            tags$p(
              "When reviewing water level data, you are looking for periods that may need
              to be flagged or removed during QC. Key issues include:",
              style = "margin-bottom: 10px;"
            ),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Sudden pressure drops"), " - may indicate the logger came out of the water"),
              tags$li(tags$strong("Flat lines"), " - could indicate sensor malfunction or the logger being frozen"),
              tags$li(tags$strong("Erratic readings with low temperature"), " - often indicates ice formation"),
              tags$li(tags$strong("Sudden jumps"), " - may indicate cable break or logger disturbance")
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px;",
            tags$h4("Understanding the data", style = "margin-top: 0;"),
            tags$p(
              "The raw water pressure values are in kilopascals (kPa) and represent the
              total pressure on the sensor. For reference, standard atmospheric pressure
              at sea level is about 101.3 kPa. A water column of 1 metre adds approximately
              9.8 kPa of pressure."
            ),
            tags$p(
              "So if the barometric pressure is 95 kPa and the water logger reads 105 kPa,
              the water pressure component is about 10 kPa, corresponding to roughly 1 metre
              of water depth (the exact calculation accounts for water temperature and density)."
            )
          )
        ),

        # Right column - plots and statistics
        column(
          width = 8,
          tags$h3("Time Series", class = "custom-h2", style = "margin-top: 0;"),
          tags$p(
            "The plots below show water pressure and water temperature over time.
            Use your mouse to zoom in on specific periods by clicking and dragging.
            Both plots are synchronized for easier comparison.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          plotlyOutput(ns("wl_combined_plot"), height = "500px") %>% withSpinner(),

          hr(),

          tags$h3("Summary Statistics", class = "custom-h2"),
          tags$p(
            "These statistics summarize the full time series. Compare the water pressure
            range to expected values for your site depth. Water temperature should be
            within reasonable ranges for your region and season.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          fluidRow(
            column(
              width = 6,
              tags$h4("Water Pressure (kPa)"),
              verbatimTextOutput(ns("wl_pressure_summary"))
            ),
            column(
              width = 6,
              tags$h4("Water Temperature (C)"),
              verbatimTextOutput(ns("wl_temp_summary"))
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Note:"),
          " The water pressure shown here is the raw absolute pressure from the logger.
          In Step 2, we will apply barometric compensation by subtracting atmospheric
          pressure and converting to water depth in metres. You will need at least one
          manual water level measurement (depth or stage) to calibrate the conversion.",
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

#' Step 1: Water Level Review Module - Server
module_step1_wl_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Update station choices when data changes
    observe({
      data_list <- session$userData$rv_raw_data$waterlevel_data
      req(data_list)
      stations <- names(data_list)
      updateSelectInput(session, "station_select",
        choices = stations, selected = stations[1]
      )
    })

    # Reactive data based on station selection
    selected_data <- reactive({
      req(input$station_select)
      req(session$userData$rv_raw_data$waterlevel_data)
      session$userData$rv_raw_data$waterlevel_data[[input$station_select]]
    })

    # Station info display
    output$station_info <- renderUI({
      req(input$station_select)
      data <- selected_data()
      n_records <- nrow(data)
      date_range <- paste(
        format(min(data$timestamp), "%Y-%m-%d"),
        "to",
        format(max(data$timestamp), "%Y-%m-%d")
      )
      tags$div(
        style = "padding-top: 8px;",
        tags$p(style = "margin: 0; font-size: 14px;",
          tags$strong("Station: "), input$station_select, " | ",
          tags$strong("Logger: "), "HOBO U20 | ",
          tags$strong("Records: "), format(n_records, big.mark = ","), " | ",
          tags$strong("Date Range: "), date_range
        )
      )
    })

    # Combined synced plot using subplot
    output$wl_combined_plot <- renderPlotly({
      data <- selected_data()
      station <- input$station_select

      p1 <- plot_ly(data, x = ~timestamp, y = ~waterpress_kPa,
              type = "scattergl", mode = "lines",
              line = list(color = "#2E7D32", width = 1),
              name = "Pressure",
              hovertemplate = "Pressure: %{y:.2f} kPa<extra></extra>")

      p2 <- plot_ly(data, x = ~timestamp, y = ~watertemp_C,
              type = "scattergl", mode = "lines",
              line = list(color = "#00796B", width = 1),
              name = "Temperature",
              hovertemplate = "Temp: %{y:.2f} C<extra></extra>")

      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = paste("Water Pressure & Temperature -", station), font = list(size = 14)),
          xaxis = list(title = "", tickangle = 0, nticks = 8),
          xaxis2 = list(title = "Date", tickangle = 0, nticks = 8),
          yaxis = list(title = "Pressure (kPa)"),
          yaxis2 = list(title = "Temperature (C)"),
          hovermode = "x unified",
          showlegend = FALSE,
          margin = list(b = 50)
        )
    })

    # Summary statistics for pressure
    output$wl_pressure_summary <- renderPrint({
      data <- selected_data()
      summary(data$waterpress_kPa)
    })

    # Summary statistics for temperature
    output$wl_temp_summary <- renderPrint({
      data <- selected_data()
      summary(data$watertemp_C)
    })
  })
}
