#' Step 1: Barometric Pressure Review Module - UI
module_step1_baro_ui <- function(id) {
 ns <- NS(id)

 tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Barometric Pressure Series", class = "custom-h1"),
      tags$h2("Review continuous barometric pressure data", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Barometric pressure data is collected from HOBO U20 loggers deployed in air,
          typically in a nearby tree or shelter above the water surface. This data is
          essential for compensating water pressure readings to calculate true water depth."
        ),
        tags$p(
          "The barometric logger measures atmospheric pressure, which changes with weather
          systems and elevation. These pressure changes need to be subtracted from the
          water level logger readings to isolate the pressure contribution from the water
          column alone."
        )
      ),

      fluidRow(
        # Left column - guidance and context
        column(
          width = 4,
          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Station Information", style = "margin-top: 0;"),
            tags$p(tags$strong("Site: "), "SLUG_ST_01"),
            tags$p(tags$strong("Logger: "), "HOBO U20"),
            tags$p(tags$strong("Serial Number: "), "12345678"),
            tags$p(tags$strong("Date Range: "), "2024-01-01 to 2024-10-10")
          ),

          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What to look for", style = "margin-top: 0;"),
            tags$p(
              "When reviewing barometric data, you are checking that the logger was
              functioning properly and that the data can be used reliably for compensation.
              Look for:",
              style = "margin-bottom: 10px;"
            ),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Sudden spikes or drops"), " - may indicate logger malfunction or obstruction"),
              tags$li(tags$strong("Flat lines"), " - could indicate the logger stopped recording"),
              tags$li(tags$strong("Gaps in the time series"), " - missing data will affect compensation"),
              tags$li(tags$strong("Unreasonable values"), " - atmospheric pressure typically ranges from 85-105 kPa depending on elevation")
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px;",
            tags$h4("Understanding the data", style = "margin-top: 0;"),
            tags$p(
              "Normal barometric pressure varies with weather systems. You should see
              gradual rises and falls over hours to days as high and low pressure systems
              move through the area. Rapid changes of more than a few kPa in an hour
              would be unusual."
            ),
            tags$p(
              "Air temperature from the barometric logger is also recorded and can be
              useful for identifying periods when water loggers may have been frozen,
              though note that air temperature may differ from water temperature,
              especially in deeper water."
            )
          )
        ),

        # Right column - plots and statistics
        column(
          width = 8,
          tags$h3("Time Series", class = "custom-h2", style = "margin-top: 0;"),
          tags$p(
            "The plots below show barometric pressure and air temperature over time.
            Use your mouse to zoom in on specific periods by clicking and dragging.
            Both plots are synchronized, so zooming one will zoom the other.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          plotlyOutput(ns("baro_combined_plot"), height = "500px") %>% withSpinner(),

          hr(),

          tags$h3("Summary Statistics", class = "custom-h2"),
          tags$p(
            "These statistics summarize the full time series. Check that the minimum
            and maximum values are within expected ranges for your region and elevation.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          fluidRow(
            column(
              width = 6,
              tags$h4("Atmospheric Pressure (kPa)"),
              verbatimTextOutput(ns("baro_pressure_summary"))
            ),
            column(
              width = 6,
              tags$h4("Air Temperature (C)"),
              verbatimTextOutput(ns("baro_temp_summary"))
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next:"),
          " Review Water Level, Dissolved Oxygen, and Conductivity data in the other subtabs,
          then check the Overview tab to compare all series together and ensure data coverage
          periods overlap appropriately.",
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

#' Step 1: Barometric Pressure Review Module - Server
module_step1_baro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Combined synced plot using subplot
    output$baro_combined_plot <- renderPlotly({
      p1 <- plot_ly(demo_baro, x = ~timestamp, y = ~airpress_kPa_U20,
              type = "scattergl", mode = "lines",
              line = list(color = "#1976D2", width = 1),
              name = "Pressure",
              hovertemplate = "Pressure: %{y:.2f} kPa<extra></extra>")

      p2 <- plot_ly(demo_baro, x = ~timestamp, y = ~airtemp_C_U20,
              type = "scattergl", mode = "lines",
              line = list(color = "#F57C00", width = 1),
              name = "Temperature",
              hovertemplate = "Temp: %{y:.2f} C<extra></extra>")

      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "Barometric Pressure & Air Temperature - SLUG_ST_01", font = list(size = 14)),
          xaxis = list(title = ""),
          xaxis2 = list(title = "Date"),
          yaxis = list(title = "Pressure (kPa)"),
          yaxis2 = list(title = "Temperature (C)"),
          hovermode = "x unified",
          showlegend = FALSE
        )
    })

    # Summary statistics for pressure
    output$baro_pressure_summary <- renderPrint({
      summary(demo_baro$airpress_kPa_U20)
    })

    # Summary statistics for temperature
    output$baro_temp_summary <- renderPrint({
      summary(demo_baro$airtemp_C_U20)
    })
  })
}
