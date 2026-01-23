#' Step 1: Conductivity Review Module - UI
module_step1_cond_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Conductivity Series", class = "custom-h1"),
      tags$h2("Review continuous electrical conductivity data", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Conductivity data is collected from HOBO U24 loggers deployed underwater.
          Electrical conductivity measures the water's ability to conduct electricity,
          which is directly related to the concentration of dissolved ions (salts, minerals)
          in the water."
        ),
        tags$p(
          "Conductivity is influenced by geology, land use, and seasonal patterns.
          It can increase during low-flow periods when water is in contact with rocks
          and sediments longer, or decrease during high-flow events when dilution occurs.
          Sudden spikes may indicate pollution events or sediment disturbance."
        )
      ),

      fluidRow(
        # Left column - guidance and context
        column(
          width = 4,
          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Station Information", style = "margin-top: 0;"),
            tags$p(tags$strong("Site: "), "BUGS_WE_01"),
            tags$p(tags$strong("Logger: "), "HOBO U24"),
            tags$p(tags$strong("Serial Number: "), "82345678"),
            tags$p(tags$strong("Date Range: "), "2024-04-21 to 2025-04-18")
          ),

          tags$div(
            style = "background-color: #fff8e1; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What to look for", style = "margin-top: 0;"),
            tags$p(
              "When reviewing conductivity data, look for patterns that indicate
              data quality issues or environmental events:",
              style = "margin-bottom: 10px;"
            ),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Sudden spikes"), " - may indicate sediment disturbance, contamination, or sensor artifact"),
              tags$li(tags$strong("Gradual drift upward"), " - often indicates sensor fouling from biofilm or sediment accumulation"),
              tags$li(tags$strong("Drop to near zero"), " - indicates logger is dry or frozen (ice has very low conductivity)"),
              tags$li(tags$strong("Large change (>200 uS/cm/hr)"), " - flagged as potential disturbance, but may be natural during floods")
            )
          ),

          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px;",
            tags$h4("Processing considerations", style = "margin-top: 0;"),
            tags$p(
              "Conductivity data requires additional processing steps compared to other
              logger types. Sensors can experience drift over long deployments, which
              can be corrected using field reference measurements from a calibrated
              handheld meter."
            ),
            tags$p(
              "Raw conductivity values are also affected by water temperature. To compare
              measurements across different times and sites, we convert to 'specific
              conductance' which is temperature-compensated to a standard 25°C. This
              conversion is applied in Step 2."
            )
          )
        ),

        # Right column - plots and statistics
        column(
          width = 8,
          tags$h3("Time Series", class = "custom-h2", style = "margin-top: 0;"),
          tags$p(
            "The plots below show conductivity and water temperature over time.
            Look for seasonal patterns, sudden changes, and the relationship between
            conductivity and temperature. Both plots are synchronized for easier comparison.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          plotlyOutput(ns("cond_combined_plot"), height = "500px") %>% withSpinner(),

          hr(),

          tags$h3("Summary Statistics", class = "custom-h2"),
          tags$p(
            "These statistics summarize the full time series. Conductivity values vary
            widely depending on local geology and water sources. Pristine streams may
            have values below 100 uS/cm, while streams in areas with mineral-rich
            geology or agricultural runoff may exceed 500 uS/cm.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          fluidRow(
            column(
              width = 6,
              tags$h4("Conductivity (uS/cm)"),
              verbatimTextOutput(ns("cond_summary"))
            ),
            column(
              width = 6,
              tags$h4("Water Temperature (C)"),
              verbatimTextOutput(ns("cond_temp_summary"))
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Note:"),
          " The raw conductivity values shown here are in microsiemens per centimeter (uS/cm).
          In Step 2, you can apply drift correction (using field reference measurements) and
          temperature compensation to produce specific conductance values that can be
          compared across different conditions.",
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

#' Step 1: Conductivity Review Module - Server
module_step1_cond_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Combined synced plot using subplot
    output$cond_combined_plot <- renderPlotly({
      p1 <- plot_ly(demo_conductivity, x = ~timestamp, y = ~conduct_uScm_U24,
              type = "scattergl", mode = "lines",
              line = list(color = "#FF6F00", width = 1),
              name = "Conductivity",
              hovertemplate = "Cond: %{y:.1f} uS/cm<extra></extra>")

      p2 <- plot_ly(demo_conductivity, x = ~timestamp, y = ~watertemp_C_U24,
              type = "scattergl", mode = "lines",
              line = list(color = "#E65100", width = 1),
              name = "Temperature",
              hovertemplate = "Temp: %{y:.2f} C<extra></extra>")

      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "Conductivity & Temperature - BUGS_WE_01", font = list(size = 14)),
          xaxis = list(title = ""),
          xaxis2 = list(title = "Date"),
          yaxis = list(title = "Conductivity (uS/cm)"),
          yaxis2 = list(title = "Temperature (C)"),
          hovermode = "x unified",
          showlegend = FALSE
        )
    })

    # Summary statistics for conductivity
    output$cond_summary <- renderPrint({
      summary(demo_conductivity$conduct_uScm_U24)
    })

    # Summary statistics for temperature
    output$cond_temp_summary <- renderPrint({
      summary(demo_conductivity$watertemp_C_U24)
    })
  })
}
