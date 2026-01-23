#' Step 1: Dissolved Oxygen Review Module - UI
module_step1_do_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Dissolved Oxygen Series", class = "custom-h1"),
      tags$h2("Review continuous dissolved oxygen data", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Dissolved oxygen (DO) data is collected from HOBO U26 loggers deployed underwater.
          Dissolved oxygen levels are critical for aquatic life and vary with water temperature,
          time of day, and biological activity. Low DO can stress or kill fish and other
          aquatic organisms."
        ),
        tags$p(
          "DO sensors can be affected by biofouling (organisms growing on the sensor),
          which typically causes a gradual decline in readings over time. The logger may
          also produce erroneous readings when frozen or out of water. Reviewing the raw
          data helps identify these issues before proceeding to analysis."
        )
      ),

      fluidRow(
        # Left column - guidance and context
        column(
          width = 4,
          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Station Information", style = "margin-top: 0;"),
            tags$p(tags$strong("Site: "), "FROG_WE_01"),
            tags$p(tags$strong("Logger: "), "HOBO U26"),
            tags$p(tags$strong("Serial Number: "), "42345678"),
            tags$p(tags$strong("Date Range: "), "2024-04-20 to 2024-10-09")
          ),

          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What to look for", style = "margin-top: 0;"),
            tags$p(
              "When reviewing dissolved oxygen data, look for patterns that indicate
              data quality issues or interesting ecological events:",
              style = "margin-bottom: 10px;"
            ),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Gradual decline"), " - may indicate biofouling on the sensor membrane"),
              tags$li(tags$strong("Supersaturation (>100%)"), " - can occur during algae blooms but sustained high values may indicate sensor issues"),
              tags$li(tags$strong("Error codes (-888.88)"), " - indicates sensor malfunction"),
              tags$li(tags$strong("Negative values"), " - physically impossible, indicates sensor error"),
              tags$li(tags$strong("Very high values (>21 mg/L)"), " - exceeds saturation limits, likely erroneous")
            )
          ),

          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px;",
            tags$h4("Normal patterns", style = "margin-top: 0;"),
            tags$p(
              "Healthy aquatic systems typically show diurnal (daily) patterns in DO.
              Photosynthesis by aquatic plants and algae produces oxygen during daylight
              hours, leading to higher DO in the afternoon. Respiration continues overnight,
              causing DO to drop and reach its minimum in early morning."
            ),
            tags$p(
              "Water temperature and DO are inversely related - colder water can hold more
              dissolved oxygen than warm water. You should see DO generally decrease as
              water temperature increases through the summer months."
            )
          )
        ),

        # Right column - plots and statistics
        column(
          width = 8,
          tags$h3("Time Series", class = "custom-h2", style = "margin-top: 0;"),
          tags$p(
            "The plots below show dissolved oxygen concentration and water temperature over time.
            Look for the expected diurnal patterns and the inverse relationship between DO and
            temperature. Both plots are synchronized for easier comparison.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          plotlyOutput(ns("do_combined_plot"), height = "500px") %>% withSpinner(),

          hr(),

          tags$h3("Summary Statistics", class = "custom-h2"),
          tags$p(
            "These statistics summarize the full time series. Typical freshwater DO values range
            from 5-12 mg/L depending on temperature and biological activity. Values consistently
            below 4 mg/L may indicate hypoxic conditions.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          fluidRow(
            column(
              width = 6,
              tags$h4("Dissolved Oxygen (mg/L)"),
              verbatimTextOutput(ns("do_conc_summary"))
            ),
            column(
              width = 6,
              tags$h4("Water Temperature (C)"),
              verbatimTextOutput(ns("do_temp_summary"))
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Note:"),
          " Dissolved oxygen is currently shown in mg/L (concentration). In Step 2, you can
          convert this to percent air saturation using water temperature and barometric
          pressure. Percent saturation is useful for comparing DO levels across different
          temperatures and elevations.",
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

#' Step 1: Dissolved Oxygen Review Module - Server
module_step1_do_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Combined synced plot using subplot
    output$do_combined_plot <- renderPlotly({
      p1 <- plot_ly(demo_dissox, x = ~timestamp, y = ~do_mgl_U26,
              type = "scattergl", mode = "lines",
              line = list(color = "#7B1FA2", width = 1),
              name = "DO",
              hovertemplate = "DO: %{y:.2f} mg/L<extra></extra>")

      p2 <- plot_ly(demo_dissox, x = ~timestamp, y = ~watertemp_C_U26,
              type = "scattergl", mode = "lines",
              line = list(color = "#C2185B", width = 1),
              name = "Temperature",
              hovertemplate = "Temp: %{y:.2f} C<extra></extra>")

      subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "Dissolved Oxygen & Temperature - FROG_WE_01", font = list(size = 14)),
          xaxis = list(title = ""),
          xaxis2 = list(title = "Date"),
          yaxis = list(title = "DO (mg/L)"),
          yaxis2 = list(title = "Temperature (C)"),
          hovermode = "x unified",
          showlegend = FALSE
        )
    })

    # Summary statistics for DO
    output$do_conc_summary <- renderPrint({
      summary(demo_dissox$do_mgl_U26)
    })

    # Summary statistics for temperature
    output$do_temp_summary <- renderPrint({
      summary(demo_dissox$watertemp_C_U26)
    })
  })
}
