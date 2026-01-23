#' Step 1: Overview Module - UI
module_step1_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Series Overview", class = "custom-h1"),
      tags$h2("Compare all logger data in a single view", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "This overview displays all four data types together for comparison. Viewing all
          data series simultaneously is helpful for understanding the relationships between
          different measurements and for identifying periods of concern that may affect
          multiple loggers."
        ),
        tags$p(
          "For example, a period of very low water levels might coincide with high water
          temperatures, low dissolved oxygen, and increased conductivity. Similarly, a
          cold snap that freezes the water surface might affect all underwater loggers
          at the same time."
        )
      ),

      fluidRow(
        # Left column - guidance and context
        column(
          width = 4,
          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Data Coverage Summary", style = "margin-top: 0;"),
            tags$p(
              "The table below shows the time periods covered by each logger type.
              For barometric compensation to work correctly, the barometric data must
              cover the same time period as the water level and DO data.",
              style = "margin-bottom: 15px;"
            ),
            tags$div(
              style = "border-left: 3px solid #1976D2; padding-left: 10px; margin-bottom: 10px;",
              tags$p(tags$strong("Barometric"), style = "margin-bottom: 0;"),
              tags$p("SLUG_ST_01: Jan - Oct 2024", style = "font-size: 13px; color: #666; margin: 0;")
            ),
            tags$div(
              style = "border-left: 3px solid #2E7D32; padding-left: 10px; margin-bottom: 10px;",
              tags$p(tags$strong("Water Level"), style = "margin-bottom: 0;"),
              tags$p("FROG_WE_01: Jan - Oct 2024", style = "font-size: 13px; color: #666; margin: 0;")
            ),
            tags$div(
              style = "border-left: 3px solid #7B1FA2; padding-left: 10px; margin-bottom: 10px;",
              tags$p(tags$strong("Dissolved Oxygen"), style = "margin-bottom: 0;"),
              tags$p("FROG_WE_01: Apr - Oct 2024", style = "font-size: 13px; color: #666; margin: 0;")
            ),
            tags$div(
              style = "border-left: 3px solid #FF6F00; padding-left: 10px;",
              tags$p(tags$strong("Conductivity"), style = "margin-bottom: 0;"),
              tags$p("BUGS_WE_01: Apr 2024 - Apr 2025", style = "font-size: 13px; color: #666; margin: 0;")
            )
          ),

          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What to check", style = "margin-top: 0;"),
            tags$p(
              "When viewing all series together, look for:",
              style = "margin-bottom: 10px;"
            ),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Coverage gaps"), " - periods where one logger has data but another doesn't"),
              tags$li(tags$strong("Correlated patterns"), " - events that affect multiple parameters simultaneously"),
              tags$li(tags$strong("Timing alignment"), " - ensure all loggers are in the same timezone"),
              tags$li(tags$strong("Seasonal trends"), " - expected patterns across the monitoring period")
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px;",
            tags$h4("Important notes", style = "margin-top: 0;"),
            tags$p(
              "Remember that barometric data must be available for any period where you want
              to convert water level or dissolved oxygen data. If your barometric logger has
              gaps, you may need to fill them with data from a nearby weather station."
            ),
            tags$p(
              "Different sites may have different loggers - in this demo, the conductivity
              logger is at a different site (BUGS_WE_01) than the water level and DO loggers
              (FROG_WE_01)."
            )
          )
        ),

        # Right column - plots and table
        column(
          width = 8,
          tags$h3("All Series Comparison", class = "custom-h2", style = "margin-top: 0;"),
          tags$p(
            "The stacked plots below show all four data types on the same time axis.
            Each plot can be zoomed independently, or you can compare patterns across
            all parameters visually.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          plotlyOutput(ns("overview_combined"), height = "700px") %>% withSpinner(),

          hr(),

          tags$h3("Data Summary Table", class = "custom-h2"),
          tags$p(
            "This table summarizes the key statistics for each parameter. Use it to quickly
            check data ranges and identify any values that seem unusual for your site.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          DT::DTOutput(ns("overview_table")) %>% withSpinner()
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next Step:"),
          " Once you have reviewed all data series and confirmed that the data coverage
          periods are appropriate and no major quality issues are apparent, proceed to
          Step 2: Convert Metrics. In that step, you will apply barometric compensation
          to convert water pressure to depth (metres) and dissolved oxygen to percent
          saturation.",
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

#' Step 1: Overview Module - Server
module_step1_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Combined overview plot - all 4 series stacked
    output$overview_combined <- renderPlotly({
      p1 <- plot_ly(demo_baro, x = ~timestamp, y = ~airpress_kPa_U20,
              type = "scattergl", mode = "lines",
              line = list(color = "#1976D2", width = 1),
              name = "Baro Pressure",
              hovertemplate = "%{y:.2f} kPa<extra></extra>")

      p2 <- plot_ly(demo_waterlevel, x = ~timestamp, y = ~waterpress_kPa_U20,
              type = "scattergl", mode = "lines",
              line = list(color = "#2E7D32", width = 1),
              name = "Water Pressure",
              hovertemplate = "%{y:.2f} kPa<extra></extra>")

      p3 <- plot_ly(demo_dissox, x = ~timestamp, y = ~do_mgl_U26,
              type = "scattergl", mode = "lines",
              line = list(color = "#7B1FA2", width = 1),
              name = "Dissolved O2",
              hovertemplate = "%{y:.2f} mg/L<extra></extra>")

      p4 <- plot_ly(demo_conductivity, x = ~timestamp, y = ~conduct_uScm_U24,
              type = "scattergl", mode = "lines",
              line = list(color = "#FF6F00", width = 1),
              name = "Conductivity",
              hovertemplate = "%{y:.1f} uS/cm<extra></extra>")

      subplot(p1, p2, p3, p4, nrows = 4, shareX = FALSE, titleY = TRUE) %>%
        layout(
          yaxis = list(title = "Baro (kPa)"),
          yaxis2 = list(title = "Water (kPa)"),
          yaxis3 = list(title = "DO (mg/L)"),
          yaxis4 = list(title = "Cond (uS/cm)"),
          hovermode = "x unified",
          showlegend = FALSE,
          margin = list(l = 60, r = 20, t = 20, b = 40)
        )
    })

    # Summary table
    output$overview_table <- DT::renderDT({
      summary_df <- data.frame(
        Parameter = c(
          "Barometric Pressure (kPa)",
          "Air Temperature (C)",
          "Water Pressure (kPa)",
          "Water Temperature - WL (C)",
          "Dissolved Oxygen (mg/L)",
          "Water Temperature - DO (C)",
          "Conductivity (uS/cm)",
          "Water Temperature - Cond (C)"
        ),
        Site = c(
          "SLUG_ST_01", "SLUG_ST_01",
          "FROG_WE_01", "FROG_WE_01",
          "FROG_WE_01", "FROG_WE_01",
          "BUGS_WE_01", "BUGS_WE_01"
        ),
        N = c(
          nrow(demo_baro), nrow(demo_baro),
          nrow(demo_waterlevel), nrow(demo_waterlevel),
          nrow(demo_dissox), nrow(demo_dissox),
          nrow(demo_conductivity), nrow(demo_conductivity)
        ),
        Min = c(
          round(min(demo_baro$airpress_kPa_U20, na.rm = TRUE), 2),
          round(min(demo_baro$airtemp_C_U20, na.rm = TRUE), 2),
          round(min(demo_waterlevel$waterpress_kPa_U20, na.rm = TRUE), 2),
          round(min(demo_waterlevel$watertemp_C_U20, na.rm = TRUE), 2),
          round(min(demo_dissox$do_mgl_U26, na.rm = TRUE), 2),
          round(min(demo_dissox$watertemp_C_U26, na.rm = TRUE), 2),
          round(min(demo_conductivity$conduct_uScm_U24, na.rm = TRUE), 2),
          round(min(demo_conductivity$watertemp_C_U24, na.rm = TRUE), 2)
        ),
        Mean = c(
          round(mean(demo_baro$airpress_kPa_U20, na.rm = TRUE), 2),
          round(mean(demo_baro$airtemp_C_U20, na.rm = TRUE), 2),
          round(mean(demo_waterlevel$waterpress_kPa_U20, na.rm = TRUE), 2),
          round(mean(demo_waterlevel$watertemp_C_U20, na.rm = TRUE), 2),
          round(mean(demo_dissox$do_mgl_U26, na.rm = TRUE), 2),
          round(mean(demo_dissox$watertemp_C_U26, na.rm = TRUE), 2),
          round(mean(demo_conductivity$conduct_uScm_U24, na.rm = TRUE), 2),
          round(mean(demo_conductivity$watertemp_C_U24, na.rm = TRUE), 2)
        ),
        Max = c(
          round(max(demo_baro$airpress_kPa_U20, na.rm = TRUE), 2),
          round(max(demo_baro$airtemp_C_U20, na.rm = TRUE), 2),
          round(max(demo_waterlevel$waterpress_kPa_U20, na.rm = TRUE), 2),
          round(max(demo_waterlevel$watertemp_C_U20, na.rm = TRUE), 2),
          round(max(demo_dissox$do_mgl_U26, na.rm = TRUE), 2),
          round(max(demo_dissox$watertemp_C_U26, na.rm = TRUE), 2),
          round(max(demo_conductivity$conduct_uScm_U24, na.rm = TRUE), 2),
          round(max(demo_conductivity$watertemp_C_U24, na.rm = TRUE), 2)
        )
      )

      DT::datatable(
        summary_df,
        options = list(
          pageLength = 10,
          dom = "t",
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
  })
}
