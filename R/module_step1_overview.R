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

      # Station selectors at top - one for each logger type
      tags$div(
        style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
        tags$h4("Select Stations for Comparison", style = "margin-top: 0; margin-bottom: 15px;"),
        fluidRow(
          column(
            width = 3,
            tags$div(
              style = "border-left: 3px solid #1976D2; padding-left: 10px;",
              selectInput(ns("baro_station"), "Barometric:",
                choices = NULL,
                width = "100%"
              )
            )
          ),
          column(
            width = 3,
            tags$div(
              style = "border-left: 3px solid #2E7D32; padding-left: 10px;",
              selectInput(ns("wl_station"), "Water Level:",
                choices = NULL,
                width = "100%"
              )
            )
          ),
          column(
            width = 3,
            tags$div(
              style = "border-left: 3px solid #7B1FA2; padding-left: 10px;",
              selectInput(ns("do_station"), "Dissolved Oxygen:",
                choices = NULL,
                width = "100%"
              )
            )
          ),
          column(
            width = 3,
            tags$div(
              style = "border-left: 3px solid #FF6F00; padding-left: 10px;",
              selectInput(ns("cond_station"), "Conductivity:",
                choices = NULL,
                width = "100%"
              )
            )
          )
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
              "The panel below shows the time periods covered by each selected station.
              For barometric compensation to work correctly, the barometric data must
              cover the same time period as the water level and DO data.",
              style = "margin-bottom: 15px;"
            ),
            uiOutput(ns("coverage_summary"))
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
              "Different sites may have different loggers - use the station selectors above
              to compare data from different sites or to view all data from a single site."
            )
          )
        ),

        # Right column - plots and table
        column(
          width = 8,
          tags$h3("All Series Comparison", class = "custom-h2", style = "margin-top: 0;"),
          tags$p(
            "The stacked plots below show all four data types. Each plot updates based on
            the selected station. Use the plots to compare patterns across parameters
            and identify periods of concern.",
            class = "clean-print",
            style = "margin-bottom: 15px;"
          ),
          plotlyOutput(ns("overview_combined"), height = "700px") %>% withSpinner(),

          hr(),

          tags$h3("Data Summary Table", class = "custom-h2"),
          tags$p(
            "This table summarizes the key statistics for each selected station. Use it to quickly
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

    # Update station choices when data changes
    observe({
      baro <- session$userData$rv_raw_data$baro_data; req(baro)
      updateSelectInput(session, "baro_station", choices = names(baro), selected = names(baro)[1])
    })
    observe({
      wl <- session$userData$rv_raw_data$waterlevel_data; req(wl)
      updateSelectInput(session, "wl_station", choices = names(wl), selected = names(wl)[1])
    })
    observe({
      do_list <- session$userData$rv_raw_data$do_data; req(do_list)
      updateSelectInput(session, "do_station", choices = names(do_list), selected = names(do_list)[1])
    })
    observe({
      cond <- session$userData$rv_raw_data$conductivity_data; req(cond)
      updateSelectInput(session, "cond_station", choices = names(cond), selected = names(cond)[1])
    })

    # Reactive data for each logger type based on station selection
    baro_data <- reactive({
      req(input$baro_station)
      req(session$userData$rv_raw_data$baro_data)
      session$userData$rv_raw_data$baro_data[[input$baro_station]]
    })

    wl_data <- reactive({
      req(input$wl_station)
      req(session$userData$rv_raw_data$waterlevel_data)
      session$userData$rv_raw_data$waterlevel_data[[input$wl_station]]
    })

    do_data <- reactive({
      req(input$do_station)
      req(session$userData$rv_raw_data$do_data)
      session$userData$rv_raw_data$do_data[[input$do_station]]
    })

    cond_data <- reactive({
      req(input$cond_station)
      req(session$userData$rv_raw_data$conductivity_data)
      session$userData$rv_raw_data$conductivity_data[[input$cond_station]]
    })

    # Coverage summary display
    output$coverage_summary <- renderUI({
      baro <- baro_data()
      wl <- wl_data()
      do_dat <- do_data()
      cond <- cond_data()

      tags$div(
        tags$div(
          style = "border-left: 3px solid #1976D2; padding-left: 10px; margin-bottom: 10px;",
          tags$p(tags$strong("Barometric"), style = "margin-bottom: 0;"),
          tags$p(paste0(input$baro_station, ": ",
            format(min(baro$timestamp), "%b %Y"), " - ",
            format(max(baro$timestamp), "%b %Y")),
            style = "font-size: 13px; color: #666; margin: 0;")
        ),
        tags$div(
          style = "border-left: 3px solid #2E7D32; padding-left: 10px; margin-bottom: 10px;",
          tags$p(tags$strong("Water Level"), style = "margin-bottom: 0;"),
          tags$p(paste0(input$wl_station, ": ",
            format(min(wl$timestamp), "%b %Y"), " - ",
            format(max(wl$timestamp), "%b %Y")),
            style = "font-size: 13px; color: #666; margin: 0;")
        ),
        tags$div(
          style = "border-left: 3px solid #7B1FA2; padding-left: 10px; margin-bottom: 10px;",
          tags$p(tags$strong("Dissolved Oxygen"), style = "margin-bottom: 0;"),
          tags$p(paste0(input$do_station, ": ",
            format(min(do_dat$timestamp), "%b %Y"), " - ",
            format(max(do_dat$timestamp), "%b %Y")),
            style = "font-size: 13px; color: #666; margin: 0;")
        ),
        tags$div(
          style = "border-left: 3px solid #FF6F00; padding-left: 10px;",
          tags$p(tags$strong("Conductivity"), style = "margin-bottom: 0;"),
          tags$p(paste0(input$cond_station, ": ",
            format(min(cond$timestamp), "%b %Y"), " - ",
            format(max(cond$timestamp), "%b %Y")),
            style = "font-size: 13px; color: #666; margin: 0;")
        )
      )
    })

    # Combined overview plot - all 4 series on single plot with multiple y-axes
    output$overview_combined <- renderPlotly({
      baro <- baro_data()
      wl <- wl_data()
      do_dat <- do_data()
      cond <- cond_data()

      plot_ly() %>%
        # Barometric pressure - left axis 1
        add_trace(data = baro, x = ~timestamp, y = ~airpress_kPa,
                  type = "scattergl", mode = "lines",
                  line = list(color = "#1976D2", width = 1.5),
                  name = paste("Baro -", input$baro_station),
                  yaxis = "y",
                  hovertemplate = "Baro: %{y:.2f} kPa<extra></extra>") %>%
        # Water pressure - left axis 2
        add_trace(data = wl, x = ~timestamp, y = ~waterpress_kPa,
                  type = "scattergl", mode = "lines",
                  line = list(color = "#2E7D32", width = 1.5),
                  name = paste("WL -", input$wl_station),
                  yaxis = "y2",
                  hovertemplate = "Water: %{y:.2f} kPa<extra></extra>") %>%
        # Dissolved oxygen - right axis 1
        add_trace(data = do_dat, x = ~timestamp, y = ~do_mgl,
                  type = "scattergl", mode = "lines",
                  line = list(color = "#7B1FA2", width = 1.5),
                  name = paste("DO -", input$do_station),
                  yaxis = "y3",
                  hovertemplate = "DO: %{y:.2f} mg/L<extra></extra>") %>%
        # Conductivity - right axis 2
        add_trace(data = cond, x = ~timestamp, y = ~conduct_uScm,
                  type = "scattergl", mode = "lines",
                  line = list(color = "#FF6F00", width = 1.5),
                  name = paste("Cond -", input$cond_station),
                  yaxis = "y4",
                  hovertemplate = "Cond: %{y:.1f} uS/cm<extra></extra>") %>%
        layout(
          xaxis = list(
            title = "Date",
            tickangle = 0,
            nticks = 10,
            domain = c(0.08, 0.92)
          ),
          # Left axis 1 - Barometric (outer left)
          yaxis = list(
            title = list(text = "Baro (kPa)", font = list(color = "#1976D2")),
            tickfont = list(color = "#1976D2"),
            side = "left",
            position = 0
          ),
          # Left axis 2 - Water Level (inner left)
          yaxis2 = list(
            title = list(text = "Water (kPa)", font = list(color = "#2E7D32")),
            tickfont = list(color = "#2E7D32"),
            side = "left",
            overlaying = "y",
            position = 0.08
          ),
          # Right axis 1 - DO (inner right)
          yaxis3 = list(
            title = list(text = "DO (mg/L)", font = list(color = "#7B1FA2")),
            tickfont = list(color = "#7B1FA2"),
            side = "right",
            overlaying = "y",
            position = 0.92
          ),
          # Right axis 2 - Conductivity (outer right)
          yaxis4 = list(
            title = list(text = "Cond (uS/cm)", font = list(color = "#FF6F00")),
            tickfont = list(color = "#FF6F00"),
            side = "right",
            overlaying = "y",
            position = 1
          ),
          hovermode = "x unified",
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = 1.05
          ),
          margin = list(l = 80, r = 80, t = 60, b = 50)
        )
    })

    # Summary table - updates based on selected stations
    output$overview_table <- DT::renderDT({
      baro <- baro_data()
      wl <- wl_data()
      do_dat <- do_data()
      cond <- cond_data()

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
          input$baro_station, input$baro_station,
          input$wl_station, input$wl_station,
          input$do_station, input$do_station,
          input$cond_station, input$cond_station
        ),
        N = c(
          nrow(baro), nrow(baro),
          nrow(wl), nrow(wl),
          nrow(do_dat), nrow(do_dat),
          nrow(cond), nrow(cond)
        ),
        Min = c(
          round(min(baro$airpress_kPa, na.rm = TRUE), 2),
          round(min(baro$airtemp_C, na.rm = TRUE), 2),
          round(min(wl$waterpress_kPa, na.rm = TRUE), 2),
          round(min(wl$watertemp_C, na.rm = TRUE), 2),
          round(min(do_dat$do_mgl, na.rm = TRUE), 2),
          round(min(do_dat$watertemp_C, na.rm = TRUE), 2),
          round(min(cond$conduct_uScm, na.rm = TRUE), 2),
          round(min(cond$watertemp_C, na.rm = TRUE), 2)
        ),
        Mean = c(
          round(mean(baro$airpress_kPa, na.rm = TRUE), 2),
          round(mean(baro$airtemp_C, na.rm = TRUE), 2),
          round(mean(wl$waterpress_kPa, na.rm = TRUE), 2),
          round(mean(wl$watertemp_C, na.rm = TRUE), 2),
          round(mean(do_dat$do_mgl, na.rm = TRUE), 2),
          round(mean(do_dat$watertemp_C, na.rm = TRUE), 2),
          round(mean(cond$conduct_uScm, na.rm = TRUE), 2),
          round(mean(cond$watertemp_C, na.rm = TRUE), 2)
        ),
        Max = c(
          round(max(baro$airpress_kPa, na.rm = TRUE), 2),
          round(max(baro$airtemp_C, na.rm = TRUE), 2),
          round(max(wl$waterpress_kPa, na.rm = TRUE), 2),
          round(max(wl$watertemp_C, na.rm = TRUE), 2),
          round(max(do_dat$do_mgl, na.rm = TRUE), 2),
          round(max(do_dat$watertemp_C, na.rm = TRUE), 2),
          round(max(cond$conduct_uScm, na.rm = TRUE), 2),
          round(max(cond$watertemp_C, na.rm = TRUE), 2)
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
