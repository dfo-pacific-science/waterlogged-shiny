#' Step 5: Summarize Data Module - UI
module_step5_summarize_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 5: Summarizing Data", class = "custom-h1"),
      tags$h2("Generate summary statistics and visualizations", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Now that you have clean, QAQC'd data, you can generate summary statistics and
          visualizations to understand patterns and prepare results for reporting. The
          get_logger_data() function makes it easy to load and aggregate your processed
          data at different temporal scales."
        ),
        tags$p(
          "Summaries are useful for reporting annual or seasonal averages, comparing between
          sites, identifying trends over time, and checking for remaining anomalies that
          may indicate issues with the data or interesting environmental events."
        )
      ),

      fluidRow(
        # Left column - explanation
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Temporal aggregation", style = "margin-top: 0;"),
            tags$p(
              "Raw logger data is typically recorded every 15 minutes to 1 hour. For many
              analyses and reports, you'll want to aggregate this to daily or monthly values."
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("Hourly"), " - Original resolution, all data points"),
              tags$li(tags$strong("Daily"), " - Mean, min, max for each day"),
              tags$li(tags$strong("Monthly"), " - Mean, min, max for each month")
            )
          ),

          tags$div(
            style = "background-color: #d4edda; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Multi-station analysis", style = "margin-top: 0;"),
            tags$p(
              "You can load data from all stations at once (select_station = 'all') to
              compare patterns across your monitoring network. This is useful for:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Regional pattern analysis"),
              tags$li("Site-to-site comparisons"),
              tags$li("Identifying outlier sites"),
              tags$li("Watershed-scale summaries")
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px;",
            tags$h4("Processing stage selection", style = "margin-top: 0;"),
            tags$p(
              "The data_processing parameter lets you choose which version of data to load:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("v1.0 / clean"), " - Fully QAQC'd data (recommended)"),
              tags$li(tags$strong("v0.3"), " - Converted but not QAQC'd"),
              tags$li(tags$strong("v0.2"), " - With baro data added"),
              tags$li(tags$strong("v0.1"), " - Raw bound data")
            )
          )
        ),

        # Right column - function details
        column(
          width = 8,
          tags$h4("The get_logger_data() Function"),
          tags$p(
            "Use this function to retrieve and optionally aggregate your processed data:",
            class = "clean-print"
          ),

      tags$h4("Function Parameters"),
      tags$div(
        class = "clean-print",
        tags$table(
          style = "width: 100%; border-collapse: collapse; margin: 15px 0; font-size: 13px;",
          tags$thead(
            tags$tr(style = "background-color: #eee;",
              tags$th(style = "border: 1px solid #ddd; padding: 8px; text-align: left;", "Parameter"),
              tags$th(style = "border: 1px solid #ddd; padding: 8px; text-align: left;", "Description")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("path_to_data")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Path to folder containing clean data files")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("logger_type")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "dissolvedoxygen, waterlevel, or baro")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("data_processing")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "\"clean\" (QAQC'd) or \"raw\" (unprocessed)")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("select_station")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Station code or \"all\" for all stations")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("temporal_scale")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "\"hourly\", \"daily\", or \"monthly\" (daily/monthly return means)")
            )
          )
        )
      ),

          tags$h4("Example R Code"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Get daily summary of water level for all stations
wl_daily <- get_logger_data(
  path_to_data = \"data/waterlevel/2024/clean\",
  logger_type = \"waterlevel\",
  data_processing = \"clean\",
  select_station = \"all\",
  temporal_scale = \"daily\"
)

# Get monthly summary of dissolved oxygen for one station
do_monthly <- get_logger_data(
  path_to_data = \"data/dissolved_oxygen/2024/clean\",
  logger_type = \"dissolvedoxygen\",
  data_processing = \"clean\",
  select_station = \"COOK_WE_40\",
  temporal_scale = \"monthly\"
)"
          )
        )
      ),

      hr(),

      tags$h3("Summary Statistics", class = "custom-h2"),

      tags$div(
        style = "display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap;",
        tags$div(
          style = "flex: 1; min-width: 200px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
          tags$h4("Hourly Data", style = "margin-top: 0; color: #00AFDB;"),
          tags$p("Original resolution", style = "font-size: 12px; color: #666;"),
          tags$ul(style = "font-size: 12px; padding-left: 20px;",
            tags$li("Raw values"),
            tags$li("All QAQC flags"),
            tags$li("Full time series")
          )
        ),
        tags$div(
          style = "flex: 1; min-width: 200px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
          tags$h4("Daily Summaries", style = "margin-top: 0; color: #00AFDB;"),
          tags$p("Aggregated by day", style = "font-size: 12px; color: #666;"),
          tags$ul(style = "font-size: 12px; padding-left: 20px;",
            tags$li("Daily mean"),
            tags$li("Daily min/max"),
            tags$li("Daily range")
          )
        ),
        tags$div(
          style = "flex: 1; min-width: 200px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
          tags$h4("Monthly Summaries", style = "margin-top: 0; color: #00AFDB;"),
          tags$p("Aggregated by month", style = "font-size: 12px; color: #666;"),
          tags$ul(style = "font-size: 12px; padding-left: 20px;",
            tags$li("Monthly mean"),
            tags$li("Monthly extremes"),
            tags$li("Seasonal patterns")
          )
        )
      ),

      tags$h3("Interactive Summary Controls", class = "custom-h2"),
      tags$p(
        "[Wireframe: Summary controls will be implemented here]",
        class = "clean-print",
        style = "color: #999; font-style: italic;"
      ),

      tags$div(
        style = "display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap;",
        tags$div(
          style = "flex: 1;",
          tags$label("Select Station(s)"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
                   "[Multi-select dropdown]")
        ),
        tags$div(
          style = "flex: 1;",
          tags$label("Variable"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
                   "[Water Level / DO / Temperature]")
        ),
        tags$div(
          style = "flex: 1;",
          tags$label("Time Scale"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
                   "[Hourly / Daily / Monthly]")
        )
      ),

      tags$h3("Summary Statistics Table", class = "custom-h2"),
      tags$div(
        style = "border: 1px solid #ddd; padding: 20px; background-color: #f9f9f9; margin: 10px 0;",
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 12px;",
          tags$thead(
            tags$tr(
              tags$th("Station", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("Period", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("n", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("Mean", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("Min", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("Max", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("SD", style = "border: 1px solid #ddd; padding: 8px; background: #eee;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("ALBR_ST_30", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("May-Oct 2024", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("4,380", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("1.25 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("0.45 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("2.15 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("0.32", style = "border: 1px solid #ddd; padding: 8px;")
            ),
            tags$tr(
              tags$td("COOK_WE_40", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("May-Oct 2024", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("4,320", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("0.85 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("0.22 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("1.65 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("0.28", style = "border: 1px solid #ddd; padding: 8px;")
            )
          )
        )
      ),

      tags$h2("Visualizations", class = "custom-h2"),

      tags$h3("Time Series Plot", class = "custom-h2"),
      tags$div(
        style = "border: 2px dashed #ccc; padding: 80px; text-align: center; margin: 20px 0;",
        tags$p("Interactive Time Series", style = "font-weight: bold;"),
        tags$p("Multi-station comparison with zoom and pan", style = "color: #999;"),
        tags$p("(plotly interactive chart)", style = "color: #999;")
      ),

      tags$h3("Daily/Monthly Hydrograph", class = "custom-h2"),
      tags$div(
        style = "border: 2px dashed #ccc; padding: 80px; text-align: center; margin: 20px 0;",
        tags$p("Hydrograph with Range Bands", style = "font-weight: bold;"),
        tags$p("Daily/monthly mean with min-max range shading", style = "color: #999;")
      ),

      tags$h3("Distribution Plots", class = "custom-h2"),
      tags$div(
        style = "display: flex; gap: 20px; margin: 20px 0;",
        tags$div(
          style = "flex: 1; border: 2px dashed #ccc; padding: 60px; text-align: center;",
          tags$p("Histogram", style = "font-weight: bold;"),
          tags$p("Value distribution", style = "color: #999;")
        ),
        tags$div(
          style = "flex: 1; border: 2px dashed #ccc; padding: 60px; text-align: center;",
          tags$p("Box Plot", style = "font-weight: bold;"),
          tags$p("By station/month", style = "color: #999;")
        ),
        tags$div(
          style = "flex: 1; border: 2px dashed #ccc; padding: 60px; text-align: center;",
          tags$p("Seasonal Pattern", style = "font-weight: bold;"),
          tags$p("Monthly averages", style = "color: #999;")
        )
      ),

      tags$h3("Export Options", class = "custom-h2"),
      tags$div(
        style = "margin: 20px 0;",
        tags$div(
          style = "display: inline-block; padding: 12px 25px; background: #007bff; color: white; border-radius: 5px; cursor: pointer; margin-right: 10px;",
          "Download Summary Table (CSV)"
        ),
        tags$div(
          style = "display: inline-block; padding: 12px 25px; background: #17a2b8; color: white; border-radius: 5px; cursor: pointer; margin-right: 10px;",
          "Download Plots (PNG)"
        ),
        tags$div(
          style = "display: inline-block; padding: 12px 25px; background: #28a745; color: white; border-radius: 5px; cursor: pointer;",
          "Generate Summary Report"
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin-top: 30px;",
        tags$p(tags$strong("Workflow Complete!"), " You have successfully processed your water
        logger data from raw files through to clean, summarized results. Your clean data
        files are saved and ready for further analysis or reporting.",
        style = "margin: 0;")
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

#' Step 5: Summarize Data Module - Server
module_step5_summarize_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
