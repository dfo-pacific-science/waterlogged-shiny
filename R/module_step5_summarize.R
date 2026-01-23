#' Step 5: Summarize Data Module - UI
module_step5_summarize_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 5: Summarizing Data", class = "custom-h1"),

      tags$p(
        "Generate summary statistics and visualizations from your clean data. This step
        helps you understand patterns in your data and prepare results for reporting.",
        class = "clean-print"
      ),

      tags$h2("Loading Clean Data", class = "custom-h2"),
      tags$h3("The get_logger_data() Function", class = "custom-h2"),

      tags$p(
        "Use this function to retrieve processed data by selecting the desired variable,
        station, and temporal scale:",
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
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "dissolvedoxygen_U26, waterlevel_U20, or baro_U20")
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
  logger_type = \"waterlevel_U20\",
  data_processing = \"clean\",
  select_station = \"all\",
  temporal_scale = \"daily\"
)

# Get monthly summary of dissolved oxygen for one station
do_monthly <- get_logger_data(
  path_to_data = \"data/dissolved_oxygen/2024/clean\",
  logger_type = \"dissolvedoxygen_U26\",
  data_processing = \"clean\",
  select_station = \"SLUG_ST_01\",
  temporal_scale = \"monthly\"
)"
      ),

      tags$h2("Summary Statistics", class = "custom-h2"),

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
              tags$td("SLUG_ST_01", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("May-Oct 2024", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("4,380", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("1.25 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("0.45 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("2.15 m", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("0.32", style = "border: 1px solid #ddd; padding: 8px;")
            ),
            tags$tr(
              tags$td("SLUG_ST_02", style = "border: 1px solid #ddd; padding: 8px;"),
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
