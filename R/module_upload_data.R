#' Upload Data Module - UI
#' Prep: Download data from HOBO loggers
module_upload_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Upload Data", class = "custom-h1"),
      tags$h2("Prep: Download data from HOBO loggers", class = "custom-h2"),

      tags$p(
        "Before processing your data in waterlogged, you need to download raw data
        from your HOBO data loggers using HOBOware software. This section explains
        the data download process and expected file formats.",
        class = "clean-print"
      ),

      tags$h3("Step-by-Step: Downloading Data from HOBOware", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        tags$ol(
          tags$li("Connect shuttle to computer and load in HOBOware"),
          tags$li("We recommend saving file names as: ", tags$code("[VARIABLE]_[SITENAME]_[SERIALNUMBER]_[YYYYMMDD]")),
          tags$li("Bulk export CSVs: ", tags$strong("HOBOware PRO > Tools > Bulk Export"), " > Select folder containing .hobo files")
        )
      ),

      tags$h3("Raw CSV File Structure", class = "custom-h2"),
      tags$p(
        "Exported CSVs from HOBOware will have this structure (not R-friendly by default):",
        class = "clean-print"
      ),

      tags$div(
        class = "clean-print",
        tags$ul(
          tags$li(tags$strong("Row 1:"), " Plot Title (contains serial number)"),
          tags$li(tags$strong("Row 2:"), " Column headers"),
          tags$li(tags$strong("Row 3+:"), " Start of data"),
          tags$li(tags$strong("Column 1:"), " Index number"),
          tags$li(tags$strong("Column 2:"), " DateTime in format mm/dd/yyyy hh:mm"),
          tags$li(tags$strong("Column 3:"), " Abs Pres (kPa) or DO (mg/L)"),
          tags$li(tags$strong("Column 4:"), " Temp (degrees C)"),
          tags$li(tags$strong("Column 5+:"), " Additional logger outputs (Coupler events, etc.)")
        )
      ),

      tags$h4("Example Raw HOBO File"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 11px; overflow-x: auto;",
"Plot Title: 12345678
#, Date Time (GMT-07:00), Abs Pres (kPa), Temp (C), Coupler Detached, ...
1, 10/14/2023 10:00, 91.828, 5.244, Logged, ...
2, 10/14/2023 11:00, 91.838, 5.244, , ...
3, 10/14/2023 12:00, 91.854, 6.674, , ...
4, 10/14/2023 13:00, 91.876, 8.082, , ...
5, 10/14/2023 14:00, 91.906, 9.077, , ..."
      ),

      tags$h3("Required Metadata File", class = "custom-h2"),
      tags$p(
        "To process your logger data, you need a metadata CSV file containing deployment
        information for each logger. This file is essential for matching raw data files
        to monitoring stations and trimming data to deployment periods.",
        class = "clean-print"
      ),

      tags$h4("Required Metadata Columns"),
      tags$div(
        class = "clean-print",
        tags$table(
          style = "width: 100%; border-collapse: collapse; margin: 15px 0;",
          tags$thead(
            tags$tr(style = "background-color: #eee;",
              tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: left;", "Column Name"),
              tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: left;", "Description"),
              tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: left;", "Example")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("sn")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Serial number of logger"),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "12345678")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("site_station_code")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Unique ID for the monitoring station"),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "SLUG_ST_01")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("timestamp_deploy")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Timestamp when logger was deployed"),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "4/16/2023 9:36")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("timestamp_remove")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Timestamp when logger was retrieved (blank if still logging)"),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "10/5/2023 15:12")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("model")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Logger model type"),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "HOBO_U20")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("metric")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Type of measurement"),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "waterlevel, barometric, dissolvedoxygen")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("status")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Current status of logger"),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "logging, removed, lost")
            )
          )
        )
      ),

      tags$h4("Example Metadata File"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 11px; overflow-x: auto;",
"site_station_code, timestamp_deploy, timestamp_remove, model, sn, metric, status
SLUG_ST_01, 4/16/2023 9:36, , HOBO_U20, 12345678, waterlevel, logging
SLUG_ST_01, 4/16/2023 9:38, , HOBO_U20, 22345678, barometric, logging
SLUG_ST_01, 4/16/2023 9:53, , HOBO_U24, 32345678, conductivity, logging
SLUG_ST_01, 4/16/2023 10:04, 10/5/2023 15:12, HOBO_U26, 42345678, dissolvedoxygen, removed
SLUG_ST_02, 4/17/2023 11:15, , HOBO_U20, 52345678, waterlevel, logging
FROG_WE_01, 4/21/2024 8:10, , HOBO_U20, 62345678, barometric, logging"
      ),

      tags$h3("Recommended Folder Structure", class = "custom-h2"),
      tags$p(
        "We recommend organizing your raw data files by logger type, year, and processing stage.
        This structure helps the waterlogged functions automatically organize outputs:",
        class = "clean-print"
      ),

      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace;",
"project_folder/
  data/
    baro/
      2023/
        raw/          <- Raw CSV files from HOBOware
        processed/    <- Created by bind_hobo_files()
      2024/
        raw/
        processed/
    waterlevel/
      2023/
        raw/
        processed/
      2024/
        raw/
        processed/
    dissolved_oxygen/
      2023/
        raw/
        processed/
  metadata/
    logger_deployments.csv    <- Your metadata file"
      ),

      tags$h3("File Upload", class = "custom-h2"),
      tags$p(
        "[Wireframe: File upload interface will be implemented here]",
        class = "clean-print",
        style = "color: #999; font-style: italic;"
      ),

      fluidRow(
        column(
          width = 6,
          tags$div(
            style = "border: 2px dashed #ccc; padding: 40px; text-align: center; margin: 10px;",
            tags$p(tags$strong("Raw CSV Files")),
            tags$p("Upload folder containing raw .csv files from HOBOware", style = "color: #666; font-size: 12px;"),
            tags$p("(Drag and drop or click to browse)", style = "color: #999;")
          )
        ),
        column(
          width = 6,
          tags$div(
            style = "border: 2px dashed #ccc; padding: 40px; text-align: center; margin: 10px;",
            tags$p(tags$strong("Metadata File")),
            tags$p("Upload logger_deployments.csv with deployment info", style = "color: #666; font-size: 12px;"),
            tags$p("(Drag and drop or click to browse)", style = "color: #999;")
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(tags$strong("Tip:"), " Keep CSV files from different logger types (baro, waterlevel, DO)
        in separate folders. This makes it easier to process each type independently.",
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

#' Upload Data Module - Server
module_upload_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
