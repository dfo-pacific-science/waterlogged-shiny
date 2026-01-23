#' Step 1: Bind Logger Data Module - UI
module_step1_bind_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 1: Bind Logger Data", class = "custom-h1"),
      tags$h2("Bind logger data for each logging station", class = "custom-h2"),

      tags$p(
        "In most cases, there will be data from multiple loggers or downloads for a
        single station. These need to be bound together to create a single data file
        for each station.",
        class = "clean-print"
      ),

      tags$p(
        "Data also needs to be trimmed to the install and removal dates in case loggers
        were activated prior to install, or if they were removed before downloading the
        data (which is typically the case).",
        class = "clean-print"
      ),

      tags$h3("What You Need", class = "custom-h2"),
      tags$div(
        class = "clean-print",
        tags$ol(
          tags$li(
            tags$strong("Folder containing raw .csv files"), " exported from HOBOware",
            tags$ul(
              tags$li("Keep CSV files from different loggers separate in different folders"),
              tags$li("Organize by year and processing stage (e.g., ", tags$code("data/waterlevel/2024/raw"), ")")
            )
          ),
          tags$li(
            tags$strong("Metadata CSV file"), " containing:",
            tags$ul(
              tags$li(tags$code("sn"), " - Serial number of logger"),
              tags$li(tags$code("site_station_code"), " - Unique ID for the monitoring station"),
              tags$li(tags$code("timestamp_deploy"), " - Timestamp of logger deployment"),
              tags$li(tags$code("timestamp_remove"), " - Timestamp of logger removal"),
              tags$li(tags$code("status"), " - Status of logger (logging, lost, removed)")
            )
          )
        )
      ),

      tags$h3("The bind_hobo_files() Function", class = "custom-h2"),
      tags$p(
        "This function will load all .csv files in the raw folder, match loggers with
        metadata, trim data to deployment timestamps, and bind data by station/location and year.",
        class = "clean-print"
      ),

      tags$h4("Function Parameters"),
      tags$div(
        class = "clean-print",
        tags$table(
          style = "width: 100%; border-collapse: collapse; margin: 15px 0;",
          tags$thead(
            tags$tr(style = "background-color: #eee;",
              tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: left;", "Parameter"),
              tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: left;", "Description")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("path_to_raw_folder")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Folder containing raw CSV files from HOBOware")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("path_to_output_folder")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Where to save bound data files (will create year/processed folders)")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("logger_type")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Type of HOBO logger: baro_U20, waterlevel_U20, dissolvedoxygen_U26, conductivity_U24, watertemp_TidbiT, airtemp_TidbiT")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("metadata")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Dataframe containing deployment info for each logger")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("timestamp_format")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Format of timestamps (default: \"%m/%d/%Y %H:%M\")")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("timestamp_timezone")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Timezone (default: \"UTC\" to ignore daylight savings)")
            )
          )
        )
      ),

      tags$h4("Example R Code"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Load metadata
metadat <- read.csv(\"metadata/logger_deployments.csv\")

# Set paths
raw_dir <- \"data/baro/2024/raw\"
out_dir <- \"data/baro\"

# Bind barometric logger files
baro_bound <- bind_hobo_files(
  path_to_raw_folder = raw_dir,
  path_to_output_folder = out_dir,
  logger_type = \"baro_U20\",
  metadata = metadat
)

# The function returns a list with 3 elements:
# [1] Trimmed and compiled dataset of all CSVs
# [2] File summary of CSVs in raw folder
# [3] Summary plots of data by station and timestamp

# Access the bound data
baro_dat <- data.frame(baro_bound[[1]])

# View file summary
baro_bound[[2]]

# View summary plot
baro_bound[[3]]"
      ),

      tags$h3("Select Logger Type", class = "custom-h2"),
      tags$p(
        "[Wireframe: Logger type selector will be implemented here]",
        class = "clean-print",
        style = "color: #999; font-style: italic;"
      ),

      tags$div(
        style = "display: flex; gap: 15px; flex-wrap: wrap; margin: 20px 0;",
        tags$div(
          style = "border: 2px solid #007bff; padding: 20px; border-radius: 5px; flex: 1; min-width: 150px; text-align: center; cursor: pointer;",
          tags$strong("baro_U20", style = "color: #007bff;"),
          tags$p("Barometric Pressure", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
        ),
        tags$div(
          style = "border: 1px solid #ccc; padding: 20px; border-radius: 5px; flex: 1; min-width: 150px; text-align: center; cursor: pointer;",
          tags$strong("waterlevel_U20"),
          tags$p("Water Level", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
        ),
        tags$div(
          style = "border: 1px solid #ccc; padding: 20px; border-radius: 5px; flex: 1; min-width: 150px; text-align: center; cursor: pointer;",
          tags$strong("dissolvedoxygen_U26"),
          tags$p("Dissolved Oxygen", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
        ),
        tags$div(
          style = "border: 1px solid #ccc; padding: 20px; border-radius: 5px; flex: 1; min-width: 150px; text-align: center; cursor: pointer;",
          tags$strong("conductivity_U24"),
          tags$p("Conductivity", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
        )
      ),

      tags$h3("Function Output", class = "custom-h2"),
      tags$p(
        "After running bind_hobo_files(), messages will appear telling you what files were saved:",
        class = "clean-print"
      ),

      tags$pre(
        class = "clean-print",
        style = "background-color: #e8f4e8; padding: 15px; font-family: monospace; font-size: 11px; border-left: 4px solid #28a745;",
"[1] \"Data (v0.1) from FROG_WE_01 added to 2024 folder\"
[1] \"Data (v0.1) from SLUG_ST_01 added to 2023 folder\"
[1] \"Data (v0.1) from SLUG_ST_01 added to 2024 folder\"
[1] \"List returned:
  [1] trimmed and compiled dataset of csvs in raw folder
  [2] file summary of csvs in raw folder
  [3] summary plots of data by site_station_code and timestamp\""
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(tags$strong("Note:"), " The suffix ", tags$code("v0.1"), " indicates this is the first
        version of processed data, where raw data has been trimmed, variables renamed, and data
        saved by calendar year. Other suffixes will be introduced in later processing steps.
        These suffixes are useful for tracking down errors in data processing.",
        style = "margin: 0;")
      ),

      tags$h3("Output Data Preview", class = "custom-h2"),
      tags$p(
        "[Wireframe: Interactive data table will be displayed here after binding]",
        class = "clean-print",
        style = "color: #999; font-style: italic;"
      ),

      tags$div(
        style = "border: 1px solid #ddd; padding: 20px; background-color: #f9f9f9; margin: 10px 0;",
        tags$p("Bound Data Preview", style = "font-weight: bold;"),
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 12px;",
          tags$thead(
            tags$tr(
              tags$th("timestamp", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("airpress_kPa_U20", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("airtemp_C_U20", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("site_station_code", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("sn", style = "border: 1px solid #ddd; padding: 8px; background: #eee;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("2024-05-01 12:00:00", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("91.828", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("5.244", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("SLUG_ST_01", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("12345678", style = "border: 1px solid #ddd; padding: 8px;")
            ),
            tags$tr(
              tags$td("2024-05-01 13:00:00", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("91.838", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("5.244", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("SLUG_ST_01", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("12345678", style = "border: 1px solid #ddd; padding: 8px;")
            )
          )
        )
      ),

      tags$h3("File Summary", class = "custom-h2"),
      tags$p(
        "The second element of the returned list contains a summary of all raw logger files:",
        class = "clean-print"
      ),

      tags$div(
        style = "border: 1px solid #ddd; padding: 20px; background-color: #f9f9f9; margin: 10px 0;",
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 12px;",
          tags$thead(
            tags$tr(
              tags$th("file_name", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("sn", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("timezone", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("data1_type", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("data1_unit", style = "border: 1px solid #ddd; padding: 8px; background: #eee;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("raw_data_hobo_u20_baro1.csv", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("12345678", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("GMT-07:00", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("Abs Pres", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("kPa", style = "border: 1px solid #ddd; padding: 8px;")
            )
          )
        )
      ),

      tags$h3("Summary Plot", class = "custom-h2"),
      tags$p(
        "[Wireframe: Interactive summary plot will be displayed here]",
        class = "clean-print",
        style = "color: #999; font-style: italic;"
      ),

      tags$div(
        style = "border: 2px dashed #ccc; padding: 80px; text-align: center; margin: 20px 0;",
        tags$p("Time Series Summary Plot", style = "font-weight: bold;"),
        tags$p("Shows all bound data by site_station_code over time", style = "color: #999;"),
        tags$p("(Interactive plotly chart)", style = "color: #999;")
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(tags$strong("Next Step:"), " Once you have bound your barometric and water level
        (or dissolved oxygen) data, proceed to Step 2 to apply barometric compensation
        and convert raw measurements to usable metrics.",
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

#' Step 1: Bind Logger Data Module - Server
module_step1_bind_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
