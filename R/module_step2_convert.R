#' Step 2: Convert Raw Measurements Module - UI
module_step2_convert_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 2: Convert Raw Measurements to Usable Metrics", class = "custom-h1"),

      tags$p(
        "This step converts raw sensor measurements into usable environmental metrics.
        The specific conversions depend on the type of data you are processing.",
        class = "clean-print"
      ),

      tags$h2("Part A: Add Barometric Compensation", class = "custom-h2"),
      tags$h3("The add_nearest_baro() Function", class = "custom-h2"),

      tags$p(
        "Both water level loggers (U20) and dissolved oxygen loggers (U26) require
        barometric compensation. This function matches your water data with the nearest
        barometric station and adds the pressure/temperature data needed for conversion.",
        class = "clean-print"
      ),

      tags$h4("What add_nearest_baro() Does"),
      tags$div(
        class = "clean-print",
        tags$ol(
          tags$li("Finds the nearest barometric station for each water monitoring station (using metadata)"),
          tags$li("Matches barometric pressure timestamps with water logger timestamps"),
          tags$li("Adds barometric pressure and temperature columns to your water data"),
          tags$li("Handles missing baro data by interpolation or using secondary baro stations")
        )
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
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("input_data")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Your bound water level or dissolved oxygen data")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("input_logger_type")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Either waterlevel_U20 or dissolvedoxygen_U26")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("baro_data")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Your bound barometric pressure data")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("metadata")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Metadata file with nearest_baro column for each station")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", tags$code("path_to_output_folder")),
              tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Where to save the output with baro compensation")
            )
          )
        )
      ),

      tags$h4("Example R Code"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Add barometric data to water level data
wl_baro_dat <- add_nearest_baro(
  input_data = wl_dat,
  input_logger_type = \"waterlevel_U20\",
  path_to_output_folder = \"data/waterlevel/2024\",
  baro_data = baro_dat,
  metadata = metadat
)

# View the result - now includes airpress_kPa_U20_adj column
head(wl_baro_dat)"
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(tags$strong("Output columns added:"),
        style = "margin: 0 0 10px 0;"),
        tags$ul(style = "margin: 0;",
          tags$li(tags$code("airpress_kPa_U20_adj"), " - Adjusted barometric pressure"),
          tags$li(tags$code("airtemp_C_U20_adj"), " - Air temperature from baro logger"),
          tags$li(tags$code("baro_qaqc_adj"), " - Notes on any adjustments made"),
          tags$li(tags$code("baro_qaqc_code"), " - QAQC codes for barometric data")
        )
      ),

      tags$hr(),

      tags$h2("Part B: Water Level Conversion", class = "custom-h2"),
      tags$h3("convert_waterlevel_kPa_m() - Convert pressure to water depth", class = "custom-h2"),

      tags$p(
        "Water level loggers measure absolute pressure (water + atmospheric). To get
        actual water level, we must apply a barometric compensation equation to each
        timestamp, then convert the pressure difference to depth in metres using a
        reference measurement.",
        class = "clean-print"
      ),

      tags$h4("What You Need"),
      tags$div(
        class = "clean-print",
        tags$ul(
          tags$li(tags$strong("input_data"), " - Water level data with barometric compensation added"),
          tags$li(tags$strong("reference_data"), " - Manual stage or depth measurements (CSV with site_station_code, timestamp, and depth/stage in metres)"),
          tags$li(tags$strong("select_station"), " - The station code to process")
        )
      ),

      tags$h4("Reference Types"),
      tags$div(
        class = "clean-print",
        tags$table(
          style = "width: 100%; border-collapse: collapse; margin: 15px 0;",
          tags$tr(
            tags$td(style = "padding: 10px; width: 100px;", tags$strong("Stage")),
            tags$td(style = "padding: 10px;", "Water surface elevation relative to a fixed benchmark (e.g., staff gauge reading)")
          ),
          tags$tr(style = "background-color: #f9f9f9;",
            tags$td(style = "padding: 10px;", tags$strong("Depth")),
            tags$td(style = "padding: 10px;", "Water depth measured from logger position to water surface")
          )
        )
      ),

      tags$h4("Example R Code"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Load reference measurements
ref_dat <- read.csv(\"data/manual_measurements.csv\")

# Convert water level for a specific station
station_wl <- convert_waterlevel_kPa_m(
  input_data = wl_baro_dat,
  select_station = \"SLUG_ST_01\",
  reference_data = ref_dat,
  var_reference_waterlevel_m = \"stage_m\",
  var_reference_timestamp = \"stage_timestamp\",
  reference_type = \"stage\",
  select_measurement = 1,  # Use first reference measurement
  path_to_output_folder = \"data/waterlevel/2024\"
)

# Returns a list with:
# [[1]] site_wl: dataframe with converted water level
# [[2]] ref_dat: reference measurements used
# [[3]] plot: visualization for QAQC

# Access the converted data
station_wl_dat <- station_wl[[1]]"
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(tags$strong("Tip:"), " The ", tags$code("select_measurement"), " parameter lets you
        choose which manual measurement to use as the reference. If the converted water level
        doesn't look right, try a different reference measurement.",
        style = "margin: 0;")
      ),

      tags$hr(),

      tags$h2("Part C: Dissolved Oxygen Conversion", class = "custom-h2"),
      tags$h3("convert_do_mgl_percsat() - Convert DO to % saturation", class = "custom-h2"),

      tags$p(
        "Dissolved oxygen loggers (U26) also require barometric compensation to convert
        raw DO readings from mg/L to percent air saturation values.",
        class = "clean-print"
      ),

      tags$h4("Example R Code"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# First, add barometric data to DO data
do_baro_dat <- add_nearest_baro(
  input_data = do_dat,
  input_logger_type = \"dissolvedoxygen_U26\",
  path_to_output_folder = \"data/dissolved_oxygen/2024\",
  baro_data = baro_dat,
  metadata = metadat
)

# Convert DO from mg/L to percent saturation
do_converted <- convert_do_mgl_percsat(
  input_data = do_baro_dat,
  var_watertemp_C = \"watertemp_C_U26\",
  var_airpress_kPa = \"airpress_kPa_U20_adj\",
  var_do_mgl = \"do_mgl_U26\",
  path_to_output_folder = \"data/dissolved_oxygen/2024\"
)

# New column added: do_percsat_U26"
      ),

      tags$h3("Interactive Preview", class = "custom-h2"),
      tags$p(
        "[Wireframe: Interactive controls and data preview will be implemented here]",
        class = "clean-print",
        style = "color: #999; font-style: italic;"
      ),

      tags$div(
        style = "display: flex; gap: 20px; margin: 20px 0;",
        tags$div(
          style = "flex: 1;",
          tags$label("Select Station"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
                   "[Dropdown: SLUG_ST_01, SLUG_ST_02, ...]")
        ),
        tags$div(
          style = "flex: 1;",
          tags$label("Reference Measurement"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
                   "[Dropdown: Measurement 1, 2, 3...]")
        )
      ),

      tags$div(
        style = "border: 2px dashed #ccc; padding: 80px; text-align: center; margin: 20px 0;",
        tags$p("Converted Water Level Time Series", style = "font-weight: bold;"),
        tags$p("Interactive plot showing converted water level (m) over time", style = "color: #999;"),
        tags$p("Reference measurements shown as points for validation", style = "color: #999;")
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(tags$strong("Next Step:"), " If the converted water level aligns well with your
        reference measurements, proceed to Step 3 (QAQC). If the data looks off, try selecting
        a different reference measurement or check your barometric data.",
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

#' Step 2: Convert Raw Measurements Module - Server
module_step2_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
