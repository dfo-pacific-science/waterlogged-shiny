#' Step 2: Convert Raw Measurements Module - UI
module_step2_convert_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 2: Convert Raw Measurements to Usable Metrics", class = "custom-h1"),
      tags$h2("Apply barometric compensation and unit conversions", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "This step converts raw sensor measurements into usable environmental metrics.
          The key conversion is barometric compensation, which removes the atmospheric pressure
          component from water pressure readings to get true water depth."
        ),
        tags$p(
          "HOBO water level loggers are not vented, meaning they read both the pressure of the
          water above them and the atmospheric pressure. Without compensation, changes in
          weather (atmospheric pressure) would appear as changes in water level. To correct
          for this, we must subtract the atmospheric pressure from each water pressure reading."
        )
      ),

      hr(),

      # ============================================================
      # Part A: Add Barometric Data
      # ============================================================
      tags$h3("Part A: Add Barometric Compensation Data", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Why barometric compensation?", style = "margin-top: 0;"),
            tags$p(
              "Atmospheric pressure typically varies by 1-3 kPa over the course of a few days
              as weather systems move through. Since 1 metre of water creates about 9.8 kPa
              of pressure, even small changes in atmospheric pressure can appear as 10-30 cm
              changes in water level if not corrected."
            ),
            tags$p(
              "The add_nearest_baro() function matches each timestamp in your water data
              with the corresponding atmospheric pressure reading from the nearest barometric
              logger."
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What this function does", style = "margin-top: 0;"),
            tags$ol(
              style = "padding-left: 20px;",
              tags$li("Finds the nearest barometric station using your metadata (lat/lon)"),
              tags$li("Matches timestamps between water and baro data"),
              tags$li("Fills gaps using data from the next-nearest baro station if needed"),
              tags$li("Adjusts for altitude differences between baro stations")
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("Output columns added", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("baro_code"), " - which baro station was used"),
              tags$li(tags$strong("airpress_kPa"), " - atmospheric pressure"),
              tags$li(tags$strong("airtemp_C"), " - air temperature"),
              tags$li(tags$strong("airpress_kPa_adj"), " - altitude-adjusted pressure")
            ),
            tags$p(
              style = "margin-top: 15px; font-size: 13px; color: #666;",
              tags$strong("Note:"), " We recommend that the atmospheric pressure logger be
              within 30 km of the water level logger for accurate compensation."
            )
          )
        ),

        # Right column - code examples
        column(
          width = 8,
          tags$h4("The add_nearest_baro() Function"),
          tags$p(
            "Both water level loggers (U20) and dissolved oxygen loggers (U26) require
            barometric compensation. This function matches your water data with the nearest
            barometric station.",
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

          tags$h4("Example R Code"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Add barometric data to water level data
wl_baro_dat <- add_nearest_baro(
  input_data = wl_dat,
  metric = \"water level\",
  baro_data = baro_dat,
  metadata_path = metadat_file
)

# View the result
head(wl_baro_dat)"
          )
        )
      ),

      hr(),

      # ============================================================
      # Part B: Water Level Conversion
      # ============================================================
      tags$h3("Part B: Convert Water Level to Depth (metres)", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("The conversion process", style = "margin-top: 0;"),
            tags$p(
              "Converting pressure to water depth requires two steps:"
            ),
            tags$ol(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Subtract atmospheric pressure"), " - This gives you the pressure
                      from the water column alone"),
              tags$li(tags$strong("Convert pressure to depth"), " - Using the relationship between
                      water pressure and depth (accounting for water density, which varies with
                      temperature)")
            ),
            tags$p(
              "However, this only gives you relative changes in water level. To get actual
              depth or stage, you need at least one manual reference measurement taken during
              the deployment period."
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Reference measurements", style = "margin-top: 0;"),
            tags$p(
              "You need a CSV file with manual water level measurements containing:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("site_station_code"), " - matching your logger station"),
              tags$li(tags$strong("timestamp"), " - when the measurement was taken"),
              tags$li(tags$strong("stage_m"), " or ", tags$strong("depth_m"), " - the water level in metres")
            ),
            tags$p(
              style = "margin-top: 15px;",
              tags$strong("Stage"), " is the water surface elevation relative to a fixed benchmark
              (like a staff gauge). ", tags$strong("Depth"), " is the distance from the logger
              to the water surface."
            )
          ),

          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px;",
            tags$h4("Choosing reference measurements", style = "margin-top: 0;"),
            tags$p(
              "If you have multiple reference measurements, the function will show you how
              well each one aligns with the converted data. If a reference point is way off
              the trend line, it may indicate a measurement error - check your field notes!"
            ),
            tags$p(
              "Use the ", tags$code("select_measurement"), " parameter to try different
              reference points until you find one that produces a good fit."
            )
          )
        ),

        # Right column - code examples
        column(
          width = 8,
          tags$h4("convert_waterlevel_kPa_m() Function"),
          tags$p(
            "This function applies barometric compensation and converts water pressure to depth
            in metres using your reference measurements.",
            class = "clean-print"
          ),

          tags$h4("Example R Code"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Load reference measurements
ref_dat <- read.csv(\"manual_measurements.csv\")

# Convert water level for a specific station
station_wl <- convert_waterlevel_kPa_m(
  input_data = wl_baro_dat,
  select_station = \"ALBR_ST_30\",
  reference_data = ref_dat,
  var_reference_waterlevel_m = \"waterlevel_m_depth\",
  reference_type = \"depth\",
  select_measurement = 1  # Try different values if needed
)

# Returns a list:
# [[1]] dataframe with waterlevel_m column
# [[2]] reference measurements used
# [[3]] plot showing fit to reference points

# Check the plot to verify the conversion
station_wl[[3]]"
          ),

          tags$div(
            class = "clean-print",
            style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin: 15px 0;",
            tags$p(tags$strong("Tip:"), " The output plot shows your converted water level (navy line)
            with reference measurements (orange points). If the orange points don't align with the
            navy line, try a different reference measurement or check your barometric data.",
            style = "margin: 0;")
          )
        )
      ),

      hr(),

      # ============================================================
      # Part C: Dissolved Oxygen Conversion
      # ============================================================
      tags$h3("Part C: Convert Dissolved Oxygen to Percent Saturation", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Why convert to percent saturation?", style = "margin-top: 0;"),
            tags$p(
              "Dissolved oxygen in mg/L tells you the absolute amount of oxygen in the water,
              but this depends heavily on temperature - cold water can hold more oxygen than
              warm water."
            ),
            tags$p(
              "Percent saturation tells you how much oxygen the water contains relative to
              the maximum it could hold at that temperature and pressure. This makes it
              easier to compare DO levels across different conditions."
            ),
            tags$p(
              "Values around 100% are typical for well-aerated water. Supersaturation (>100%)
              can occur during algae blooms. Low values (<50%) may indicate oxygen depletion."
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("Required data", style = "margin-top: 0;"),
            tags$p(
              "To convert DO to percent saturation, you need:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("DO concentration (mg/L)"),
              tags$li("Water temperature (from DO logger)"),
              tags$li("Atmospheric pressure (from baro logger)")
            ),
            tags$p(
              "The atmospheric pressure is needed because oxygen saturation depends on
              the partial pressure of oxygen in the air, which varies with elevation and
              weather."
            )
          )
        ),

        # Right column - code examples
        column(
          width = 8,
          tags$h4("convert_do_mgl_percsat() Function"),
          tags$p(
            "This function calculates percent air saturation from DO concentration using
            water temperature and atmospheric pressure.",
            class = "clean-print"
          ),

          tags$h4("Example R Code"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# First, add barometric data to DO data
do_baro_dat <- add_nearest_baro(
  input_data = do_dat,
  metric = \"dissolvedoxygen\",
  baro_data = baro_dat,
  metadata_path = metadat_file
)

# Convert DO from mg/L to percent saturation
do_converted <- convert_do_mgl_percsat(
  input_data = do_baro_dat
)

# New column added: do_percsat
head(do_converted)"
          )
        )
      ),

      hr(),

      # ============================================================
      # Part D: Conductivity Processing (optional)
      # ============================================================
      tags$h3("Part D: Conductivity Processing (Optional)", class = "custom-h2"),

      fluidRow(
        # Left column - explanatory text
        column(
          width = 4,
          tags$div(
            style = "background-color: #fff8e1; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Additional processing for conductivity", style = "margin-top: 0;"),
            tags$p(
              "Conductivity data has two optional processing steps that can improve data quality:"
            ),
            tags$ol(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Drift correction"), " - Uses field reference measurements to
                      correct for sensor drift over long deployments"),
              tags$li(tags$strong("Temperature compensation"), " - Converts raw conductivity to
                      specific conductance (standardized to 25°C)")
            ),
            tags$p(
              "Specific conductance is the recommended measurement for comparing between sites
              or times because it removes temperature as a variable."
            )
          )
        ),

        # Right column - code examples
        column(
          width = 8,
          tags$h4("Example R Code"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Drift correction using field YSI measurements
cond_corrected <- correct_drift(
  input_data = cond_dat,
  ref_data = ysi_measurements,
  select_station = \"TUMT_WE_40\",
  var_ref_conduct_uScm = \"ysi_conduct_uScm\",
  var_ref_timestamp = \"ysi_timestamp\"
)

# Temperature compensation to get specific conductance
cond_final <- conduct_temp_comp(
  input_data = cond_corrected[[1]]
)

# New column: spc_uScm (specific conductance)"
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(tags$strong("Next Step:"), " Once your conversions look correct, proceed to Step 3 (QAQC)
        to identify and flag data quality issues such as ice formation, dry periods, and sensor errors.",
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
