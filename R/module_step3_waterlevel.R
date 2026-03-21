# =============================================================================
# Step 3: Water Level — three sub-step modules
# =============================================================================

# -----------------------------------------------------------------------------
# 3a  Join Baro Data  (add_nearest_baro)
# -----------------------------------------------------------------------------
module_step3_wl_baro_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 3: Water Level", class = "custom-h1"),
      tags$h2("3a. Join Barometric Compensation Data", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "HOBO U20 water level loggers measure absolute pressure \u2014 that includes BOTH
          the weight of water above the sensor AND the weight of the atmosphere.
          To get just the water depth, we need to subtract the atmospheric pressure.
          This step pairs each water level timestamp with the corresponding barometric
          reading from the nearest barometric station."
        )
      ),

      hr(),

      fluidRow(
        # Left column - explanation
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("How auto selection works", style = "margin-top: 0;"),
            tags$ol(
              style = "padding-left: 20px;",
              tags$li("Calculates the geographic distance (Haversine formula) between each
                      water level station and every barometric station in the metadata"),
              tags$li("Selects up to 3 nearest baro stations (within 30 km by default)"),
              tags$li("If multiple baro stations overlap in time, calibrates them against
                      the closest one and combines them to fill gaps"),
              tags$li("Prefers the QAQC-adjusted values (airpress_kPa_adj) when available")
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Columns added", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("airpress_kPa"), " \u2014 atmospheric pressure at each timestamp"),
              tags$li(tags$strong("airtemp_C"), " \u2014 air temperature from the baro station"),
              tags$li(tags$strong("baro_site_stn_code"), " \u2014 which baro station provided the value"),
              tags$li(tags$strong("airpress_kPa_adj"), " \u2014 altitude-adjusted pressure")
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px;",
            tags$h4("Note", style = "margin-top: 0;"),
            tags$p(
              style = "font-size: 13px; color: #666;",
              "We recommend that the atmospheric pressure logger be within 30 km of the
              water level logger for accurate compensation."
            )
          )
        ),

        # Right column - code example
        column(
          width = 8,
          tags$h4("add_nearest_baro()"),
          tags$p(
            "This function matches each water level timestamp with barometric pressure
            from the nearest station.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Attach barometric data to water level readings
wl_with_baro <- add_nearest_baro(
  input_data          = level_data,       # water level data from bind_hobo_files()
  baro_data           = baro_qc,          # QAQC'd baro data from Step 2
  metric              = \"water level\",    # tells function what we're processing
  baro_site_selection = \"auto\",           # auto-select nearest baro station
  metadata_path       = metadata_file     # for coordinate lookup
)

# Returns a list:
#   [[1]] = data frame with baro columns added
#   [[2]] = plot showing which baro station was used

wl_data    <- wl_with_baro[[1]]
baro_plot  <- wl_with_baro[[2]]

# Check for timestamps where baro couldn't be matched
baro_na <- sum(is.na(wl_data$airpress_kPa))
cat(\"Missing baro:\", baro_na, \"of\", nrow(wl_data), \"timestamps\")"
          ),

          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px; color: #333;",
              tags$strong("Placeholder:"),
              " The interactive controls and output for running add_nearest_baro()
              will be wired up in a future update."
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next:"),
          " Proceed to ", tags$strong("Convert to Metres"), " to convert pressure
          readings into water depth.",
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

module_step3_wl_baro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}


# -----------------------------------------------------------------------------
# 3b  Convert to Metres  (convert_waterlevel_kPa_m)
# -----------------------------------------------------------------------------
module_step3_wl_convert_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 3: Water Level", class = "custom-h1"),
      tags$h2("3b. Convert Pressure (kPa) to Water Level (metres)", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "This step converts raw pressure readings into actual water depth or stage
          in metres, using physics and a field reference measurement. The logger records
          pressure, but what we care about is how deep the water is."
        )
      ),

      hr(),

      fluidRow(
        # Left column - explanation
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("How the conversion works", style = "margin-top: 0;"),
            tags$ol(
              style = "padding-left: 20px;",
              tags$li("Calculates water density based on temperature (Kell 1975 equation)"),
              tags$li("Converts water pressure to a depth: depth = pressure / (density \u00D7 g)"),
              tags$li("Converts baro pressure to an equivalent depth the same way"),
              tags$li("Subtracts baro depth from water depth to get the water column height"),
              tags$li("Adds a constant \"k\" to anchor the result to a real-world reference
                      (e.g., a staff gauge reading taken during a field visit)")
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Reference measurements", style = "margin-top: 0;"),
            tags$p(
              "The reference file may contain multiple field visits. The ",
              tags$code("select_measurement"), " parameter selects which visit to use as the
              anchor point. The conversion constant \"k\" is calibrated so the logger's
              computed water level matches the reference at that time."
            ),
            tags$p(
              tags$strong("reference_type:"),
              tags$ul(
                style = "padding-left: 20px; font-size: 13px;",
                tags$li(tags$strong("\"stage\""), " \u2014 staff gauge reading (water surface
                        elevation above a fixed point)"),
                tags$li(tags$strong("\"depth\""), " \u2014 direct depth measurement (distance
                        from water surface to sensor)")
              )
            )
          ),

          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px;",
            tags$h4("Choosing reference points", style = "margin-top: 0;"),
            tags$p(
              "If the reference points don't align with the computed line, try a
              different ", tags$code("select_measurement"), " value. A misaligned
              reference point may indicate a measurement error \u2014 check your field notes."
            )
          )
        ),

        # Right column - code example
        column(
          width = 8,
          tags$h4("convert_waterlevel_kPa_m()"),
          tags$p(
            "Applies barometric compensation and converts water pressure to depth
            in metres using your reference measurements.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Convert pressure to water level in metres
converted <- convert_waterlevel_kPa_m(
  input_data         = wl_data,           # water level data with baro attached
  select_station     = \"ALBR_ST_30\",      # process one station at a time
  reference_data     = reference_file,    # manual field measurements CSV
  reference_type     = \"stage\",           # our references are staff gauge readings
  select_measurement = 1,                # use the first valid reference as anchor
  return_plots       = TRUE               # generate the interactive plot
)

# Returns a named list:
#   $site_wl  = data frame with waterlevel_m column added
#   $ref_dat  = the reference measurements used
#   $wl_plot  = interactive plotly chart of WL + reference points

site_wl <- converted$site_wl
ref_dat <- converted$ref_dat
wl_plot <- converted$wl_plot

cat(\"Water level range:\",
    round(min(site_wl$waterlevel_m, na.rm = TRUE), 3), \"to\",
    round(max(site_wl$waterlevel_m, na.rm = TRUE), 3), \"metres\")"
          ),

          tags$div(
            style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px;",
              tags$strong("Tip:"),
              " The output plot overlays the computed water level (blue line) with
              reference measurements (orange dots). If the logger is well-calibrated,
              the dots should sit on or near the line."
            )
          ),

          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px; color: #333;",
              tags$strong("Placeholder:"),
              " The interactive controls for station selection, reference file upload,
              and conversion output will be wired up in a future update."
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next:"),
          " Proceed to ", tags$strong("Water Level QAQC"), " to run automatic quality
          checks on the converted data.",
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

module_step3_wl_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}


# -----------------------------------------------------------------------------
# 3c  Water Level QAQC  (qc_waterlevel)
# -----------------------------------------------------------------------------
module_step3_wl_qaqc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 3: Water Level", class = "custom-h1"),
      tags$h2("3c. Water Level QAQC", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Even after a good conversion, the data may contain artifacts from ice
          formation, the sensor drying out, or physical disturbances (e.g., a flood
          moving the logger). This step runs automatic quality checks and flags
          suspicious values for review."
        ),
        tags$p(
          tags$strong("Important distinction \u2014 \"flag\" vs \"edit\":"),
          tags$ul(
            style = "padding-left: 20px;",
            tags$li(tags$strong("flag_*"), " columns = \"we noticed something, please review\"
                    (data NOT changed, just marked for attention)"),
            tags$li(tags$strong("edit_*"), " columns = \"we changed the data\"
                    (the _adj value was modified, usually set to NA)")
          )
        )
      ),

      hr(),

      fluidRow(
        # Left column - QAQC rules
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Automatic QAQC rules", style = "margin-top: 0;"),
            tags$p("qc_waterlevel() applies these checks:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("NEGATIVE_WL"), " \u2014 Negative water level set to 0"),
              tags$li(tags$strong("LOGGER_ICE"), " \u2014 Water temp < -1\u00B0C removed"),
              tags$li(tags$strong("NEGATIVE_WT"), " \u2014 Water temp -1 to 0\u00B0C set to 0"),
              tags$li(tags$strong("FLAG_DRY"), " \u2014 Water pressure \u2264 air pressure and temp > 19\u00B0C"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 Water temp < 0.3\u00B0C"),
              tags$li(tags$strong("FLAG_DISTURBANCE"), " \u2014 Water level change > 0.1m in 1 hour")
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Output columns", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("waterlevel_m_adj"), " \u2014 adjusted water level (working column)"),
              tags$li(tags$strong("watertemp_C_adj"), " \u2014 adjusted water temperature"),
              tags$li(tags$strong("FLAG_DISTURBANCE"), " \u2014 step change detected"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 temperature suggests ice"),
              tags$li(tags$strong("FLAG_DRY"), " \u2014 sensor may be out of water")
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px;",
            tags$h4("Manual adjustments", style = "margin-top: 0;"),
            tags$p("After reviewing flags, use these functions to correct specific issues:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$code("adjust_logger_NA()"), " \u2014 remove ice/dry periods"),
              tags$li(tags$code("adjust_waterlevel_spike()"), " \u2014 flatten ice spikes"),
              tags$li(tags$code("adjust_WL_offset()"), " \u2014 correct cable breaks"),
              tags$li(tags$code("wl_multipoint_correction()"), " \u2014 fix gradual drift")
            )
          )
        ),

        # Right column - code examples
        column(
          width = 8,
          tags$h4("qc_waterlevel()"),
          tags$p(
            "Performs preliminary QAQC, automatically identifying and flagging common
            issues. Returns the data frame with QAQC columns added.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Run automatic QAQC on the converted water level data
wl_qc <- qc_waterlevel(
  input_data     = site_wl,
  select_station = \"ALBR_ST_30\"
)

# Summarize what was flagged
cat(\"Total rows:       \", nrow(wl_qc))
cat(\"Ice flagged:      \", sum(wl_qc$FLAG_ICE, na.rm = TRUE))
cat(\"Disturbance:      \", sum(wl_qc$FLAG_DISTURBANCE, na.rm = TRUE))
cat(\"Dry flagged:      \", sum(wl_qc$FLAG_DRY, na.rm = TRUE))
cat(\"Temp outlier:     \", sum(wl_qc$OUTLIER_WT, na.rm = TRUE))

# The QAQC log is stored as an attribute
qaqc_log <- attr(wl_qc, \"qaqc_log\")

# Visual review with interactive plot
plot_qaqc_timeseries(
  wl_data        = wl_qc,
  baro_data      = baro_qc,
  select_station = \"ALBR_ST_30\"
)"
          ),

          tags$h4("Example: Manual corrections"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Remove an ice-affected period
wl_qc <- adjust_logger_NA(
  input_data       = wl_qc,
  select_station   = \"ALBR_ST_30\",
  metric           = \"waterlevel\",
  reason_to_adjust = \"ice\",
  timestamp_start  = \"2025-01-15 00:00:00\",
  timestamp_end    = \"2025-03-01 00:00:00\",
  manual_note      = \"Ice confirmed from field visit\"
)

# Flatten an ice spike (linear interpolation)
wl_qc <- adjust_waterlevel_spike(
  input_data       = wl_qc,
  select_station   = \"ALBR_ST_30\",
  timestamp_start  = \"2025-02-11 21:00:00\",
  timestamp_end    = \"2025-02-20 16:00:00\",
  reason_to_adjust = \"ice\"
)"
          ),

          # Flag legend
          tags$h4("QAQC Flag Legend"),
          tags$div(
            style = "display: flex; gap: 10px; margin: 15px 0; flex-wrap: wrap;",
            tags$div(style = "padding: 8px 15px; background: #28a745; color: white; border-radius: 5px;", "Good Data"),
            tags$div(style = "padding: 8px 15px; background: #17a2b8; color: white; border-radius: 5px;", "FLAG_ICE"),
            tags$div(style = "padding: 8px 15px; background: #ffc107; color: black; border-radius: 5px;", "FLAG_DRY"),
            tags$div(style = "padding: 8px 15px; background: #343a40; color: white; border-radius: 5px;", "FLAG_DISTURBANCE"),
            tags$div(style = "padding: 8px 15px; background: #dc3545; color: white; border-radius: 5px;", "ERROR (removed)")
          ),

          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px; color: #333;",
              tags$strong("Placeholder:"),
              " The interactive QAQC controls (station selector, threshold parameters,
              run button, output plots, and manual adjustment tools) will be wired up
              in a future update."
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next Step:"),
          " Once your water level data has been quality-checked, proceed to Step 4
          for dissolved oxygen and conductivity processing, or skip ahead to save
          your clean data.",
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

module_step3_wl_qaqc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
