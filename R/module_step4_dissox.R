# =============================================================================
# Step 4: Dissolved Oxygen — four sub-step modules
# =============================================================================

# -----------------------------------------------------------------------------
# 4a  Join Baro Data  (add_nearest_baro with metric = "DO")
# -----------------------------------------------------------------------------
module_step4_do_baro_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 4: Dissolved Oxygen", class = "custom-h1"),
      tags$h2("4a. Join Barometric Compensation Data", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Dissolved oxygen saturation depends on atmospheric pressure. A DO reading
          of 8 mg/L at sea level means something different than 8 mg/L at high
          elevation. By attaching barometric pressure, we can convert the raw mg/L
          reading into percent saturation (%sat) \u2014 a measure that accounts for
          altitude and weather conditions."
        ),
        tags$p(
          "This step is the same as for water level: find the nearest barometric
          station and join its air pressure and temperature readings to each DO timestamp."
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Baro station selection", style = "margin-top: 0;"),
            tags$p("You can select the baro station in two ways:"),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(
                tags$strong("\"auto\""), " \u2014 the function calculates distance
                (Haversine formula) between the DO station and all baro stations,
                picks up to 3 nearest within max_km, calibrates and coalesces them."
              ),
              tags$li(
                tags$strong("\"<station_code>\""), " \u2014 you specify exactly which
                baro station to use. Simpler and avoids calibration overhead."
              )
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
            tags$h4("Why baro matters for DO", style = "margin-top: 0;"),
            tags$p(
              style = "font-size: 13px;",
              "Atmospheric pressure is needed because oxygen saturation depends on the
              partial pressure of oxygen in the air, which varies with elevation and
              weather. The baro data also helps detect dry-sensor conditions by comparing
              air and water temperature."
            )
          )
        ),

        column(
          width = 8,
          tags$h4("add_nearest_baro() for DO"),
          tags$p(
            "Same function as for water level, but with metric = \"DO\".",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Attach barometric data to dissolved oxygen readings
do_with_baro <- add_nearest_baro(
  input_data          = do_data,         # DO data from bind_hobo_files()
  baro_data           = baro_qc,         # QAQC'd baro data from Step 2
  metric              = \"DO\",            # tells function we're processing DO
  baro_site_selection = \"DARL_ST_30\",    # or use \"auto\" for auto-selection
  metadata_path       = metadata_file    # for coordinate lookup
)

do_data <- do_with_baro[[1]]

# Check for gaps in baro coverage
baro_na <- sum(is.na(do_data$airpress_kPa))
cat(\"Missing baro:\", baro_na, \"of\", nrow(do_data), \"rows\")"
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
          " Proceed to ", tags$strong("Convert to %Saturation"), " to calculate
          percent air saturation from DO concentration.",
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

module_step4_do_baro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}


# -----------------------------------------------------------------------------
# 4b  Convert to %Saturation  (convert_do_mgl_percsat)
# -----------------------------------------------------------------------------
module_step4_do_convert_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 4: Dissolved Oxygen", class = "custom-h1"),
      tags$h2("4b. Convert DO mg/L to Percent Saturation", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Percent saturation tells you how much oxygen is in the water relative to
          what the water COULD hold at that temperature and pressure. It's a better
          indicator of ecosystem health than mg/L alone because it accounts for
          physical conditions."
        ),
        tags$p(
          "For example, cold water holds more oxygen than warm water. So 8 mg/L in
          cold water might be only 70% saturated, while 8 mg/L in warm water might
          be 95% saturated. Values around 100% are typical for well-aerated water.
          Supersaturation (>100%) can occur during algae blooms. Low values (<50%)
          may indicate oxygen depletion."
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("The math (Weiss 1970/1974)", style = "margin-top: 0;"),
            tags$ol(
              style = "padding-left: 20px;",
              tags$li("Calculate the theoretical maximum DO at the given temperature
                      and pressure using the Weiss oxygen solubility equation"),
              tags$li("Divide the measured DO by the theoretical maximum"),
              tags$li("Multiply by 100 to get percent")
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Required input data", style = "margin-top: 0;"),
            tags$p("To convert DO to percent saturation, you need:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("DO concentration (mg/L) \u2014 ", tags$code("do_mgl")),
              tags$li("Water temperature (\u00B0C) \u2014 ", tags$code("watertemp_C")),
              tags$li("Atmospheric pressure (kPa) \u2014 ", tags$code("airpress_kPa"),
                      " (from Step 4a)")
            )
          ),

          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px;",
            tags$h4("New column added", style = "margin-top: 0;"),
            tags$p(
              tags$strong("do_percsat"), " \u2014 percent air saturation (%)",
              style = "font-size: 13px;"
            )
          )
        ),

        column(
          width = 8,
          tags$h4("convert_do_mgl_percsat()"),
          tags$p(
            "Calculates percent air saturation from DO concentration using water
            temperature and atmospheric pressure.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Convert DO from mg/L to percent saturation
do_data <- convert_do_mgl_percsat(do_data)

# Check the result
cat(\"DO %sat range:\",
    round(min(do_data$do_percsat, na.rm = TRUE), 1), \"to\",
    round(max(do_data$do_percsat, na.rm = TRUE), 1), \"%\")

# Preview the key columns
head(do_data[, c(\"timestamp\", \"do_mgl\", \"watertemp_C\",
                  \"airpress_kPa\", \"do_percsat\")])"
          ),

          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px; color: #333;",
              tags$strong("Placeholder:"),
              " The interactive controls and output for running convert_do_mgl_percsat()
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
          " Proceed to ", tags$strong("DO QAQC"), " to run automatic quality
          checks on the dissolved oxygen data.",
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

module_step4_do_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}


# -----------------------------------------------------------------------------
# 4c  DO QAQC  (qc_dissox)
# -----------------------------------------------------------------------------
module_step4_do_qaqc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 4: Dissolved Oxygen", class = "custom-h1"),
      tags$h2("4c. Dissolved Oxygen QAQC", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "DO sensors can produce artifacts from logger errors, negative readings, ice
          formation, sensor fouling, or exposure to air (dry). This step catches common
          problems automatically."
        ),
        tags$p(
          tags$strong("Flags vs edits"), " (same pattern as water level):",
          tags$ul(
            style = "padding-left: 20px;",
            tags$li(tags$strong("flag_*"), " = \"we noticed something \u2014 please review\"
                    (data NOT changed)"),
            tags$li(tags$strong("edit_*"), " = \"we changed the data\" (values set to NA or corrected)")
          )
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Automatic QAQC rules", style = "margin-top: 0;"),
            tags$p("qc_dissox() applies these checks:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("DO_ERROR"), " \u2014 error code (-888.88) set to NA"),
              tags$li(tags$strong("NEGATIVE_DO"), " \u2014 DO < -1 mg/L removed; -1 to 0 set to 0"),
              tags$li(tags$strong("OUTLIER_DO"), " \u2014 DO > 21 mg/L removed"),
              tags$li(tags$strong("LOGGER_ICE"), " \u2014 water temp < -1\u00B0C, DO removed"),
              tags$li(tags$strong("FLAG_DRY"), " \u2014 12-point window: %sat \u2265 100 AND
                      air-water temp diff \u2264 2\u00B0C"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 water temp < 0.3\u00B0C")
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Output columns", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("do_mgl_adj"), " \u2014 adjusted DO in mg/L"),
              tags$li(tags$strong("do_percsat_adj"), " \u2014 adjusted DO in %sat"),
              tags$li(tags$strong("watertemp_C_do_adj"), " \u2014 adjusted water temperature"),
              tags$li(tags$strong("FLAG_DRY"), " \u2014 sensor may be in air"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 temperature suggests ice")
            )
          ),

          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px;",
            tags$h4("Tips for DO QAQC", style = "margin-top: 0;"),
            tags$p("Look for periods where:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Water temperature closely tracks air temperature"),
              tags$li("DO jumps to >100% saturation suddenly"),
              tags$li("Water level has dropped significantly")
            ),
            tags$p(
              style = "font-size: 13px;",
              "These patterns often indicate the logger is dry or frozen."
            )
          )
        ),

        column(
          width = 8,
          tags$h4("qc_dissox()"),
          tags$p(
            "Performs preliminary quality control on dissolved oxygen data and produces
            interactive plots for visual assessment.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Run automatic QAQC on dissolved oxygen data
do_qc <- qc_dissox(
  input_data     = do_data,
  select_station = \"COOK_WE_40\"
)

# Summarize the flags
cat(\"Total rows:      \", nrow(do_qc))
cat(\"Logger errors:   \", sum(do_qc$ERROR_DO, na.rm = TRUE))
cat(\"DO outliers:     \", sum(do_qc$OUTLIER_DO, na.rm = TRUE))
cat(\"Ice flagged:     \", sum(do_qc$FLAG_ICE, na.rm = TRUE))
cat(\"Dry flagged:     \", sum(do_qc$FLAG_DRY, na.rm = TRUE))
cat(\"Range flagged:   \", sum(do_qc$FLAG_RANGE, na.rm = TRUE))

# Visual review
plot_qaqc_timeseries(
  do_data        = do_qc,
  baro_data      = baro_qc,
  select_station = \"COOK_WE_40\"
)"
          ),

          tags$h4("Example: Manual removal"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Remove a dry period
do_qc <- adjust_logger_NA(
  input_data       = do_qc,
  select_station   = \"COOK_WE_40\",
  metric           = \"dissolvedoxygen\",
  reason_to_adjust = \"dry\",
  timestamp_start  = \"2025-08-01 00:00:00\",
  timestamp_end    = \"2025-08-15 00:00:00\",
  manual_note      = \"Sensor exposed during low-flow period\"
)

# Remove a disturbance (keep temperature data)
do_qc <- adjust_logger_NA(
  input_data       = do_qc,
  select_station   = \"COOK_WE_40\",
  metric           = \"dissolvedoxygen\",
  reason_to_adjust = \"disturbance\",
  timestamp_start  = \"2025-06-01 12:00:00\",
  timestamp_end    = \"2025-06-01 14:00:00\",
  keep_temp        = TRUE,
  manual_note      = \"Brief maintenance - DO affected, temp OK\"
)"
          ),

          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px; color: #333;",
              tags$strong("Placeholder:"),
              " The interactive QAQC controls (station selector, run button,
              output plots, and manual adjustment tools) will be wired up
              in a future update."
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next:"),
          " Proceed to ", tags$strong("Drift Correction"), " to correct for
          gradual sensor fouling using field reference measurements.",
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

module_step4_do_qaqc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}


# -----------------------------------------------------------------------------
# 4d  Drift Correction  (wq_drift_correction with metric = "do")
# -----------------------------------------------------------------------------
module_step4_do_drift_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 4: Dissolved Oxygen", class = "custom-h1"),
      tags$h2("4d. Drift Correction", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "DO sensors slowly lose accuracy over their deployment. The sensing membrane
          gets fouled (coated with biofilm, sediment, etc.), causing readings to drift
          away from true values. During field visits, a handheld reference instrument
          (e.g., a calibrated YSI) provides a \"truth\" measurement. Drift correction
          uses these reference points to straighten out the logged data."
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Reference event types", style = "margin-top: 0;"),
            tags$p("The reference file classifies each field measurement:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(
                tags$strong("\"calibration\""), " \u2014 sensor was recalibrated during this visit.
                Data BEFORE is corrected; data AFTER is trusted (sensor was just cleaned)."
              ),
              tags$li(
                tags$strong("\"check\""), " \u2014 reference measured but sensor NOT touched.
                Used to quantify drift at that point in time."
              ),
              tags$li(
                tags$strong("\"ignore\""), " \u2014 skip this measurement (e.g., suspect reference)."
              )
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Correction methods", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(
                tags$strong("\"ratio\""), " (default) \u2014 multiplicative correction. Better
                for sensors where drift is proportional (e.g., 10% fouling reduces all
                readings by 10%). This is typical for DO sensors."
              ),
              tags$li(
                tags$strong("\"offset\""), " \u2014 additive correction. Better when drift is
                a constant shift regardless of measurement value."
              )
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("Output", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("do_percsat_adj"), " \u2014 updated with drift correction"),
              tags$li(tags$strong("edit_do_drift"), " \u2014 TRUE for corrected rows")
            )
          )
        ),

        column(
          width = 8,
          tags$h4("wq_drift_correction() for DO"),
          tags$p(
            "Adjusts logged DO to match field reference measurements, correcting for
            gradual sensor drift between site visits.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Apply drift correction using field reference measurements
do_qc <- wq_drift_correction(
  input_data     = do_qc,
  ref_data_path  = reference_file,     # CSV with field measurements
  select_station = \"COOK_WE_40\",
  metric         = \"do\",               # tells function to correct DO
  method         = \"ratio\",            # multiplicative correction
  max_ref_gap_h  = 1                   # match logger to ref within 1 hour
)

# Check results
if (\"edit_do_drift\" %in% names(do_qc)) {
  drift_count <- sum(do_qc$edit_do_drift, na.rm = TRUE)
  cat(\"Rows corrected by drift:\", drift_count, \"of\", nrow(do_qc))
}

# Show the drift correction log
drift_log <- attr(do_qc, \"qaqc_log\")
print(drift_log)"
          ),

          tags$div(
            style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px;",
              tags$strong("Tip:"),
              " After drift correction, always re-plot to verify the adjusted trace
              aligns well with the reference points. The corrected values are stored
              in do_percsat_adj and do_mgl_adj."
            )
          ),

          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px; color: #333;",
              tags$strong("Placeholder:"),
              " The interactive drift correction controls (reference file upload,
              method selection, station selector, and output plots) will be wired up
              in a future update."
            )
          )
        )
      ),

      hr(),

      # Recap
      tags$h3("DO Pipeline Recap", class = "custom-h2"),
      tags$div(
        style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
        tags$pre(
          style = "margin: 0; font-size: 12px;",
"Step 4a:  add_nearest_baro()       \u2192  DO + baro attached
Step 4b:  convert_do_mgl_percsat() \u2192  adds do_percsat column
Step 4c:  qc_dissox()            \u2192  auto QAQC + manual adjustments
Step 4d:  wq_drift_correction()    \u2192  drift corrected

Key columns for analysis:
  do_percsat_adj  \u2014 fully corrected DO in %sat (use this)
  do_mgl_adj      \u2014 fully corrected DO in mg/L
  edit_do_drift   \u2014 TRUE where drift correction was applied"
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next Step:"),
          " Proceed to ", tags$strong("Step 5: Conductivity"), " if you have
          conductivity data, or skip ahead to save your clean data.",
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

module_step4_do_drift_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
