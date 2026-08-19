# =============================================================================
# Step 5: Conductivity — three sub-step modules
# =============================================================================

# -----------------------------------------------------------------------------
# 5a  Conductivity QC  (qc_conductivity)
# -----------------------------------------------------------------------------
module_step5_cond_qaqc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 5: Conductivity", class = "custom-h1"),
      tags$h2("5a. Conductivity QC", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Electrical conductivity (EC) measures how well water conducts electricity,
          reported in microsiemens per centimetre (\u00B5S/cm). Higher conductivity means
          more dissolved ions (salts, minerals). It's a quick indicator of water chemistry
          changes \u2014 runoff events, groundwater inputs, or pollution can all shift
          conductivity."
        ),
        tags$p(
          tags$strong("No barometric data needed:"),
          " Unlike water level and DO, conductivity does not require atmospheric pressure
          correction. This makes the pipeline shorter."
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fff8e1; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Automatic QC rules", style = "margin-top: 0;"),
            tags$p("qc_conductivity() applies these checks:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("Spikes"), " \u2014 sudden large jumps in conductivity between
                      consecutive readings (> disturbance_threshold_uScm, default 200)"),
              tags$li(tags$strong("Ice"), " \u2014 water temp below 0.1\u00B0C (sensor may be frozen)"),
              tags$li(tags$strong("Temp range"), " \u2014 temperature outside plausible limits
                      (-2 to 40\u00B0C)"),
              tags$li(tags$strong("Dry sensor"), " \u2014 very low conductivity (< 10 \u00B5S/cm)
                      or air-water temp diff < 2\u00B0C"),
              tags$li(tags$strong("Near-zero temp"), " \u2014 water temp -1 to 0\u00B0C corrected to 0")
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Output columns", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("conduct_uScm_adj"), " \u2014 adjusted conductivity (working column)"),
              tags$li(tags$strong("watertemp_C_adj"), " \u2014 adjusted water temperature"),
              tags$li(tags$strong("FLAG_DISTURBANCE"), " \u2014 conductivity spike detected"),
              tags$li(tags$strong("FLAG_ICE"), " \u2014 temp < 0.1\u00B0C (possible ice)"),
              tags$li(tags$strong("FLAG_RANGE"), " \u2014 temp out of range"),
              tags$li(tags$strong("FLAG_DRY"), " \u2014 sensor appears out of water")
            )
          ),

          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px;",
            tags$h4("What to look for", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Sudden jumps (disturbance, debris on sensor)"),
              tags$li("Flat low readings (sensor out of water)"),
              tags$li("Gradual drift from biofouling")
            )
          )
        ),

        column(
          width = 8,
          tags$h4("qc_conductivity()"),
          tags$p(
            "Runs automatic quality checks on conductivity data and flags suspicious
            values. Returns the data frame with QC columns added.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Run automatic QC on conductivity data
cond_qc <- qc_conductivity(
  input_data                 = cond_data,
  select_station             = \"TUMT_WE_40\",
  temp_low_limit_C           = -2,
  temp_high_limit_C          = 40,
  disturbance_threshold_uScm = 200,
  dry_threshold_uScm         = 10,
  air_water_diff_threshold_C = 2
)

# Summarize flags
cat(\"Total rows:     \", nrow(cond_qc))
cat(\"Spikes:         \", sum(cond_qc$FLAG_DISTURBANCE, na.rm = TRUE))
cat(\"Ice flagged:    \", sum(cond_qc$FLAG_ICE, na.rm = TRUE))
cat(\"Temp out range: \", sum(cond_qc$FLAG_RANGE, na.rm = TRUE))
cat(\"Dry flagged:    \", sum(cond_qc$FLAG_DRY, na.rm = TRUE))

# Visual review
plot_qc_timeseries(
  cond_data      = cond_qc,
  select_station = \"TUMT_WE_40\"
)"
          ),

          tags$h4("Example: Manual removal"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Remove a dry period
cond_qc <- adjust_logger_NA(
  input_data       = cond_qc,
  select_station   = \"TUMT_WE_40\",
  metric           = \"conductivity\",
  reason_to_adjust = \"dry\",
  timestamp_start  = \"2025-05-20 00:00:00\",
  timestamp_end    = \"2025-05-22 00:00:00\",
  manual_note      = \"Sensor exposed during low water\"
)

# Remove a disturbance
cond_qc <- adjust_logger_NA(
  input_data       = cond_qc,
  select_station   = \"TUMT_WE_40\",
  metric           = \"conductivity\",
  reason_to_adjust = \"disturbance\",
  timestamp_start  = \"2025-05-10 12:00:00\",
  timestamp_end    = \"2025-05-10 14:00:00\",
  manual_note      = \"Debris caught on sensor during storm\"
)"
          ),

          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px; color: #333;",
              tags$strong("Placeholder:"),
              " The interactive QC controls (station selector, threshold parameters,
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
          tags$strong("Next:"),
          " Proceed to ", tags$strong("Drift Correction"), " to correct for
          sensor fouling using field reference measurements.",
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

module_step5_cond_qaqc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}


# -----------------------------------------------------------------------------
# 5b  Drift Correction  (wq_drift_correction with metric = "cond")
# -----------------------------------------------------------------------------
module_step5_cond_drift_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 5: Conductivity", class = "custom-h1"),
      tags$h2("5b. Drift Correction", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Conductivity sensors can drift due to biofouling (algae/biofilm coating the
          electrodes), mineral deposits, or calibration drift. Field measurements taken
          with a calibrated handheld meter give us \"truth\" values to correct against."
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
                tags$strong("\"calibration\""), " \u2014 sensor was cleaned and recalibrated.
                Data BEFORE is corrected; data AFTER is trusted."
              ),
              tags$li(
                tags$strong("\"check\""), " \u2014 reference measured but sensor NOT touched.
                Quantifies drift at that point in time."
              ),
              tags$li(
                tags$strong("\"ignore\""), " \u2014 skip this measurement."
              )
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Correction methods", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(
                tags$strong("\"ratio\""), " (default) \u2014 multiplicative correction.
                Correction factor = reference / logger, interpolated linearly from 1.0
                at segment start. Good when drift is proportional."
              ),
              tags$li(
                tags$strong("\"offset\""), " \u2014 additive correction.
                Correction = reference - logger, interpolated from 0 at segment start.
                Good when drift is a constant shift."
              )
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$h4("Output", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("conduct_uScm_adj"), " \u2014 updated with drift correction"),
              tags$li(tags$strong("edit_cond_drift"), " \u2014 TRUE for corrected rows")
            )
          )
        ),

        column(
          width = 8,
          tags$h4("wq_drift_correction() for conductivity"),
          tags$p(
            "Adjusts logged conductivity to match field reference measurements,
            correcting for gradual sensor drift between site visits.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Apply drift correction using field reference measurements
cond_qc <- wq_drift_correction(
  input_data     = cond_qc,
  ref_data_path  = reference_file,     # CSV with field measurements
  select_station = \"TUMT_WE_40\",
  metric         = \"cond\",              # tells function to correct conductivity
  method         = \"ratio\",            # multiplicative correction
  max_ref_gap_h  = 1                   # match logger to ref within 1 hour
)

# Check results
if (\"edit_cond_drift\" %in% names(cond_qc)) {
  drift_count <- sum(cond_qc$edit_cond_drift, na.rm = TRUE)
  cat(\"Rows corrected by drift:\", drift_count, \"of\", nrow(cond_qc))
}

# Show the drift correction log
drift_log <- attr(cond_qc, \"qaqc_log\")
print(drift_log)"
          ),

          tags$div(
            style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px;",
              tags$strong("Important:"),
              " Run drift correction BEFORE temperature compensation, so the
              temperature compensation uses the drift-corrected conductivity values."
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

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next:"),
          " Proceed to ", tags$strong("Temp Compensation"), " to convert electrical
          conductivity to specific conductance (normalized to 25\u00B0C).",
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

module_step5_cond_drift_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}


# -----------------------------------------------------------------------------
# 5c  Temperature Compensation  (convert_conductivity_spc_uScm)
# -----------------------------------------------------------------------------
module_step5_cond_tempcomp_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 5: Conductivity", class = "custom-h1"),
      tags$h2("5c. Temperature Compensation (Specific Conductance)", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "Conductivity varies with water temperature \u2014 warmer water conducts
          electricity better. A reading of 100 \u00B5S/cm at 10\u00B0C represents a different
          amount of dissolved ions than 100 \u00B5S/cm at 25\u00B0C."
        ),
        tags$p(
          "Specific conductance (SPC) removes this temperature effect by normalizing
          to a standard temperature (25\u00B0C), making it possible to compare readings
          across seasons and between sites at different temperatures."
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fff8e1; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Why specific conductance?", style = "margin-top: 0;"),
            tags$p("SPC is the standard for reporting because it lets you:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Compare readings across seasons (cold winter vs warm summer)"),
              tags$li("Compare readings between sites at different temperatures"),
              tags$li("Detect real water chemistry changes vs temperature changes")
            )
          ),

          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("The math (ISO 27888)", style = "margin-top: 0;"),
            tags$p(
              style = "font-size: 13px;",
              "Uses a 5th-order polynomial to model the temperature dependence of
              conductivity. The function calculates what the conductivity WOULD be if
              the water were at exactly 25\u00B0C."
            ),
            tags$p(
              style = "font-size: 13px;",
              tags$strong("Inputs:"),
              " conduct_uScm_adj and watertemp_C_adj"
            ),
            tags$p(
              style = "font-size: 13px;",
              tags$strong("Output:"),
              " spc_uScm_adj (specific conductance in \u00B5S/cm at 25\u00B0C)"
            )
          ),

          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px;",
            tags$h4("Important", style = "margin-top: 0;"),
            tags$p(
              style = "font-size: 13px;",
              "Run this AFTER drift correction, so the temperature compensation uses
              the drift-corrected conductivity values."
            )
          )
        ),

        column(
          width = 8,
          tags$h4("convert_conductivity_spc_uScm()"),
          tags$p(
            "Converts electrical conductivity (EC) to specific conductance (SPC)
            normalized to 25\u00B0C.",
            class = "clean-print"
          ),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Calculate specific conductance (run AFTER drift correction)
cond_qc <- convert_conductivity_spc_uScm(cond_qc)

cat(\"Temperature compensation complete.\")
cat(\"Raw conductivity range:   \",
    round(min(cond_qc$conduct_uScm_adj, na.rm = TRUE), 1), \"to\",
    round(max(cond_qc$conduct_uScm_adj, na.rm = TRUE), 1), \"uS/cm\")
cat(\"Specific conductance range:\",
    round(min(cond_qc$spc_uScm_adj, na.rm = TRUE), 1), \"to\",
    round(max(cond_qc$spc_uScm_adj, na.rm = TRUE), 1), \"uS/cm @ 25C\")

# Compare EC vs SPC at different temperatures
sample_rows <- cond_qc[seq(1, nrow(cond_qc), length.out = 6),
                        c(\"timestamp\", \"watertemp_C_adj\",
                          \"conduct_uScm_adj\", \"spc_uScm_adj\")]
print(sample_rows)"
          ),

          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; font-size: 13px; color: #333;",
              tags$strong("Placeholder:"),
              " The interactive controls and output for running
              convert_conductivity_spc_uScm() will be wired up in a future update."
            )
          ),

          hr(),

          # Recap
          tags$h4("Conductivity Pipeline Recap"),
          tags$div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
            tags$pre(
              style = "margin: 0; font-size: 12px;",
"Step 5a:  qc_conductivity()              \u2192  auto QC + manual adjustments
Step 5b:  wq_drift_correction()            \u2192  drift corrected
Step 5c:  convert_conductivity_spc_uScm() \u2192  specific conductance at 25\u00B0C

Key columns for analysis:
  spc_uScm_adj      \u2014 fully corrected specific conductance (use this)
  conduct_uScm_adj  \u2014 drift-corrected electrical conductivity
  edit_cond_drift   \u2014 TRUE where drift correction was applied"
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(
          tags$strong("Next Step:"),
          " Proceed to ", tags$strong("Step 6: Save Data"), " to export your
          final clean datasets.",
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

module_step5_cond_tempcomp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
