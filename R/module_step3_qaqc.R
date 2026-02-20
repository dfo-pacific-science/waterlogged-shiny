#' Step 3: QAQC Module - UI
module_step3_qaqc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 3: QAQC", class = "custom-h1"),
      tags$h2("Quality Assurance and Quality Control", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "QAQC (Quality Assurance and Quality Control) is a critical step in water data
          processing. From experience with continuous water monitoring data, it is necessary
          to investigate data from each station individually. Visual investigation of time
          series and automated flags allows you to confirm periods where loggers were dry,
          frozen, or disturbed."
        ),
        tags$p(
          "The waterlogged package provides automated QAQC functions that flag common issues
          using predefined rules, plus manual adjustment functions for correcting specific
          problems you identify during visual review. All adjustments are recorded in QAQC
          columns, creating a transparent audit trail."
        )
      ),

      fluidRow(
        # Left column - overview and philosophy
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("QAQC Philosophy", style = "margin-top: 0;"),
            tags$p(
              "The goal of QAQC is to identify data that may not represent true environmental
              conditions, so that it can be flagged, adjusted, or removed. Common issues include:"
            ),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(tags$strong("Ice"), " - Logger frozen in ice, producing erratic or
                      artificially high readings"),
              tags$li(tags$strong("Dry"), " - Logger out of water, recording air conditions"),
              tags$li(tags$strong("Disturbance"), " - Physical movement of the logger (cable
                      break, sediment burial, beaver activity)"),
              tags$li(tags$strong("Sensor error"), " - Malfunction producing impossible values")
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Using multiple loggers", style = "margin-top: 0;"),
            tags$p(
              "Overlaying data from multiple loggers at the same site makes QAQC much easier.
              For example, you can compare:"
            ),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li("Water temperature from the water level logger vs. air temperature
                      from the barometric logger (if they match, the logger may be dry)"),
              tags$li("Water level vs. dissolved oxygen (if water level drops, DO may be
                      affected by shallow conditions)"),
              tags$li("Temperature from different loggers at the same site (should generally
                      agree unless one is malfunctioning)")
            )
          ),

          tags$div(
            style = "background-color: #d4edda; padding: 20px; border-radius: 5px;",
            tags$h4("Documentation", style = "margin-top: 0;"),
            tags$p(
              "We recommend saving your QAQC R scripts as documentation for future reference
              and transparency. This creates a reproducible record of all adjustments made
              to the data."
            ),
            tags$p(
              "Each adjustment function adds QAQC columns to the data with codes explaining
              what was changed and why. The original raw values are preserved in separate
              columns."
            )
          )
        ),

        # Right column - available functions
        column(
          width = 8,
          tags$h4("Available QAQC Functions"),
          tags$p(
            "The package provides QAQC functions for each logger type. Each function
            applies automated rules and produces interactive plots for visual assessment.",
            class = "clean-print"
          ),

          tags$div(
            style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 15px; margin: 15px 0;",
            tags$div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
              tags$code("waterlevel_qaqc()"),
              tags$p(style = "font-size: 13px; color: #666; margin: 5px 0 0 0;", "Water level loggers")
            ),
            tags$div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
              tags$code("dissox_qaqc()"),
              tags$p(style = "font-size: 13px; color: #666; margin: 5px 0 0 0;", "Dissolved oxygen loggers")
            ),
            tags$div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
              tags$code("conductivity_qaqc()"),
              tags$p(style = "font-size: 13px; color: #666; margin: 5px 0 0 0;", "Conductivity loggers")
            ),
            tags$div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
              tags$code("barometric_qaqc()"),
              tags$p(style = "font-size: 13px; color: #666; margin: 5px 0 0 0;", "Barometric loggers")
            )
          )
        )
      ),

      hr(),

      # ============================================================
      # Part A: Water Level QAQC
      # ============================================================
      tags$h3("Part A: Water Level QAQC", class = "custom-h2"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Automatic QAQC Rules", style = "margin-top: 0;"),
            tags$p("The waterlevel_qaqc() function applies these rules:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("NEGATIVE_WL"), " - Negative water level set to 0"),
              tags$li(tags$strong("LOGGER_ICE"), " - Water temp < -1C removed"),
              tags$li(tags$strong("NEGATIVE_WT"), " - Water temp -1 to 0C set to 0"),
              tags$li(tags$strong("FLAG_DRY"), " - Water pressure ≤ air pressure and temp > 19C"),
              tags$li(tags$strong("FLAG_ICE"), " - Water temp < 0.3C"),
              tags$li(tags$strong("FLAG_DISTURBANCE"), " - Water level changes > 0.1m in 1 hour")
            )
          )
        ),
        column(
          width = 8,
          tags$h4("waterlevel_qaqc() - Automatic flagging"),
          tags$p(
            "This function performs preliminary QAQC, automatically identifying and flagging
            common issues based on the rules shown.",
            class = "clean-print"
          ),

          tags$h4("Example R Code"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Run automatic QAQC on water level data
station_wl_dat2 <- waterlevel_qaqc(
  input_data = station_wl_dat,
  select_station = \"ALBR_ST_30\",
  plot_only = FALSE
)

# Returns a list:
# [[1]] Dataframe with QAQC flags added
# [[2]] Interactive plot for visual assessment

# View the interactive QAQC plot
station_wl_dat2[[2]]"
          )
        )
      ),

      hr(),

      # ============================================================
      # Manual Adjustments Section
      # ============================================================
      tags$h3("Manual Adjustment Functions", class = "custom-h2"),

      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "After reviewing the automated QAQC results, you will often identify specific issues
          that need manual correction. The package provides several functions for common
          adjustment scenarios. Use your mouse to zoom into the QAQC plots to identify the
          exact timestamps for problematic periods."
        )
      ),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Ice spikes", style = "margin-top: 0;"),
            tags$p(
              "During winter, snow and ice accumulation on or around the logger can create
              artificially high pressure readings. These appear as sharp increases in water
              level that don't correspond to actual hydrological events."
            ),
            tags$p(
              "Use ", tags$code("adjust_waterlevel_spike()"), " to flatten these periods.
              The function fits a linear slope between the start and end points."
            )
          ),
          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Cable breaks", style = "margin-top: 0;"),
            tags$p(
              "If a logger cable breaks or the logger shifts position, you'll see a sudden
              jump or drop in water level that persists afterwards. This is different from
              a spike because the offset continues."
            ),
            tags$p(
              "Use ", tags$code("adjust_waterlevel_cablebreak()"), " to correct for sudden
              shifts in the reference level."
            )
          ),
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px;",
            tags$h4("Dry/ice removal", style = "margin-top: 0;"),
            tags$p(
              "For periods where the logger was clearly out of water or frozen, you may
              want to remove the data entirely rather than just flagging it."
            ),
            tags$p(
              "Use ", tags$code("adjust_logger_NA()"), " to remove data and record the
              reason (dry, ice, or other)."
            )
          )
        ),
        column(
          width = 8,
          tags$h4("Example: Removing an ice spike"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# First, identify the spike timestamps from the QAQC plot
# Hover over the plot to see exact timestamps

# Flatten an ice-related spike
station_wl_dat3 <- adjust_waterlevel_spike(
  input_data = station_wl_dat2[[1]],
  select_station = \"ALBR_ST_30\",
  timestamp_start = \"2024-01-11 08:00:00\",
  timestamp_end = \"2024-02-07 07:00:00\",
  reason_to_adjust = \"ice\"
)

# View the result
waterlevel_qaqc(input_data = station_wl_dat3,
                select_station = \"ALBR_ST_30\",
                plot_only = TRUE)"
          ),

          tags$h4("Example: Handling a cable break"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Correct for a sudden shift in water level
station_wl_dat4 <- adjust_waterlevel_cablebreak(
  input_data = station_wl_dat3,
  timestamp_start = \"2024-10-01 18:00:00\",
  timestamp_end = \"2024-10-01 19:00:00\",
  select_station = \"ALBR_ST_30\"
)"
          )
        )
      ),

      hr(),

      # ============================================================
      # Part B: Dissolved Oxygen QAQC
      # ============================================================
      tags$h3("Part B: Dissolved Oxygen QAQC", class = "custom-h2"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fce4ec; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Automatic QAQC Rules", style = "margin-top: 0;"),
            tags$p("The dissox_qaqc() function applies these rules:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("DO_ERROR"), " - Error code (-888.88) set to NA"),
              tags$li(tags$strong("NEGATIVE_DO"), " - DO < -1 mg/L removed"),
              tags$li(tags$strong("OUTLIER_DO"), " - DO > 21 mg/L removed"),
              tags$li(tags$strong("LOGGER_ICE"), " - Water temp < 0C, DO removed"),
              tags$li(tags$strong("FLAG_DRY"), " - Water temp matches air temp, DO > 100%"),
              tags$li(tags$strong("FLAG_ICE"), " - Water temp < 0.3C")
            )
          ),

          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px;",
            tags$h4("Tips for DO QAQC", style = "margin-top: 0;"),
            tags$p(
              "If you have water level data from the same site, include it in the QAQC
              function for better dry/ice detection. Look for periods where:"
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Water temperature closely tracks air temperature"),
              tags$li("DO jumps to >100% saturation suddenly"),
              tags$li("Water level has dropped significantly")
            ),
            tags$p(
              "These patterns often indicate the logger is dry or frozen."
            )
          )
        ),
        column(
          width = 8,
          tags$h4("dissox_qaqc() Function"),
          tags$p(
            "Similar to water level QAQC, this function performs preliminary quality control
            on dissolved oxygen data and produces interactive plots for visual assessment.",
            class = "clean-print"
          ),

          tags$h4("Example R Code"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Run automatic QAQC on dissolved oxygen data
station_do_dat2 <- dissox_qaqc(
  input_data = station_do_dat,
  select_station = \"ALBR_ST_30\"
)

# Include water level data for better detection
station_do_dat2 <- dissox_qaqc(
  input_data = station_do_dat,
  select_station = \"ALBR_ST_30\",
  var_waterlevel_m = \"waterlevel_m_adj\",
  var_watertemp_C_2 = \"watertemp_C_adj\"
)

# Remove a dry period
station_do_dat3 <- adjust_logger_NA(
  input_data = station_do_dat2[[1]],
  select_station = \"ALBR_ST_30\",
  timestamp_start = \"2024-09-09 06:00:00\",
  timestamp_end = \"2024-09-25 21:00:00\",
  logger_to_adjust = \"dissolvedoxygen\",
  reason_to_adjust = \"dry\"
)"
          )
        )
      ),

      hr(),

      # QAQC Summary
      tags$h3("QAQC Flag Legend", class = "custom-h2"),
      tags$div(
        style = "display: flex; gap: 10px; margin: 20px 0; flex-wrap: wrap;",
        tags$div(style = "padding: 8px 15px; background: #28a745; color: white; border-radius: 5px;", "Good Data"),
        tags$div(style = "padding: 8px 15px; background: #17a2b8; color: white; border-radius: 5px;", "FLAG_ICE"),
        tags$div(style = "padding: 8px 15px; background: #ffc107; color: black; border-radius: 5px;", "FLAG_DRY"),
        tags$div(style = "padding: 8px 15px; background: #343a40; color: white; border-radius: 5px;", "FLAG_DISTURBANCE"),
        tags$div(style = "padding: 8px 15px; background: #dc3545; color: white; border-radius: 5px;", "ERROR (removed)")
      ),

      tags$p(
        "Flags indicate suspicious data that should be reviewed. They don't automatically remove
        data - you decide what action to take based on visual inspection and your knowledge
        of site conditions.",
        class = "clean-print"
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(tags$strong("Next Step:"), " Once you are satisfied with your QAQC adjustments
        and have reviewed all flagged periods, proceed to Step 4 to save your clean data files.
        The clean data will include all QAQC columns documenting the adjustments made.",
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

#' Step 3: QAQC Module - Server
module_step3_qaqc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
