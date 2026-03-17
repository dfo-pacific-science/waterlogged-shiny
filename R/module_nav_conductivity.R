# =============================================================================
# Conductivity Workflow Modules
# =============================================================================
#
# Pipeline for HOBO U24 conductivity loggers:
#   1. Import Data  — bind_hobo_files() for conductivity loggers
#   2. Convert & QC — add_nearest_baro(), conductivity_temp_compensation(),
#                     qc_conductivity()
#
# Note: Conductivity does not require barometric pressure compensation for
# its primary measurement, but baro data provides air temperature which
# helps with dry-sensor detection and is needed for some temp compensation
# workflows.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Import Data
# -----------------------------------------------------------------------------
module_cond_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Import Conductivity Logger Data", class = "custom-h1"),
      tags$h2("Upload raw HOBO U24 conductivity logger files",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Import raw HOBO CSV files from conductivity loggers (HOBO U24).
        These loggers record electrical conductivity (\u00b5S/cm) and water
        temperature (\u00b0C). When both Low Range and Full Range channels
        are present, the appropriate range is automatically selected."
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fff8e1; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("About HOBO U24 Data", style = "margin-top: 0;"),
            tags$p(
              "The U24 measures electrical conductivity (EC) \u2014 how well
              water conducts electricity. Higher conductivity indicates more
              dissolved ions (salts, minerals)."
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$code("conduct_uScm"), " \u2014 raw conductivity (\u00b5S/cm)"),
              tags$li(tags$code("watertemp_C"), " \u2014 water temperature (\u00b0C)"),
              tags$li(tags$code("conduct_source"), " \u2014 which range was used"),
              tags$li(tags$code("site_station_code"), " \u2014 station ID")
            )
          ),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
            tags$h4("Functions Used", style = "margin-top: 0;"),
            tags$code("bind_hobo_files()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Parse raw HOBO CSVs, auto-select conductivity range,
                   match to metadata")
          )
        ),

        column(
          width = 8,
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; margin-bottom: 15px; border-radius: 5px; background: #fafafa;",
            tags$p(tags$strong("Metadata Upload"), style = "color: #888; margin-bottom: 5px;"),
            tags$div(
              style = "border: 1px solid #ddd; padding: 8px 15px; display: inline-block; border-radius: 4px; background: white;",
              "Choose metadata CSV..."
            )
          ),
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; margin-bottom: 15px; border-radius: 5px; background: #fafafa;",
            tags$p(tags$strong("Raw Conductivity Logger Files"), style = "color: #888; margin-bottom: 5px;"),
            tags$div(
              style = "border: 1px solid #ddd; padding: 8px 15px; display: inline-block; border-radius: 4px; background: white;",
              "Choose HOBO U24 CSV files..."
            )
          ),

          tags$div(
            style = "border: 2px dashed #ccc; padding: 50px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("Conductivity Overview Plot"), style = "color: #888;"),
            tags$p("Conductivity (\u00b5S/cm) and water temperature time series",
                   style = "color: #aaa; font-size: 13px;")
          ),
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; border-radius: 5px;",
            tags$p(tags$strong("Data Summary"), style = "color: #888;"),
            tags$p("Stations, date ranges, conductivity range",
                   style = "color: #aaa; font-size: 13px;")
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_cond_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}


# -----------------------------------------------------------------------------
# 2. Convert & QC
# -----------------------------------------------------------------------------
module_cond_convert_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Convert & QC Conductivity", class = "custom-h1"),
      tags$h2("Temperature compensation, barometric join, and quality control",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "This tab combines three processing steps for conductivity data:
        optionally adding barometric data (for air temp), converting raw
        conductivity to specific conductance (normalized to 25\u00b0C), and
        running automated quality checks."
      ),

      hr(),

      # ---- Part A: Add Nearest Baro (optional) ----
      tags$h3("Part A: Add Nearest Barometric Station (Optional)",
              style = "color: #1565c0; margin-bottom: 15px;"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px;",
            tags$h4("Why Baro for Conductivity?", style = "margin-top: 0;"),
            tags$p(
              "Unlike water level and DO, conductivity does not require
              barometric compensation for its primary measurement. However,
              adding baro data provides air temperature which improves
              dry-sensor detection in the QC step."
            ),
            tags$div(
              style = "margin-top: 10px;",
              tags$code("add_nearest_baro()"),
              tags$p(style = "font-size: 12px; color: #666; margin-top: 3px;",
                     "Optional \u2014 adds airtemp_C for dry detection")
            )
          )
        ),
        column(
          width = 8,
          tags$div(
            style = "display: flex; gap: 15px; margin-bottom: 15px;",
            tags$div(
              style = "flex: 1; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Baro Selection"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "Auto (nearest) or [Station Code]")
            ),
            tags$div(
              style = "flex: 1; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Max Distance (km)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "30")
            )
          ),
          tags$div(
            style = "display: inline-block; padding: 10px 25px; background: #1565c0; color: white; border-radius: 5px; cursor: pointer; margin-bottom: 5px;",
            "Add Baro Data"
          ),
          tags$span(style = "margin-left: 15px; color: #999; font-size: 13px;", "(optional)")
        )
      ),

      hr(),

      # ---- Part B: Convert to Specific Conductance ----
      tags$h3("Part B: Convert to Specific Conductance (SPC)",
              style = "color: #e65100; margin-bottom: 15px;"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fff3e0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Why Temperature Compensation?", style = "margin-top: 0;"),
            tags$p(
              "Electrical conductivity varies with water temperature \u2014 warmer
              water conducts better. Specific conductance (SPC) normalizes
              readings to 25\u00b0C using the ISO 27888 standard, enabling
              comparison across seasons and sites."
            ),
            tags$p(style = "font-size: 13px; color: #666;",
              "A reading of 100 \u00b5S/cm at 10\u00b0C represents different
              ionic content than 100 \u00b5S/cm at 25\u00b0C. SPC removes this
              ambiguity."
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("conductivity_temp_compensation()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Normalises raw conductivity to a reference temperature (25\u00b0C) using ",
                   "the adjusted conductivity and water temperature columns. Outputs the ",
                   "data frame with a new ", tags$code("spc_uScm_adj"),
                   " (specific conductance) column.")
          )
        ),
        column(
          width = 8,
          tags$div(
            style = "display: inline-block; padding: 10px 25px; background: #e65100; color: white; border-radius: 5px; cursor: pointer; margin-bottom: 15px;",
            "Convert to Specific Conductance"
          ),
          tags$div(
            style = "border: 2px dashed #ccc; padding: 50px; text-align: center; border-radius: 5px;",
            tags$p(tags$strong("EC vs SPC Comparison Plot"), style = "color: #888;"),
            tags$p("Raw conductivity and SPC with water temperature overlay",
                   style = "color: #aaa; font-size: 13px;")
          )
        )
      ),

      hr(),

      # ---- Part C: QC Conductivity ----
      tags$h3("Part C: QC Conductivity",
              style = "color: #2e7d32; margin-bottom: 15px;"),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Automatic QAQC Rules", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("Spikes"), " \u2014 large jumps between readings"),
              tags$li(tags$strong("Ice"), " \u2014 water temp < 0.1\u00b0C"),
              tags$li(tags$strong("Temp range"), " \u2014 outside -2 to 40\u00b0C"),
              tags$li(tags$strong("Dry sensor"), " \u2014 very low conductivity or
                      air-water temp convergence"),
              tags$li(tags$strong("Near-zero temp"), " \u2014 -1 to 0\u00b0C corrected to 0")
            )
          ),
          tags$div(
            style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
            tags$h4("Follow Along in the R Package (waterlogged)", style = "margin-top: 0;"),
            tags$code("qc_conductivity()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Runs automated range checks and spike detection on conductivity and ",
                   "water temperature. Outputs the data frame with ",
                   tags$code("conduct_uScm_adj"), ", ", tags$code("watertemp_C_adj"),
                   ", and ", tags$code("flag_co_*"), " columns.")
          )
        ),
        column(
          width = 8,
          # QAQC parameters
          tags$h4("QAQC Parameters"),
          tags$div(
            style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; margin-bottom: 15px;",
            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Temp Low (\u00b0C)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "-2")
            ),
            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Temp High (\u00b0C)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "40")
            ),
            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Spike (\u00b5S/cm)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "200")
            ),
            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Dry Thresh (\u00b5S/cm)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "10")
            ),
            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Air-Water Diff (\u00b0C)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "2")
            ),
            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Select Station"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "[All Stations]")
            )
          ),

          tags$div(
            style = "display: inline-block; padding: 10px 25px; background: #2e7d32; color: white; border-radius: 5px; cursor: pointer; margin-bottom: 15px;",
            "Run Conductivity QAQC"
          ),

          # Flag summary
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("Flag Summary Table"), style = "color: #888;"),
            tags$p("Counts of each flag type by station", style = "color: #aaa; font-size: 13px;")
          ),

          # QAQC plot
          tags$div(
            style = "border: 2px dashed #ccc; padding: 60px; text-align: center; border-radius: 5px;",
            tags$p(tags$strong("Conductivity QAQC Plot"), style = "color: #888;"),
            tags$p("Raw vs adjusted conductivity and temperature with flag markers (Plotly)",
                   style = "color: #aaa; font-size: 13px;")
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_cond_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
