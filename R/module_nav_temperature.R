# =============================================================================
# Temperature Workflow Modules
# =============================================================================
#
# Workflow for standalone temperature loggers (TidbiT, Pendant) recording
# water or air temperature time series.
#
# This section handles ONLY raw temperature CSV files — not the temperature
# channels embedded in other logger types (those are handled within their
# respective metric workflows: Water Level, Dissolved Oxygen, etc.).
#
# Sub-tabs:
#   1. Import Data          — bind_hobo_files() with metric = "watertemp" or "airtemp"
#   2. Snap Timestamps      — resample_timestamps() to align to regular grid
#   3. Add Nearest Baro     — OVERLAY ONLY: add_nearest_baro() for air temp
#                             comparison — does NOT modify temperature data
#   4. QC Air Temperature   — qc_airtemp() + export
#   5. QC Water Temperature — qc_watertemp() + export
#
# Package functions used:
#   bind_hobo_files()       — parse raw HOBO CSVs (TidbiT, Pendant)
#   resample_timestamps()   — snap timestamps to regular interval grid
#   add_nearest_baro()      — join nearest baro air temp (overlay only)
#   qc_airtemp()            — range-check QAQC for air temperature
#   qc_watertemp()          — ice/dry/negative-temp QAQC for water temperature
#   adjust_logger_NA()      — manual removal of bad data windows
#   write_clean_data()      — export v1.0 CSV (metric = "watertemp" or "airtemp")
#
# Metadata requirements:
#   logger_manager.csv must have metric = "watertemp" or "airtemp" for
#   TidbiT / Pendant deployments.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Import Data
# -----------------------------------------------------------------------------
module_temp_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Import Temperature Logger Data", class = "custom-h1"),
      tags$h2("Upload raw HOBO temperature time series and deployment metadata",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Import raw HOBO CSV files from ", tags$strong("standalone temperature loggers"),
        " (TidbiT v2, Pendant). These loggers record only temperature ",
        "(\u00b0C) \u2014 no pressure, DO, or conductivity. The raw column ",
        "produced is ", tags$code("watertemp_C"), " (for water temperature) ",
        "or ", tags$code("airtemp_C"), " (for air temperature), determined ",
        "by the ", tags$code("metric"), " column in your metadata file."
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(style = "margin: 0;",
          tags$strong("Note:"), " Temperature channels from multi-metric loggers ",
          "(HOBO U20 water temp, U26 water temp, U24 water temp, U20 baro air temp) ",
          "are handled in their respective metric workflows (Water Level, ",
          "Dissolved Oxygen, Conductivity, Barometric Pressure). This tab is only ",
          "for standalone temperature loggers."
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #fff3e0; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Supported Loggers", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li(tags$strong("TidbiT v2"), " \u2014 standalone water or air temperature"),
              tags$li(tags$strong("Pendant"), " \u2014 legacy standalone temperature logger")
            ),
            tags$p(style = "font-size: 13px; color: #666; margin-top: 10px;",
              "The logger type is auto-detected from the HOBO CSV header. ",
              "Whether it records water or air temp is determined by the ",
              tags$code("metric"), " column in your metadata."
            )
          ),

          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Functions Used", style = "margin-top: 0;"),
            tags$code("bind_hobo_files()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Parse raw HOBO CSVs, extract serial number from header, ",
                   "match SN + timestamps to metadata deployments, combine ",
                   "into a single tidy data frame")
          ),

          # Metadata format reference
          tags$div(
            style = "background-color: #e8eaf6; padding: 15px; border-radius: 5px;",
            tags$h4("Metadata Format", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px;",
              "Your ", tags$code("logger_manager.csv"), " must contain these 9 columns:"
            ),
            tags$table(
              style = "width: 100%; font-size: 12px; border-collapse: collapse;",
              tags$tr(
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", tags$code("site_station_code")),
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", "e.g. ALBR_ST_30")
              ),
              tags$tr(
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", tags$code("timestamp_deploy")),
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", "e.g. 4/8/2021 10:00")
              ),
              tags$tr(
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", tags$code("timestamp_remove")),
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", "e.g. 7/26/2022 8:11")
              ),
              tags$tr(
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", tags$code("model")),
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", "tidbit, Pendant")
              ),
              tags$tr(
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", tags$code("sn")),
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", "Serial number (e.g. 20712042)")
              ),
              tags$tr(
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", tags$code("metric")),
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;",
                        tags$strong("watertemp"), " or ", tags$strong("airtemp"))
              ),
              tags$tr(
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", tags$code("status")),
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", "logging, removed, lost")
              ),
              tags$tr(
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", tags$code("latitude")),
                tags$td(style = "padding: 3px 6px; border-bottom: 1px solid #ddd;", "Decimal degrees")
              ),
              tags$tr(
                tags$td(style = "padding: 3px 6px;", tags$code("longitude")),
                tags$td(style = "padding: 3px 6px;", "Decimal degrees")
              )
            )
          )
        ),

        column(
          width = 8,
          # Metadata upload
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; margin-bottom: 15px; border-radius: 5px; background: #fafafa;",
            tags$p(tags$strong("Metadata Upload"), style = "color: #888; margin-bottom: 5px;"),
            tags$p("Upload your logger_manager.csv deployment metadata file",
                   style = "color: #aaa; font-size: 13px;"),
            tags$div(
              style = "border: 1px solid #ddd; padding: 8px 15px; display: inline-block; border-radius: 4px; background: white; margin-top: 8px;",
              "Choose CSV File..."
            )
          ),

          # Sample metadata
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; overflow-x: auto;",
            tags$h4("Example logger_manager.csv (temperature entries)", style = "margin-top: 0;"),
            tags$pre(
              style = "font-size: 11px; margin: 0; white-space: pre; overflow-x: auto;",
"site_station_code,timestamp_deploy,timestamp_remove,model,sn,metric,status,latitude,longitude
ALBR_ST_30,4/8/2021 10:00,7/26/2022 8:11,tidbit,20712042,airtemp,removed,52.589762,-119.10045
AVOL_ST_30,4/9/2021 12:03,,tidbit,20764247,airtemp,logging,52.123456,-119.654321
COOK_WE_40,5/1/2025 10:00,10/15/2025 12:00,tidbit,20712099,watertemp,removed,49.33973,-123.001504
TUMT_WE_40,5/1/2025 10:00,,tidbit,20712101,watertemp,logging,49.33974,-123.001507"
            ),
            tags$div(
              style = "margin-top: 10px;",
              tags$div(
                style = "display: inline-block; padding: 6px 15px; background: #6c757d; color: white; border-radius: 4px; cursor: pointer; font-size: 13px;",
                "Download Sample Metadata CSV"
              )
            )
          ),

          # Raw files upload
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; margin-bottom: 15px; border-radius: 5px; background: #fafafa;",
            tags$p(tags$strong("Raw Temperature Logger Files"), style = "color: #888; margin-bottom: 5px;"),
            tags$p("Upload one or more raw HOBO CSV files (TidbiT / Pendant temperature time series)",
                   style = "color: #aaa; font-size: 13px;"),
            tags$div(
              style = "border: 1px solid #ddd; padding: 8px 15px; display: inline-block; border-radius: 4px; background: white; margin-top: 8px;",
              "Choose CSV Files..."
            )
          ),

          # Bind button
          tags$div(
            style = "display: inline-block; padding: 10px 25px; background: #007bff; color: white; border-radius: 5px; cursor: pointer; margin-bottom: 15px;",
            "Bind & Import Files"
          ),

          # Data preview
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("Data Preview"), style = "color: #888;"),
            tags$p("Table showing first rows of imported data (timestamp, site_station_code, watertemp_C or airtemp_C)",
                   style = "color: #aaa; font-size: 13px;")
          ),

          tags$div(
            style = "border: 2px dashed #ccc; padding: 50px; text-align: center; border-radius: 5px;",
            tags$p(tags$strong("Overview Plot"), style = "color: #888;"),
            tags$p("Interactive temperature time series for all imported stations",
                   style = "color: #aaa; font-size: 13px;")
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_temp_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}


# -----------------------------------------------------------------------------
# 2. Snap Timestamps
# -----------------------------------------------------------------------------
module_temp_snap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Snap Logger Timestamps", class = "custom-h1"),
      tags$h2("Align timestamps to regular intervals for cross-logger comparison",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Different loggers may record at slightly different times even when
        programmed to the same interval. This step rounds timestamps to the
        nearest regular interval (e.g., the nearest 15-minute mark) so that
        data from multiple loggers can be reliably compared by timestamp."
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(style = "margin: 0;",
          tags$strong("Important:"), " This does ", tags$strong("not"),
          " resample or average the data. All original rows are preserved ",
          "\u2014 only the timestamp values are shifted. The original ",
          "timestamps are saved in a ", tags$code("timestamp_raw"), " column."
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Why Snap Timestamps?", style = "margin-top: 0;"),
            tags$p(
              "When overlaying data from multiple loggers (e.g., matching a
              TidbiT water temp with a baro station air temp), timestamps
              must align exactly. A logger programmed to log every hour might
              record at 12:00:03 instead of 12:00:00."
            ),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li("Enables reliable joins across logger types"),
              tags$li("Corrects minor clock drift between loggers"),
              tags$li("Standardizes irregular recording intervals")
            )
          ),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
            tags$h4("Function Used", style = "margin-top: 0;"),
            tags$code("resample_timestamps()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Supported intervals: 5 min, 10 min, 15 min, 30 min, 1 hour. ",
                   "Alignment modes: round (nearest), floor, ceiling.")
          )
        ),

        column(
          width = 8,
          tags$div(
            style = "display: flex; gap: 15px; margin-bottom: 20px; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 15px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Select Station"),
              tags$div(style = "border: 1px solid #ccc; padding: 8px; background: white; border-radius: 4px; margin-top: 5px;",
                       "[All Stations]")
            ),
            tags$div(
              style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 15px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Snap Interval"),
              tags$div(style = "border: 1px solid #ccc; padding: 8px; background: white; border-radius: 4px; margin-top: 5px;",
                       "15 min")
            ),
            tags$div(
              style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 15px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Alignment"),
              tags$div(style = "border: 1px solid #ccc; padding: 8px; background: white; border-radius: 4px; margin-top: 5px;",
                       "round (nearest)")
            )
          ),

          tags$div(
            style = "display: inline-block; padding: 10px 25px; background: #007bff; color: white; border-radius: 5px; cursor: pointer; margin-bottom: 20px;",
            "Snap Timestamps"
          ),

          tags$div(
            style = "border: 2px dashed #ccc; padding: 30px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("Before / After Preview"), style = "color: #888;"),
            tags$p("Table: timestamp_raw vs timestamp, with shift (seconds)",
                   style = "color: #aaa; font-size: 13px;")
          ),
          tags$div(
            style = "border: 2px dashed #ccc; padding: 40px; text-align: center; border-radius: 5px;",
            tags$p(tags$strong("Shift Distribution"), style = "color: #888;"),
            tags$p("Histogram showing how far each timestamp was moved",
                   style = "color: #aaa; font-size: 13px;")
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_temp_snap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}


# -----------------------------------------------------------------------------
# 3. Add Nearest Baro (Overlay Only)
# -----------------------------------------------------------------------------
module_temp_baro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Add Nearest Baro (Overlay)", class = "custom-h1"),
      tags$h2("Attach air temperature from a nearby barometric station for comparison",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "This step joins air temperature readings from the nearest barometric
        logger to your temperature data. The baro data is used ",
        tags$strong("only for visual overlay and comparison"), " \u2014 it ",
        "does not modify your temperature values. Comparing water and air ",
        "temperature side-by-side helps identify dry sensors, ice conditions, ",
        "and sensor errors during the QC step."
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(style = "margin: 0;",
          tags$strong("Overlay only:"), " The baro join adds an ",
          tags$code("airtemp_C"), " column to your data for comparison. ",
          "Your ", tags$code("watertemp_C"), " values are ",
          tags$strong("not changed"), " by this step. The air temperature ",
          "is used downstream by ", tags$code("qc_watertemp()"), " to ",
          "improve dry-sensor detection."
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Why Overlay Air Temp?", style = "margin-top: 0;"),
            tags$p("Comparing air and water temperature helps detect:"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("Dry sensor"), " \u2014 water temp tracks air temp
                      closely (air-water diff < 2\u00b0C)"),
              tags$li(tags$strong("Ice formation"), " \u2014 both temps near or below
                      0\u00b0C simultaneously"),
              tags$li(tags$strong("Sensor malfunction"), " \u2014 water temp diverges
                      unrealistically from air temp")
            ),
            tags$p(style = "font-size: 13px; color: #666; margin-top: 10px;",
              "This step is optional but strongly recommended before running
              water temperature QC, as it enables the air-water comparison
              dry-detection rule."
            )
          ),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Function Used", style = "margin-top: 0;"),
            tags$code("add_nearest_baro()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Finds nearest baro station using GPS coordinates from metadata,
                   matches timestamps, adds airtemp_C column. Also adds
                   airpress_kPa (not used for temperature QC).")
          ),
          tags$div(
            style = "background-color: #fff3cd; padding: 15px; border-radius: 5px;",
            tags$h4("Prerequisite", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; margin: 0;",
              "Barometric data must be imported and QC'd first (in the
              Barometric Pressure workflow). This step uses the QC'd baro
              data to source air temperature values.")
          )
        ),

        column(
          width = 8,
          tags$div(
            style = "display: flex; gap: 15px; margin-bottom: 15px;",
            tags$div(
              style = "flex: 1; border: 1px solid #ddd; padding: 15px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Baro Station Selection"),
              tags$div(style = "border: 1px solid #ccc; padding: 8px; background: white; border-radius: 4px; margin-top: 5px;",
                       "Auto (nearest by GPS)")
            ),
            tags$div(
              style = "flex: 1; border: 1px solid #ddd; padding: 15px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Max Distance (km)"),
              tags$div(style = "border: 1px solid #ccc; padding: 8px; background: white; border-radius: 4px; margin-top: 5px;",
                       "30")
            )
          ),

          tags$div(
            style = "display: inline-block; padding: 10px 25px; background: #17a2b8; color: white; border-radius: 5px; cursor: pointer; margin-bottom: 15px;",
            "Add Baro Overlay"
          ),

          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("Baro Station Match Summary"), style = "color: #888;"),
            tags$p("Which baro station was matched to each temperature station, distance, coverage",
                   style = "color: #aaa; font-size: 13px;")
          ),

          tags$div(
            style = "border: 2px dashed #ccc; padding: 60px; text-align: center; border-radius: 5px;",
            tags$p(tags$strong("Air vs Water Temperature Overlay"), style = "color: #888;"),
            tags$p("Plotly time series: water temp (dark blue) with air temp (light orange) overlay. ",
                   "Periods where they converge may indicate dry sensor conditions.",
                   style = "color: #aaa; font-size: 13px;")
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_temp_baro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}


# -----------------------------------------------------------------------------
# 4. QC Air Temperature
# -----------------------------------------------------------------------------
module_temp_qc_air_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("QC Air Temperature", class = "custom-h1"),
      tags$h2("Quality control for standalone air temperature loggers",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Run automated quality checks on air temperature data from TidbiT or ",
        "Pendant loggers deployed in air (metadata metric = \"airtemp\"). ",
        "Values outside the specified range are set to NA in the adjusted ",
        "column (", tags$code("airtemp_C_adj"), "). The raw column (",
        tags$code("airtemp_C"), ") is never modified."
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("QAQC Rules", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("Range check"), " \u2014 air temp outside [",
                      tags$code("temp_low_limit"), ", ",
                      tags$code("temp_high_limit"), "] set to NA"),
              tags$li(tags$strong("edit_temp_range"), " \u2014 TRUE where values were removed")
            ),
            tags$p(style = "font-size: 13px; color: #666; margin-top: 10px;",
              "The function creates ", tags$code("airtemp_C_adj"), " (copy of raw, ",
              "with out-of-range values set to NA) and ", tags$code("edit_temp_range"),
              " (boolean flag)."
            )
          ),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Functions Used", style = "margin-top: 0;"),
            tags$code("qc_airtemp()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Automated range-check QAQC for air temperature"),
            tags$code("adjust_logger_NA()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Manual removal of bad data periods (ice, disturbance)"),
            tags$code("write_clean_data()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Export v1.0 CSV with metric = \"airtemp\"")
          ),
          tags$div(
            style = "background-color: #fff3cd; padding: 15px; border-radius: 5px;",
            tags$h4("Output Columns", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 12px; margin-bottom: 0;",
              tags$li(tags$code("airtemp_C"), " \u2014 raw (never modified)"),
              tags$li(tags$code("airtemp_C_adj"), " \u2014 adjusted"),
              tags$li(tags$code("edit_temp_range"), " \u2014 TRUE where changed")
            )
          )
        ),

        column(
          width = 8,
          # QAQC parameters
          tags$h4("QAQC Parameters"),
          tags$div(
            style = "display: flex; gap: 15px; margin-bottom: 15px; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Select Station"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "[Station Dropdown]")
            ),
            tags$div(
              style = "flex: 1; min-width: 120px; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Min Temp (\u00b0C)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "-40")
            ),
            tags$div(
              style = "flex: 1; min-width: 120px; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Max Temp (\u00b0C)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "40")
            )
          ),

          tags$div(
            style = "display: inline-block; padding: 10px 25px; background: #28a745; color: white; border-radius: 5px; cursor: pointer; margin-bottom: 20px;",
            "Run Air Temp QAQC"
          ),

          # Flag summary
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("QAQC Summary"), style = "color: #888;"),
            tags$p("Total rows, values flagged out-of-range, NAs before/after",
                   style = "color: #aaa; font-size: 13px;")
          ),

          # QAQC plot
          tags$div(
            style = "border: 2px dashed #ccc; padding: 60px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("Air Temperature QAQC Plot"), style = "color: #888;"),
            tags$p("Raw (light) vs adjusted (dark) air temperature with edit_temp_range markers",
                   style = "color: #aaa; font-size: 13px;")
          ),

          # Manual adjustment section
          tags$div(
            style = "border-left: 4px solid #e65100; padding: 15px 20px; margin-bottom: 20px; background: #f8f9fa; border-radius: 0 5px 5px 0;",
            tags$h4("Manual Adjustments", style = "margin-top: 0; color: #e65100;"),
            tags$p(style = "font-size: 13px;",
              "Use ", tags$code("adjust_logger_NA()"), " to manually remove data ",
              "for periods identified in the plot (e.g., sensor in direct ",
              "sunlight, sensor covered by snow)."
            ),
            tags$div(
              style = "display: flex; gap: 15px; flex-wrap: wrap; margin-top: 10px;",
              tags$div(
                style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: white;",
                tags$label("Start Timestamp"), "[datetime picker]"
              ),
              tags$div(
                style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: white;",
                tags$label("End Timestamp"), "[datetime picker]"
              ),
              tags$div(
                style = "flex: 1; min-width: 120px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: white;",
                tags$label("Reason"), "[disturbance]"
              )
            ),
            tags$div(
              style = "display: inline-block; padding: 8px 20px; background: #e65100; color: white; border-radius: 4px; cursor: pointer; margin-top: 10px;",
              "Remove Period"
            )
          ),

          hr(),

          # ---- Export / Download section ----
          tags$h4("Export Clean Data"),
          tags$p(style = "font-size: 13px; color: #666;",
            "Download the QAQC'd air temperature data as a v1.0 CSV. ",
            "Columns exported: ", tags$code("site_station_code"), ", ",
            tags$code("timestamp"), ", ", tags$code("airtemp_C"), ", ",
            tags$code("airtemp_C_adj"), ", plus all ", tags$code("edit_*"),
            " flag columns."
          ),
          tags$div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            tags$div(
              style = "display: inline-block; padding: 10px 25px; background: #007bff; color: white; border-radius: 5px; cursor: pointer;",
              "Download Clean CSV"
            ),
            tags$div(
              style = "display: inline-block; padding: 10px 25px; background: #17a2b8; color: white; border-radius: 5px; cursor: pointer;",
              "Download QAQC Log"
            )
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_temp_qc_air_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}


# -----------------------------------------------------------------------------
# 5. QC Water Temperature
# -----------------------------------------------------------------------------
module_temp_qc_water_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("QC Water Temperature", class = "custom-h1"),
      tags$h2("Quality control for standalone water temperature loggers",
              class = "custom-h2"),

      tags$p(
        class = "clean-print",
        "Run automated quality checks on water temperature from TidbiT ",
        "loggers deployed in water (metadata metric = \"watertemp\"). ",
        "The function detects ice conditions, dry sensor exposure, and ",
        "impossible negative values. If air temperature was added via the ",
        "baro overlay step, the dry-detection rule uses the air-water ",
        "temperature comparison for improved accuracy."
      ),

      hr(),

      fluidRow(
        column(
          width = 4,
          tags$div(
            style = "background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("QAQC Rules", style = "margin-top: 0;"),
            tags$ul(
              style = "padding-left: 20px; font-size: 13px;",
              tags$li(tags$strong("edit_temp_lt_minus1"), " \u2014 temp < -1\u00b0C ",
                      "\u2192 set to NA (impossible for liquid water)"),
              tags$li(tags$strong("edit_temp_neg_near"), " \u2014 temp -1 to 0\u00b0C ",
                      "\u2192 corrected to 0\u00b0C"),
              tags$li(tags$strong("flag_ice"), " \u2014 temp < 0.3\u00b0C ",
                      "\u2192 flagged (possible ice), data NOT changed"),
              tags$li(tags$strong("flag_wt_dry"), " \u2014 possible dry sensor: ",
                      "temp > 19\u00b0C, or air-water diff < 2\u00b0C, ",
                      "or air warmer than water")
            )
          ),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("Functions Used", style = "margin-top: 0;"),
            tags$code("qc_watertemp()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Automated ice/dry/negative-temp QAQC"),
            tags$code("adjust_logger_NA()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Manual removal of bad data (ice, dry, disturbance)"),
            tags$code("write_clean_data()"),
            tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                   "Export v1.0 CSV with metric = \"watertemp\"")
          ),
          tags$div(
            style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px;",
            tags$h4("Tip: Air Temp Overlay", style = "margin-top: 0;"),
            tags$p(style = "font-size: 13px; margin: 0;",
              "If you ran the Add Nearest Baro (Overlay) step, an ",
              tags$code("airtemp_C"), " column is available. ",
              tags$code("qc_watertemp()"), " will automatically use it ",
              "for improved dry-sensor detection (comparing air vs water temp)."
            )
          )
        ),

        column(
          width = 8,
          tags$h4("QAQC Parameters"),
          tags$div(
            style = "display: flex; gap: 15px; margin-bottom: 15px; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Select Station"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "[Station Dropdown]")
            ),
            tags$div(
              style = "flex: 1; min-width: 130px; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Ice Threshold (\u00b0C)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "0.3")
            ),
            tags$div(
              style = "flex: 1; min-width: 130px; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Dry Temp (\u00b0C)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "19")
            ),
            tags$div(
              style = "flex: 1; min-width: 130px; border: 1px solid #ddd; padding: 12px; border-radius: 5px; background: #f9f9f9;",
              tags$label("Air-Water Diff (\u00b0C)"),
              tags$div(style = "border: 1px solid #ccc; padding: 6px; background: white; border-radius: 4px; margin-top: 5px;",
                       "2")
            )
          ),

          tags$div(
            style = "display: inline-block; padding: 10px 25px; background: #28a745; color: white; border-radius: 5px; cursor: pointer; margin-bottom: 20px;",
            "Run Water Temp QAQC"
          ),

          # Flag summary
          tags$div(
            style = "border: 2px dashed #ccc; padding: 25px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("Flag Summary"), style = "color: #888;"),
            tags$p("Counts: flag_ice, flag_wt_dry, edit_temp_lt_minus1, edit_temp_neg_near",
                   style = "color: #aaa; font-size: 13px;")
          ),

          # QAQC plot
          tags$div(
            style = "border: 2px dashed #ccc; padding: 70px; text-align: center; border-radius: 5px; margin-bottom: 15px;",
            tags$p(tags$strong("Water Temperature QAQC Plot"), style = "color: #888;"),
            tags$p("Raw vs adjusted water temp with air temp overlay and flag markers. ",
                   "Ice flags (blue), dry flags (yellow), removed data (red).",
                   style = "color: #aaa; font-size: 13px;")
          ),

          # Flag legend
          tags$div(
            style = "display: flex; gap: 8px; margin-bottom: 15px; flex-wrap: wrap;",
            tags$div(style = "padding: 6px 12px; background: #28a745; color: white; border-radius: 4px; font-size: 12px;", "Good Data"),
            tags$div(style = "padding: 6px 12px; background: #17a2b8; color: white; border-radius: 4px; font-size: 12px;", "flag_ice"),
            tags$div(style = "padding: 6px 12px; background: #ffc107; color: black; border-radius: 4px; font-size: 12px;", "flag_wt_dry"),
            tags$div(style = "padding: 6px 12px; background: #dc3545; color: white; border-radius: 4px; font-size: 12px;", "Removed (< -1\u00b0C)")
          ),

          # Manual adjustment section
          tags$div(
            style = "border-left: 4px solid #e65100; padding: 15px 20px; margin-bottom: 20px; background: #f8f9fa; border-radius: 0 5px 5px 0;",
            tags$h4("Manual Adjustments", style = "margin-top: 0; color: #e65100;"),
            tags$p(style = "font-size: 13px;",
              "Use ", tags$code("adjust_logger_NA()"), " to remove data for ",
              "periods confirmed as ice or dry from visual inspection."
            ),
            tags$div(
              style = "display: flex; gap: 15px; flex-wrap: wrap; margin-top: 10px;",
              tags$div(
                style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: white;",
                tags$label("Start Timestamp"), "[datetime picker]"
              ),
              tags$div(
                style = "flex: 1; min-width: 150px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: white;",
                tags$label("End Timestamp"), "[datetime picker]"
              ),
              tags$div(
                style = "flex: 1; min-width: 120px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: white;",
                tags$label("Reason"), "[ice / dry / disturbance]"
              )
            ),
            tags$div(
              style = "display: inline-block; padding: 8px 20px; background: #e65100; color: white; border-radius: 4px; cursor: pointer; margin-top: 10px;",
              "Remove Period"
            )
          ),

          hr(),

          # ---- Export / Download section ----
          tags$h4("Export Clean Data"),
          tags$p(style = "font-size: 13px; color: #666;",
            "Download the QAQC'd water temperature data as a v1.0 CSV. ",
            "Columns exported: ", tags$code("site_station_code"), ", ",
            tags$code("timestamp"), ", ", tags$code("watertemp_C"), ", ",
            tags$code("watertemp_C_adj"), ", plus all ", tags$code("flag_*"),
            " and ", tags$code("edit_*"), " columns."
          ),
          tags$div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            tags$div(
              style = "display: inline-block; padding: 10px 25px; background: #007bff; color: white; border-radius: 5px; cursor: pointer;",
              "Download Clean CSV"
            ),
            tags$div(
              style = "display: inline-block; padding: 10px 25px; background: #17a2b8; color: white; border-radius: 5px; cursor: pointer;",
              "Download QAQC Log"
            )
          )
        )
      )
    ),
    .wireframe_footer()
  )
}

module_temp_qc_water_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}


# -----------------------------------------------------------------------------
# Shared footer helper (not exported)
# -----------------------------------------------------------------------------
.wireframe_footer <- function() {
  fluidRow(column(
    width = 12,
    tags$p(
      "Waterlogged Water Data Processing Application",
      style = "text-align: center; color: #8A939A; margin-top: 10px; font-size: 12px;"
    )
  ))
}
