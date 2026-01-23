#' Step 3: QAQC Module - UI
module_step3_qaqc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 3: QAQC", class = "custom-h1"),
      tags$h2("Quality Assurance and Quality Control", class = "custom-h2"),

      tags$p(
        "QAQC is a critical step in water data processing. The waterlogged package provides
        automated QAQC functions that flag common issues, plus manual adjustment functions
        for correcting specific problems identified during visual review.",
        class = "clean-print"
      ),

      tags$p(
        "All data processing is saved in R scripts for tracking QAQC of each logger type
        and year. Adjustments are recorded as additional QAQC columns in the clean data
        files, creating a transparent audit trail.",
        class = "clean-print"
      ),

      tags$h2("Part A: Water Level QAQC", class = "custom-h2"),
      tags$h3("waterlevel_qaqc() - Automatic flagging", class = "custom-h2"),

      tags$p(
        "This function performs preliminary QAQC on water level data, automatically
        identifying and flagging common issues:",
        class = "clean-print"
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$h4("Automatic QAQC Rules", style = "margin-top: 0;"),
        tags$ul(
          tags$li("Negative water level values are changed to zero"),
          tags$li("Water temperatures above 19C flagged as ", tags$code("FLAG_DRY"), " (potentially exposed logger)"),
          tags$li("Water temperatures below 0.3C flagged as ", tags$code("FLAG_ICE"), " (potentially frozen)"),
          tags$li("Water temperatures below -1C are removed (sensor error)"),
          tags$li("Consecutive dry/ice flags for 12+ hours create extended flags")
        )
      ),

      tags$h4("Example R Code"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Run automatic QAQC on water level data
station_wl_dat2 <- waterlevel_qaqc(
  input_data = station_wl_dat,
  select_station = \"SLUG_ST_01\",
  plot_only = FALSE  # Set TRUE to just view plot without making changes
)

# Returns a list with:
# [[1]] Dataframe with QAQC flags added
# [[2]] Interactive plot for visual assessment

# Access the data with QAQC flags
station_wl_qaqc <- station_wl_dat2[[1]]

# View the interactive QAQC plot
station_wl_dat2[[2]]"
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$p(tags$strong("QAQC columns added:"), style = "margin: 0 0 10px 0;"),
        tags$ul(style = "margin: 0;",
          tags$li(tags$code("waterlevel_m_U20_adj"), " - Adjusted water level value"),
          tags$li(tags$code("watertemp_C_U20_adj"), " - Adjusted water temperature"),
          tags$li(tags$code("wl_qaqc_adj"), " - Type of adjustment made"),
          tags$li(tags$code("wl_qaqc_code"), " - QAQC status code"),
          tags$li(tags$code("wl_qaqc_note"), " - Detailed notes on QAQC decisions")
        )
      ),

      tags$h3("Manual Adjustment Functions", class = "custom-h2"),

      tags$p(
        "After reviewing the QAQC plot, you may identify specific issues that need manual
        correction. The waterlogged package provides several adjustment functions:",
        class = "clean-print"
      ),

      tags$h4("adjust_waterlevel_spike() - Remove spikes from ice or disturbance"),
      tags$p(
        "Use this to flatten water level during periods affected by ice formation or
        physical disturbance (e.g., beaver activity, debris).",
        class = "clean-print"
      ),

      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Remove an ice-related spike
station_wl_dat3 <- adjust_waterlevel_spike(
  input_data = station_wl_dat2,
  select_station = \"SLUG_ST_01\",
  timestamp_start = \"2024-01-15 00:00:00\",
  timestamp_end = \"2024-01-20 12:00:00\",
  reason_to_adjust = \"ice\"  # or \"disturbance\"
)

# View result with plot_only = TRUE
waterlevel_qaqc(input_data = station_wl_dat3,
                select_station = \"SLUG_ST_01\",
                plot_only = TRUE)"
      ),

      tags$h4("adjust_waterlevel_zero() - Shift water level reference"),
      tags$p(
        "Use this when the stage is observed to be dry but logger shows non-zero values.
        Shifts all values up or down to correct the reference level.",
        class = "clean-print"
      ),

      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Shift water level down when observed dry period shows positive values
station_wl_dat4 <- adjust_waterlevel_zero(
  input_data = station_wl_dat3,
  select_station = \"SLUG_ST_01\",
  timestamp_start = \"2024-08-01 12:00:00\",
  timestamp_end = \"2024-08-01 14:00:00\",
  shift_up_or_down = \"down\"
)"
      ),

      tags$h4("adjust_waterlevel_cablebreak() - Handle logger movement"),
      tags$p(
        "Use this when a logger has shifted position (e.g., cable break, sediment burial)
        causing a sudden offset in readings.",
        class = "clean-print"
      ),

      tags$hr(),

      tags$h2("Part B: Dissolved Oxygen QAQC", class = "custom-h2"),
      tags$h3("dissox_qaqc() - Automatic DO flagging", class = "custom-h2"),

      tags$p(
        "Similar to water level QAQC, this function performs preliminary quality control
        on dissolved oxygen data:",
        class = "clean-print"
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$h4("Automatic QAQC Rules for DO", style = "margin-top: 0;"),
        tags$ul(
          tags$li("DO values < 0 or > 200% saturation flagged as ", tags$code("DO_ERROR")),
          tags$li("Temperature < -1C or > 30C flagged as ", tags$code("TEMP_ERROR")),
          tags$li("DO > 21 mg/L removed (sensor malfunction)"),
          tags$li("Negative DO values removed and coded as ", tags$code("NEGATIVE_DO")),
          tags$li("If water temp > 19C for 12+ hours, flagged as ", tags$code("FLAG_DRY")),
          tags$li("If water temp < 0.3C, flagged as ", tags$code("FLAG_ICE"))
        )
      ),

      tags$h4("Example R Code"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Run automatic QAQC on dissolved oxygen data
station_do_dat2 <- dissox_qaqc(
  input_data = station_do_dat,
  select_station = \"SLUG_ST_01\",
  plot_only = FALSE,
  var_do_mgl = \"do_mgl_U26\",
  var_do_percsat = \"do_percsat_U26\",
  var_watertemp_C_1 = \"watertemp_C_U26\"
)

# If you also have water level data for this station,
# include it for better dry/ice detection:
station_do_dat2 <- dissox_qaqc(
  input_data = station_do_dat,
  select_station = \"SLUG_ST_01\",
  var_waterlevel_m = \"waterlevel_m_U20\",
  var_watertemp_C_2 = \"watertemp_C_U20\"
)"
      ),

      tags$h3("Interactive QAQC Tool", class = "custom-h2"),
      tags$p(
        "[Wireframe: Interactive QAQC interface will be implemented here]",
        class = "clean-print",
        style = "color: #999; font-style: italic;"
      ),

      tags$div(
        style = "display: flex; gap: 20px; margin: 20px 0;",
        tags$div(
          style = "flex: 1;",
          tags$label("Select Station"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
                   "[Dropdown selector]")
        ),
        tags$div(
          style = "flex: 1;",
          tags$label("Data Type"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
                   "[Water Level / Dissolved Oxygen]")
        )
      ),

      tags$h4("QAQC Flag Legend"),
      tags$div(
        style = "display: flex; gap: 10px; margin: 20px 0; flex-wrap: wrap;",
        tags$div(style = "padding: 8px 15px; background: #28a745; color: white; border-radius: 5px;", "Good Data"),
        tags$div(style = "padding: 8px 15px; background: #17a2b8; color: white; border-radius: 5px;", "FLAG_ICE"),
        tags$div(style = "padding: 8px 15px; background: #ffc107; color: black; border-radius: 5px;", "FLAG_DRY"),
        tags$div(style = "padding: 8px 15px; background: #343a40; color: white; border-radius: 5px;", "FLAG_DISTURBANCE"),
        tags$div(style = "padding: 8px 15px; background: #dc3545; color: white; border-radius: 5px;", "ERROR (removed)")
      ),

      tags$div(
        style = "border: 2px dashed #ccc; padding: 80px; text-align: center; margin: 20px 0;",
        tags$p("Interactive QAQC Time Series Plot", style = "font-weight: bold;"),
        tags$p("Click and drag to select time ranges for adjustment", style = "color: #999;"),
        tags$p("QAQC flags shown as colored symbols/bands", style = "color: #999;")
      ),

      tags$h3("QAQC Summary Table", class = "custom-h2"),
      tags$div(
        style = "border: 1px solid #ddd; padding: 20px; background-color: #f9f9f9; margin: 10px 0;",
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 12px;",
          tags$thead(
            tags$tr(
              tags$th("Station", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("Total Records", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("Good", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("FLAG_ICE", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("FLAG_DRY", style = "border: 1px solid #ddd; padding: 8px; background: #eee;"),
              tags$th("Removed", style = "border: 1px solid #ddd; padding: 8px; background: #eee;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("SLUG_ST_01", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("8,760", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("7,800 (89%)", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("480 (5%)", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("420 (5%)", style = "border: 1px solid #ddd; padding: 8px;"),
              tags$td("60 (1%)", style = "border: 1px solid #ddd; padding: 8px;")
            )
          )
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 20px;",
        tags$p(tags$strong("Next Step:"), " Once you are satisfied with your QAQC adjustments,
        proceed to Step 4 to save your clean data files.",
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
