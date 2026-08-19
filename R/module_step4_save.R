#' Step 4: Save Clean Data Module - UI
module_step4_save_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 4: Save Clean Site Data", class = "custom-h1"),
      tags$h2("Export processed data with QC documentation", class = "custom-h2"),

      # Introduction
      tags$div(
        class = "clean-print",
        style = "margin-bottom: 20px;",
        tags$p(
          "After completing QC, you need to save your clean data files for further analysis
          or sharing. The write_clean_data() function prepares your data by keeping only the
          essential columns while preserving the QC documentation that explains what
          adjustments were made and why."
        ),
        tags$p(
          "Clean data files are saved with the suffix 'v1.0' to indicate they have been fully
          processed and are ready for analysis. This distinguishes them from intermediate
          processing stages (v0.1 = raw bound, v0.2 = baro added, v0.3 = converted)."
        )
      ),

      fluidRow(
        # Left column - explanation
        column(
          width = 4,
          tags$div(
            style = "background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("What gets saved?", style = "margin-top: 0;"),
            tags$p(
              "Clean data files contain the essential processed values plus QC metadata.
              Intermediate columns used during processing (raw values, temporary calculations)
              are dropped to keep the files clean and focused."
            ),
            tags$p(
              "The QC columns are important - they document exactly what adjustments were
              made to each data point, creating a transparent audit trail."
            )
          ),

          tags$div(
            style = "background-color: #d4edda; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
            tags$h4("File naming convention", style = "margin-top: 0;"),
            tags$p(
              "Files are named with a standard pattern:"
            ),
            tags$code(
              style = "display: block; background: #f8f9fa; padding: 10px; border-radius: 3px; font-size: 12px;",
              "[SITE]_[TYPE]_[START]_[END]_v1.0.rds"
            ),
            tags$p(
              style = "margin-top: 10px; font-size: 13px;",
              "Example: ALBR_ST_30_WL_20241010_20251022_v1.0.rds"
            )
          ),

          tags$div(
            style = "background-color: #fff3cd; padding: 20px; border-radius: 5px;",
            tags$h4("Documentation tip", style = "margin-top: 0;"),
            tags$p(
              "We recommend saving your R script with all QC function calls alongside
              your clean data. This creates a reproducible record of all processing steps
              and allows you or others to re-run the analysis if needed."
            )
          )
        ),

        # Right column - function details
        column(
          width = 8,
          tags$h4("The write_clean_data() Function"),
          tags$p(
            "This function streamlines your QC'd data by keeping only the essential columns
            and saving it in a standardized format.",
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
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("input_data")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Your QC'd dataframe for a single station")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("logger_type")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Type: waterlevel, baro, dissolvedoxygen, conductivity, watertemp_TidbiT, airtemp_TidbiT")
            )
          )
        )
      ),

          tags$h4("Example R Code"),
          tags$pre(
            class = "clean-print",
            style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Save clean water level data
write_clean_data(
  input_data = station_wl_dat_final,
  logger_type = \"waterlevel\"
)

# Save clean dissolved oxygen data
write_clean_data(
  input_data = station_do_dat_final,
  logger_type = \"dissolvedoxygen\"
)

# Save clean conductivity data
write_clean_data(
  input_data = cond_dat_final,
  logger_type = \"conductivity\"
)"
          )
        )
      ),

      hr(),

      tags$h3("Output File Structure", class = "custom-h2"),
      tags$p(
        "Clean data files contain the processed values plus QC documentation columns.
        The exact columns depend on the logger type:",
        class = "clean-print"
      ),

      tags$div(
        class = "clean-print",
        style = "display: flex; gap: 20px; flex-wrap: wrap; margin: 20px 0;",
        tags$div(
          style = "flex: 1; min-width: 250px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
          tags$h4("Water Level (waterlevel)", style = "margin-top: 0; color: #00AFDB;"),
          tags$ul(style = "padding-left: 20px; margin: 0;",
            tags$li("timestamp"),
            tags$li("site_station_code"),
            tags$li("waterlevel_m"),
            tags$li("watertemp_C"),
            tags$li("wl_qaqc_code"),
            tags$li("wl_qaqc_note")
          )
        ),
        tags$div(
          style = "flex: 1; min-width: 250px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
          tags$h4("Dissolved Oxygen (dissolvedoxygen)", style = "margin-top: 0; color: #00AFDB;"),
          tags$ul(style = "padding-left: 20px; margin: 0;",
            tags$li("timestamp"),
            tags$li("site_station_code"),
            tags$li("do_mgl"),
            tags$li("do_percsat"),
            tags$li("watertemp_C"),
            tags$li("do_qaqc_code"),
            tags$li("do_qaqc_note")
          )
        )
      ),

      tags$h3("Export Options", class = "custom-h2"),
      tags$p(
        "[Wireframe: Export interface will be implemented here]",
        class = "clean-print",
        style = "color: #999; font-style: italic;"
      ),

      tags$div(
        style = "display: flex; gap: 20px; margin: 20px 0;",
        tags$div(
          style = "border: 1px solid #007bff; padding: 20px; border-radius: 5px; flex: 1; text-align: center; cursor: pointer;",
          tags$strong("CSV", style = "color: #007bff;"),
          tags$p("Standard format", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
        ),
        tags$div(
          style = "border: 1px solid #ccc; padding: 20px; border-radius: 5px; flex: 1; text-align: center; cursor: pointer;",
          tags$strong("RDS"),
          tags$p("R native format", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
        ),
        tags$div(
          style = "border: 1px solid #ccc; padding: 20px; border-radius: 5px; flex: 1; text-align: center; cursor: pointer;",
          tags$strong("Excel"),
          tags$p("Multi-sheet workbook", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
        )
      ),

      tags$h3("Data Selection", class = "custom-h2"),
      tags$div(
        class = "clean-print",
        style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px; margin: 15px 0;",
        tags$div(style = "margin: 8px 0;",
          tags$input(type = "checkbox", checked = "checked", style = "margin-right: 10px;"),
          "ALBR_ST_30 - Water Level (8,500 records)"
        ),
        tags$div(style = "margin: 8px 0;",
          tags$input(type = "checkbox", checked = "checked", style = "margin-right: 10px;"),
          "COOK_WE_40 - Water Level (8,700 records)"
        ),
        tags$div(style = "margin: 8px 0;",
          tags$input(type = "checkbox", style = "margin-right: 10px;"),
          "ALBR_ST_30 - Dissolved Oxygen (8,200 records)"
        ),
        tags$div(style = "margin: 8px 0;",
          tags$input(type = "checkbox", checked = "checked", style = "margin-right: 10px;"),
          "Include QC metadata columns"
        )
      ),

      tags$div(
        style = "margin-top: 30px; text-align: center;",
        tags$div(
          style = "display: inline-block; padding: 15px 40px; background: #007bff; color: white; border-radius: 5px; cursor: pointer; margin-right: 10px;",
          "Download Selected Data"
        ),
        tags$div(
          style = "display: inline-block; padding: 15px 40px; background: #28a745; color: white; border-radius: 5px; cursor: pointer;",
          "Generate QC Report"
        )
      ),

      tags$div(
        class = "clean-print",
        style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 30px;",
        tags$p(tags$strong("Next Step:"), " Once you have saved your clean data files,
        proceed to Step 5 to generate summary statistics and visualizations.",
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

#' Step 4: Save Clean Data Module - Server
module_step4_save_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic will be implemented in future iterations
  })
}
