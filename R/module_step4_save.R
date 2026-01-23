#' Step 4: Save Clean Data Module - UI
module_step4_save_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Step 4: Save Clean Site Data", class = "custom-h1"),

      tags$p(
        "After completing QAQC, save your clean data files for further analysis. The
        write_clean_data() function drops unnecessary columns and saves the final
        dataset in a standardized format.",
        class = "clean-print"
      ),

      tags$h2("The write_clean_data() Function", class = "custom-h2"),

      tags$p(
        "This function streamlines your QAQC'd data by keeping only the essential columns
        and saving it as a clean CSV file ready for analysis.",
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
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Your QAQC'd dataframe for a single station")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("path_to_output_folder")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Where to save the clean data file")
            ),
            tags$tr(
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", tags$code("logger_type")),
              tags$td(style = "border: 1px solid #ddd; padding: 10px;", "Type: waterlevel_U20, baro_U20, dissolvedoxygen_U26, watertemp_TidbiT, airtemp_TidbiT")
            )
          )
        )
      ),

      tags$h3("Example: Save Water Level Data", class = "custom-h2"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Save clean water level data for a station
write_clean_data(
  input_data = station_wl_dat_final,
  path_to_output_folder = \"data/waterlevel/2024/clean\",
  logger_type = \"waterlevel_U20\"
)

# Output file will be named:
# [site_station_code]_waterlevel_U20_[year]_clean.csv"
      ),

      tags$h3("Example: Save Dissolved Oxygen Data", class = "custom-h2"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace; font-size: 12px;",
"# Save clean dissolved oxygen data for a station
write_clean_data(
  input_data = station_do_dat_final,
  path_to_output_folder = \"data/dissolved_oxygen/2024/clean\",
  logger_type = \"dissolvedoxygen_U26\"
)

# Output file will be named:
# [site_station_code]_dissolvedoxygen_U26_[year]_clean.csv"
      ),

      tags$h3("Output File Structure", class = "custom-h2"),
      tags$p(
        "Clean data files will contain the following columns (depending on logger type):",
        class = "clean-print"
      ),

      tags$div(
        class = "clean-print",
        style = "display: flex; gap: 20px; flex-wrap: wrap; margin: 20px 0;",
        tags$div(
          style = "flex: 1; min-width: 250px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
          tags$h4("Water Level (waterlevel_U20)", style = "margin-top: 0; color: #00AFDB;"),
          tags$ul(style = "padding-left: 20px; margin: 0;",
            tags$li("timestamp"),
            tags$li("site_station_code"),
            tags$li("waterlevel_m_U20"),
            tags$li("watertemp_C_U20"),
            tags$li("wl_qaqc_code"),
            tags$li("wl_qaqc_note")
          )
        ),
        tags$div(
          style = "flex: 1; min-width: 250px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
          tags$h4("Dissolved Oxygen (dissolvedoxygen_U26)", style = "margin-top: 0; color: #00AFDB;"),
          tags$ul(style = "padding-left: 20px; margin: 0;",
            tags$li("timestamp"),
            tags$li("site_station_code"),
            tags$li("do_mgl_U26"),
            tags$li("do_percsat_U26"),
            tags$li("watertemp_C_U26"),
            tags$li("do_qaqc_code"),
            tags$li("do_qaqc_note")
          )
        )
      ),

      tags$h3("Recommended Folder Structure", class = "custom-h2"),
      tags$pre(
        class = "clean-print",
        style = "background-color: #f5f5f5; padding: 15px; font-family: monospace;",
"data/
  waterlevel/
    2024/
      raw/           <- Original HOBOware exports
      processed/     <- Output from bind_hobo_files() (v0.1)
      clean/         <- Output from write_clean_data()
        SLUG_ST_01_waterlevel_U20_2024_clean.csv
        SLUG_ST_02_waterlevel_U20_2024_clean.csv
  dissolved_oxygen/
    2024/
      raw/
      processed/
      clean/
        SLUG_ST_01_dissolvedoxygen_U26_2024_clean.csv"
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
          "SLUG_ST_01 - Water Level (8,500 records)"
        ),
        tags$div(style = "margin: 8px 0;",
          tags$input(type = "checkbox", checked = "checked", style = "margin-right: 10px;"),
          "SLUG_ST_02 - Water Level (8,700 records)"
        ),
        tags$div(style = "margin: 8px 0;",
          tags$input(type = "checkbox", style = "margin-right: 10px;"),
          "SLUG_ST_01 - Dissolved Oxygen (8,200 records)"
        ),
        tags$div(style = "margin: 8px 0;",
          tags$input(type = "checkbox", checked = "checked", style = "margin-right: 10px;"),
          "Include QAQC metadata columns"
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
          "Generate QAQC Report"
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
