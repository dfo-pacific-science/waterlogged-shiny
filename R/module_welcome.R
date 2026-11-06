#' Welcome Page Module - UI
module_welcome_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,

        tags$div(
          style = "margin: 0 auto; padding: 20px 15px; max-width: 1000px;",

          # Hero section
          tags$div(
            style = "text-align: center; margin-bottom: 30px;",
            tags$h1("Waterlogged", style = "font-size: 2.5em; color: #2c3e50; margin-bottom: 5px;"),
            tags$p("Water Level & Water Quality Data Processing",
                   style = "font-size: 1.2em; color: #7f8c8d; margin-top: 0;")
          ),

          # Welcome card
          tags$div(
            style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",

            tags$p(
              "Waterlogged is a data processing package for water data from continuous
              water data loggers. The goal of waterlogged is to streamline the process
              from raw data to clean data, document QAQC processing via R scripts, and
              do so without relying on proprietary software.",
              style = "font-size: 15px; line-height: 1.6; color: #555; margin-bottom: 15px;"
            ),

            tags$p(
              "This version of the package was built for data from Onset HOBO barometric
              pressure (U20), dissolved oxygen (U26), conductivity (U24), and TidbiT loggers.",
              style = "font-size: 15px; line-height: 1.6; color: #555; margin: 0;"
            )
          ),

          # Workflow Overview card - full width with no padding around flowchart
          tags$div(
            style = "background-color: #fff; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); overflow: hidden;",

            tags$div(
              style = "padding: 25px 25px 10px 25px;",
              tags$h2("Workflow Overview", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
              tags$p(
                "This application guides you through a step-by-step workflow for processing
                your water logger data:",
                style = "color: #666; margin-bottom: 15px;"
              )
            ),

            # Flowchart image - no horizontal padding, spans full card width
            tags$img(
              src = "img/flowchart.png",
              style = "width: 100%; display: block; height: auto;"
            ),

            # Repository links - inside padded area below
            tags$div(
              style = "padding: 20px 25px 25px 25px;",
              tags$div(
                style = "display: flex; gap: 20px; flex-wrap: wrap;",
                tags$div(
                  style = "flex: 1; min-width: 250px; background: #f8f9fa; padding: 12px 16px; border-radius: 6px; border-left: 4px solid #3498db;",
                  icon("box-open", style = "color: #3498db; margin-right: 6px;"),
                  tags$span(style = "color: #555;", "Follow along in the R package: "),
                  tags$a(
                    href = "https://github.com/dfo-pacific-science/waterlogged",
                    target = "_blank",
                    style = "color: #3498db; font-weight: bold;",
                    "github.com/dfo-pacific-science/waterlogged"
                  )
                ),
                tags$div(
                  style = "flex: 1; min-width: 250px; background: #f8f9fa; padding: 12px 16px; border-radius: 6px; border-left: 4px solid #27ae60;",
                  icon("desktop", style = "color: #27ae60; margin-right: 6px;"),
                  tags$span(style = "color: #555;", "Shiny app repository: "),
                  tags$a(
                    href = "https://github.com/dfo-pacific-science/waterlogged-shiny",
                    target = "_blank",
                    style = "color: #27ae60; font-weight: bold;",
                    "github.com/dfo-pacific-science/waterlogged-shiny"
                  )
                )
              ),
              # ---- Documentation downloads row ----
              tags$div(
                style = "display: flex; flex-wrap: wrap; gap: 10px; margin-top: 10px;",
                tags$div(
                  style = "flex: 1; min-width: 250px; background: #f8f9fa; padding: 12px 16px; border-radius: 6px; border-left: 4px solid #8e44ad;",
                  icon("file-pdf", style = "color: #8e44ad; margin-right: 6px;"),
                  tags$span(style = "color: #555; font-weight: bold;", "Documentation & Tutorials"),
                  tags$ul(
                    style = "list-style: none; padding-left: 0; margin: 8px 0 0 0;",
                    tags$li(style = "margin-bottom: 4px;",
                      tags$a(href = "waterlogged_manual.pdf", target = "_blank",
                        style = "color: #8e44ad;",
                        icon("download", style = "margin-right: 4px;"),
                        "waterlogged R package manual"
                      )
                    ),
                    tags$li(style = "margin-bottom: 4px;",
                      tags$a(href = "barometric-pressure.pdf", target = "_blank",
                        style = "color: #8e44ad;",
                        icon("download", style = "margin-right: 4px;"),
                        "Tutorial: Barometric timeseries"
                      )
                    ),
                    tags$li(style = "margin-bottom: 4px;",
                      tags$a(href = "water-level.pdf", target = "_blank",
                        style = "color: #8e44ad;",
                        icon("download", style = "margin-right: 4px;"),
                        "Tutorial: Water level timeseries"
                      )
                    ),
                    tags$li(style = "margin-bottom: 4px;",
                      tags$a(href = "dissolved-oxygen.pdf", target = "_blank",
                        style = "color: #8e44ad;",
                        icon("download", style = "margin-right: 4px;"),
                        "Tutorial: Dissolved oxygen timeseries"
                      )
                    ),
                    tags$li(
                      tags$a(href = "conductivity.pdf", target = "_blank",
                        style = "color: #8e44ad;",
                        icon("download", style = "margin-right: 4px;"),
                        "Tutorial: Conductivity timeseries"
                      )
                    )
                  )
                )
              )
            )
          ),

          # Two-column layout for Logger Types and Getting Started
          fluidRow(
            column(
              width = 6,
              tags$div(
                style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); height: 100%;",

                tags$h2("Supported Loggers", style = "margin-top: 0; color: #2c3e50; font-size: 1.3em; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),

                tags$div(
                  style = "font-size: 14px;",
                  tags$div(style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                    tags$strong("HOBO U20", style = "color: #3498db;"),
                    tags$span(" - Water Level", style = "color: #666;")
                  ),
                  tags$div(style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                    tags$strong("HOBO U20", style = "color: #3498db;"),
                    tags$span(" - Barometric Pressure", style = "color: #666;")
                  ),
                  tags$div(style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                    tags$strong("HOBO U26", style = "color: #9b59b6;"),
                    tags$span(" - Dissolved Oxygen", style = "color: #666;")
                  ),
                  tags$div(style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                    tags$strong("HOBO U24", style = "color: #e67e22;"),
                    tags$span(" - Conductivity", style = "color: #666;")
                  ),
                  tags$div(style = "padding: 8px 0;",
                    tags$strong("HOBO TidbiT", style = "color: #27ae60;"),
                    tags$span(" - Temperature", style = "color: #666;")
                  )
                )
              )
            ),
            column(
              width = 6,
              tags$div(
                style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); height: 100%;",

                tags$h2("Getting Started", style = "margin-top: 0; color: #2c3e50; font-size: 1.3em; border-bottom: 2px solid #27ae60; padding-bottom: 10px;"),

                tags$ol(
                  style = "padding-left: 20px; margin: 0; font-size: 14px; color: #555; line-height: 1.8;",
                  tags$li("Navigate to ", tags$strong("Upload Data")),
                  tags$li("Follow each step in sequence"),
                  tags$li("Review instructions and examples"),
                  tags$li("Process and visualize your data")
                )
              )
            )
          ),

          # Key Functions card
          tags$div(
            style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",

            tags$h2("Key Functions", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),

            tags$div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 15px; margin-top: 15px;",

              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #888; font-size: 12px;", "Data Binding"),
                tags$br(),
                tags$code(style = "color: #e74c3c;", "bind_logger_files()")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #888; font-size: 12px;", "Barometric Compensation"),
                tags$br(),
                tags$code(style = "color: #e74c3c;", "add_nearest_baro()")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #888; font-size: 12px;", "Water Level Conversion"),
                tags$br(),
                tags$code(style = "color: #e74c3c;", "convert_waterlevel_kPa_m()")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #888; font-size: 12px;", "DO Conversion"),
                tags$br(),
                tags$code(style = "color: #e74c3c;", "convert_do_mgl_percsat()")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #888; font-size: 12px;", "QAQC"),
                tags$br(),
                tags$code(style = "color: #e74c3c;", "qc_waterlevel(), qc_dissox()")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #888; font-size: 12px;", "Data Export"),
                tags$br(),
                tags$code(style = "color: #e74c3c;", "write_clean_data()")
              )
            )
          ),

          # Authors card
          tags$div(
            style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",

            tags$h2("Authors & Contributors", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),

            tags$p(
              "Department of Fisheries and Oceans, Canada",
              style = "font-weight: 600; color: #2c3e50; margin-bottom: 20px;"
            ),

            fluidRow(
              column(
                width = 4,
                tags$h4("Lead Authors/Developers", style = "color: #3498db; font-size: 14px; margin-bottom: 10px;"),
                tags$div(style = "font-size: 13px; line-height: 1.8;",
                  tags$div("Daniella Lo"),
                  tags$div("Amanda Martens"),
                  tags$div("Julian Gan")
                )
              ),
              column(
                width = 4,
                tags$h4("R Package Contributions", style = "color: #3498db; font-size: 14px; margin-bottom: 10px;"),
                tags$div(style = "font-size: 13px; line-height: 1.8;",
                  tags$div("Paula Soto")
                )
              ),
              column(
                width = 4,
                tags$h4("Technical Support", style = "color: #3498db; font-size: 14px; margin-bottom: 10px;"),
                tags$div(style = "font-size: 13px; line-height: 1.8;",
                  tags$div("Matthew Bayly: MJBA (R Package & App Development)"),
                  tags$div("Frank Poulsen: Ashlu (Techical Support)")
                )
              )
            ),

            tags$div(
              style = "text-align: center; margin-top: 25px; padding-top: 20px; border-top: 1px solid #eee;",
              tags$img(
                src = "./img/dfo_logo.png",
                style = "max-width: 200px; opacity: 0.8;"
              )
            )
          ),

          # Footer
          tags$p(
            "Waterlogged Water Data Processing Application",
            style = "text-align: center; color: #8A939A; margin-top: 20px; font-size: 12px;"
          )
        )
      )
    )
  )
}

#' Welcome Page Module - Server
module_welcome_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Welcome page is static, no server logic needed for wireframe
  })
}
