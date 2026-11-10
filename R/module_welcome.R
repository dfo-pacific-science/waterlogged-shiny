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
            tags$h1("waterlogged", style = "font-size: 2.5em; color: #233d4d; margin-bottom: 5px;"),
            tags$p("Standardized and efficient processing of environmental data from continuous data loggers",
                   style = "font-size: 1.2em; color: #555; margin-top: 0;")
          ),

          # Welcome card
          tags$div(
            style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",

            tags$p(
              "waterlogged is a data processing application for water data from
              continuous water data loggers. The goal of waterlogged is to make
              aquatic environmental data processing more streamlined,
              standardized and accessible to a wide range of users.",
              style = "font-size: 15px; line-height: 1.6; color: #555; margin-bottom: 15px;"
            ),

            tags$p(
              "This version of the application was built for data from Onset
              HOBO barometric pressure (U20), dissolved oxygen (U26),
              conductivity (U24), and TidbiT loggers.",
              style = "font-size: 15px; line-height: 1.6; color: #555; margin: 0;"
            )
          ),

          # Workflow Overview card - full width with no padding around flowchart
          tags$div(
            style = "background-color: #fff; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); overflow: hidden;",

            tags$div(
              style = "padding: 25px 25px 10px 25px;",
              tags$h2("Workflow Overview", style = "margin-top: 0; color: #233d4d; border-bottom: 2px solid #233d4d; padding-bottom: 10px;"),
              tags$p(
                "This application guides you through a step-by-step workflow for processing
                your logger data:",
                style = "color: #555; margin-bottom: 15px;"
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
                  style = "flex: 1; min-width: 250px; background: #f8f9fa; padding: 12px 16px; border-radius: 6px; border-left: 4px solid #619b8a;",
                  icon("box-open", style = "color: #619b8a; margin-right: 6px;"),
                  tags$span(style = "color: #555;", "Follow along in the R package: "),
                  tags$a(
                    href = "https://github.com/dfo-pacific-science/waterlogged",
                    target = "_blank",
                    style = "color: #619b8a; font-weight: bold;",
                    "github.com/dfo-pacific-science/waterlogged"
                  )
                ),
                tags$div(
                  style = "flex: 1; min-width: 250px; background: #f8f9fa; padding: 12px 16px; border-radius: 6px; border-left: 4px solid #a1c181;",
                  icon("desktop", style = "color: #a1c181; margin-right: 6px;"),
                  tags$span(style = "color: #555;", "Shiny app repository: "),
                  tags$a(
                    href = "https://github.com/dfo-pacific-science/waterlogged-shiny",
                    target = "_blank",
                    style = "color: #a1c181; font-weight: bold;",
                    "github.com/dfo-pacific-science/waterlogged-shiny"
                  )
                )
              ),
              # ---- Documentation downloads row ----
              tags$div(
                style = "display: flex; flex-wrap: wrap; gap: 10px; margin-top: 10px;",
                tags$div(
                  style = "flex: 1; min-width: 250px; background: #f8f9fa; padding: 12px 16px; border-radius: 6px; border-left: 4px solid #233d4d;",
                  icon("file-pdf", style = "color: #233d4d; margin-right: 6px;"),
                  tags$span(style = "color: #555; font-weight: bold;", "Documentation & Tutorials"),
                  tags$ul(
                    style = "list-style: none; padding-left: 0; margin: 8px 0 0 0;",
                    tags$li(style = "margin-bottom: 4px;",
                      tags$a(href = "waterlogged_manual.pdf", target = "_blank",
                        style = "color: #233d4d;",
                        icon("download", style = "margin-right: 4px;"),
                        "waterlogged R package manual"
                      )
                    ),
                    tags$li(style = "margin-bottom: 4px;",
                      tags$a(href = "barometric-pressure.pdf", target = "_blank",
                        style = "color: #233d4d;",
                        icon("download", style = "margin-right: 4px;"),
                        "Tutorial: Barometric timeseries"
                      )
                    ),
                    tags$li(style = "margin-bottom: 4px;",
                      tags$a(href = "water-level.pdf", target = "_blank",
                        style = "color: #233d4d;",
                        icon("download", style = "margin-right: 4px;"),
                        "Tutorial: Water level timeseries"
                      )
                    ),
                    tags$li(style = "margin-bottom: 4px;",
                      tags$a(href = "dissolved-oxygen.pdf", target = "_blank",
                        style = "color: #233d4d;",
                        icon("download", style = "margin-right: 4px;"),
                        "Tutorial: Dissolved oxygen timeseries"
                      )
                    ),
                    tags$li(
                      tags$a(href = "conductivity.pdf", target = "_blank",
                        style = "color: #233d4d;",
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

                tags$h2("Supported Loggers", style = "margin-top: 0; color: #233d4d; font-size: 1.3em; border-bottom: 2px solid #233d4d; padding-bottom: 10px;"),

                tags$div(
                  style = "font-size: 14px;",
                  tags$div(style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                    tags$strong("HOBO U20", style = "color: #FDB840;"),
                    tags$span(" - Water Level", style = "color: #555;")
                  ),
                  tags$div(style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                    tags$strong("HOBO U20", style = "color: #fe7f2d;"),
                    tags$span(" - Barometric Pressure", style = "color: #555;")
                  ),
                  tags$div(style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                    tags$strong("HOBO U26", style = "color: #a1c181;"),
                    tags$span(" - Dissolved Oxygen", style = "color: #555;")
                  ),
                  tags$div(style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                    tags$strong("HOBO U24", style = "color: #619b8a;"),
                    tags$span(" - Conductivity", style = "color: #555;")
                  ),
                  tags$div(style = "padding: 8px 0;",
                    tags$strong("HOBO TidbiT", style = "color: #915e3d;"),
                    tags$span(" - Temperature", style = "color: #555;")
                  )
                )
              )
            ),
            column(
              width = 6,
              tags$div(
                style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); height: 100%;",

                tags$h2("Getting Started", style = "margin-top: 0; color: #233d4d; font-size: 1.3em; border-bottom: 2px solid #233d4d; padding-bottom: 10px;"),

                tags$ol(
                  style = "padding-left: 20px; margin: 0; font-size: 14px; color: #555; line-height: 1.8;",
                  tags$li("In the left column, navigate to the primary metric of
                          the logger data to be processed, ", tags$strong("Import Data")),
                  tags$li("Upload metadata and review"),
                  tags$li("Upload raw logger files and ", tags$strong("Bind Files")),
                  tags$li("Review raw data series"),
                  tags$li("Proceed to run Automated Quality Control"),
                  tags$li("Manually review data flags"),
                  tags$li("Export clean logger data and QC logs")

                )
              )
            )
          ),

          # Key Functions card
          tags$div(
            style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",

            tags$h2("Key Functions", style = "margin-top: 0; color: #233d4d; border-bottom: 2px solid #233d4d; padding-bottom: 10px;"),

            tags$div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 15px; margin-top: 15px;",

              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #555; font-size: 12px;", "Bind and trim raw logger files by station metadata"),
                tags$br(),
                tags$code(style = "color: #fe7f2d;", "bind_logger_files()")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #555; font-size: 12px;", "Add nearby barometric data"),
                tags$br(),
                tags$code(style = "color: #fe7f2d;", "add_nearest_baro()")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #555; font-size: 12px;", "Unit conversions"),
                tags$br(),
                tags$code(style = "color: #fe7f2d;", "convert_waterlevel_kPa_m(), convert_do_mgl_percsat(), convert_conductivity_spc_uScm()")
              ),

              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #555; font-size: 12px;", "Automated QC to produce data flags"),
                tags$br(),
                tags$code(style = "color:#fe7f2d;", "qc_watertemp(), qc_airtemp(), qc_barometric(), qc_waterlevel(), qc_dissox(), qc_conductivity()")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                       tags$span(style = "color: #555; font-size: 12px;", "Visualize logger data and flags"),
                       tags$br(),
                       tags$code(style = "color:#fe7f2d;", "plot_qc_timeseries")
              ),
              tags$div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
                tags$span(style = "color: #555; font-size: 12px;", "Export  and summarize clean logger data"),
                tags$br(),
                tags$code(style = "color: #fe7f2d;", "write_clean_data(), summarise_clean_data(), plot_clean_timeseries()")
              )
            )
          ),

          # Authors card
          tags$div(
            style = "background-color: #fff; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",

            tags$h2("Authors & Contributors", style = "margin-top: 0; color: #233d4d; border-bottom: 2px solid #233d4d; padding-bottom: 10px;"),

            tags$p(
              "Department of Fisheries and Oceans, Canada",
              style = "font-weight: 600; color: #233d4d; margin-bottom: 20px;"
            ),

            fluidRow(
              column(
                width = 3,
                tags$h4("Package Authors:", style = "color: #233d4d; font-size: 14px; margin-bottom: 10px;"),
                tags$div(style = "font-size: 13px; line-height: 1.8;",
                  tags$div("Daniella LoScerbo"),
                  tags$div("Amanda Martens"),
                  tags$div("Julian Gan"),
                  tags$div("Paula Soto")
                )
              ),
              column(
                width = 3,
                tags$h4("Principle Investigators:", style = "color: #233d4d; font-size: 14px; margin-bottom: 10px;"),
                tags$div(style = "font-size: 13px; line-height: 1.8;",
                         tags$div("Douglas Braun"),
                         tags$div("Sean Naman"),
                         tags$div("Joel Harding")
                )
              ),
              column(
                width = 3,
                tags$h4("Reviewers:", style = "color: #233d4d; font-size: 14px; margin-bottom: 10px;"),
                tags$div(style = "font-size: 13px; line-height: 1.8;",
                         tags$div("David Reid"),
                         tags$div("Sarah Hnytka")
                )
              ),
              column(
                width = 3,
                tags$h4("App Developers, Technical Support:", style = "color: #233d4d; font-size: 14px; margin-bottom: 10px;"),
                tags$div(style = "font-size: 13px; line-height: 1.8;",
                  tags$div("Matthew Bayly (MJBA)"),
                  tags$div("Frank Poulsen (Ashlu)")
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
            "waterlogged: streamlined environmental data processing",
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
