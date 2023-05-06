# parcel_report Module
# rm(list = ls()); library(shiny); runApp()
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_parcel_report_ui <- function(id) {
    ns <- NS(id)

    tagList(
        shinydashboard::box(
            width = 12,
            fluidRow(
                column(
                    width = 12,
                    tags$h1("FDL Parcel Report", class = "custom-h1"),
                    tags$p(
                        "This page details summary data for each parcel providing the rental rate estimates and the input values used to drive each estimate. FDL parcel data is summarized as follows: a.) summarize total area per LSRS class per parcel (acre), b.) next estimate the rental rate per unit area for each LSRS class ($/acre), then c.) calculate the total value by multiplying the rate ($/acre) by total area (acres), d.) repeat this process for each of the Dry and Irr agricultural input type for all LSRS land classes and then e.) summarize final values for each parcel. To download this data, navigate to the Excel Download tab.",
                        class = "clean-print"
                    ),
                    tags$p("Use the following summary tables and plots on this page to determine if values make sense. Look carefully for any errors, issues, unexpected trends, or anomalies. If any issues are identified navigate back to the previous input data tabs or crop selector tabs to make necessary adjustments.",
                        class = "clean-print"
                    ),
                )
            ),


            fluidRow(
              column(
                width = 4,
                valueBoxOutput(ns("median_rate"), width = 12),
              ),
              column(
                width = 4,
                valueBoxOutput(ns("median_rent"), width = 12),
              ),
              column(
                width = 4,
                valueBoxOutput(ns("total_value"), width = 12),
              )
            ),


            fluidRow(
                column(
                    width = 12,
                    tags$h2("Parcel Report", class = "custom-h2"),
                    tags$p(
                        "Summary data  and rental rate calculations for each FDL parcel",
                        class = "clean-print"
                    ),
                    DT::dataTableOutput(ns("dt_parcel_report_table"))
                ),
            ),


            fluidRow(
              column(
                width = 12,

                tags$p(
                  "FDL Parcel Rental Rate Summary Plot: Showing to total rental rate for each parcel on the x-axis ($) and the rental rate value per acre ($/acre) on the y-axis. Use this plot to inspect individual parcels and identify any errors, issues, or anomalies. Scan for any extreme values or unexpected trends.",
                  class = "clean-print"
                ),
                plotlyOutput(ns("plotly_output")),
              )
            ),

        ),
        # Below box
        fluidRow(
            column(
                width = 12,
                tags$p("Farm Development Leases (FDL) Toolbox | contact mjbayly@mjbayly.com for more information", style = "text-align: center; color: #8A939A; margin-top: -15px; font-size: 12px;")
            )
        ),
    )
}

# Define server module for Excel workbook upload
module_parcel_report_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            print("loaded module_parcel_report_server")
            ns <- session$ns

            # rV_parcel_report_table_trigger
            session$userData$rV_parcel_report_table_trigger <- reactiveVal(0)


            # Listen to Excel data update
            parcel_report_table <- reactive({
                print("parcel_report_table() reactive expression triggered...")

                # rm(list = ls()); library(shiny); runApp()

                # Cant run until select crop data is ready
                req(session$userData$rv_crop_selection$rv_crop_selection)

                # Toggle reactive val on user load
                session$userData$rV_parcel_report_table_trigger()

                # Listen to change in input object
                all_data <- session$userData$rv_excel_data$rv_excel_data
                crops_selected <- session$userData$rv_crop_selection$rv_crop_selection

                if (is.null(all_data)) {
                    return(NULL)
                }

                if (is.null(crops_selected)) {
                    return(NULL)
                }

                if (all(crops_selected$selected == 0)) {
                    return(NULL)
                }

                print("running build_ltay_tables...")
                # rm(list = ls()); library(shiny); runApp()

                stop_year <- session$userData$rv_crop_selection_inputs$stop_year
                start_year <- session$userData$rv_crop_selection_inputs$start_year


                ltay <- suppressWarnings({
                    build_ltay_tables(
                        excel_data = all_data,
                        crops_selected = crops_selected,
                        start_year = start_year,
                        stop_year = stop_year
                    )
                })

                # Set default crop selection
                session$userData$rv_ltay$rv_ltay <- ltay

                print("running parcel_data_gis...")
                # rm(list = ls()); library(shiny); runApp()

                parcel_data <- parcel_data_gis(
                    excel_data = all_data,
                    ltay_tables = ltay
                )


                # Set default crop selection
                session$userData$rv_parcel_data$rv_parcel_data$dat <- parcel_data

                return(parcel_data)
            })



            # Rentder the data table
            output$dt_parcel_report_table <- DT::renderDataTable({
                print("render DT style for parcel_report...")
                # Get data from reactive object
                dt_data <- parcel_report_table()

                # Check if data is populated and fix style
                if (!(is.null(dt_data))) {

                    # rm(list = ls()); library(shiny); runApp()
                    # Get object for crops

                    # Set year to integer
                    # Set other columns to factor for multi-select
                    dt_data$`AFSC Crop Risk Zone` <- as.factor(dt_data$`AFSC Crop Risk Zone`)
                    dt_data$`FDL` <- as.character(dt_data$`FDL`)

                    # Rounding
                    dt_data <- dt_data %>% mutate_if(is.numeric, round, 3)

                    dt_data$`Total Acres (GIS FDL Boundary)` <- round(dt_data$`Total Acres (GIS FDL Boundary)`, 1)


                    c_index <- colnames(dt_data)[grepl("Dry Acres", colnames(dt_data))]
                    dt_data <- dt_data %>% mutate_at(c_index, round, 1)

                    c_index <- colnames(dt_data)[grepl("Irr Acres", colnames(dt_data))]
                    dt_data <- dt_data %>% mutate_at(c_index, round, 1)
                    # Colnames
                    colnames(dt_data) <- gsub("Total Value Dry", "Total Value Dry ($)", colnames(dt_data))
                    colnames(dt_data) <- gsub("Total Value Irr", "Total Value Irr ($)", colnames(dt_data))
                    colnames(dt_data) <- gsub("Total Value", "Total Value ($)", colnames(dt_data))
                    colnames(dt_data) <- gsub("TValue Per Acre", "Value Per Acre ($/Ac)", colnames(dt_data))

                    shinyjs::hide(id = "no_data_message")
                } else {

                    # browser()
                    # Show no data message
                    shinyjs::show(id = "no_data_message")
                    # Render empty table
                    dt_data <- tibble(selected = numeric())
                }

                # rm(list = ls()); library(shiny); runApp()


                if (nrow(dt_data) == 0) {
                    datatable(dt_data)
                } else {

                    # Render table
                    datatable(
                        dt_data,
                        filter = "top",
                        editable = FALSE,
                        rownames = FALSE,
                        selection = "none",
                        # extensions = "Buttons",
                        options = list(
                            paging = TRUE,
                            pageLength = 10,
                            scrollX = TRUE,
                            # dom = "Bfrtip",
                            search = list(regex = TRUE, caseInsensitive = FALSE),
                            # buttons = c("csv", "excel"),
                            # columnDefs = list(visible = FALSE, targets = c(7)),
                            columnDefs = list(list(className = "dt-center", targets = "_all"))
                        )
                    )
                }
            })





            # Value summary boxes

            output$total_value <- renderValueBox({

              dt_data <- parcel_report_table()

              if (is.null(dt_data)) {
                mval <- 0
              } else {

                mval <- sum(dt_data$`Total Value`, na.rm = TRUE)
                mval <- round(mval, 0)
                mval <- format(mval, nsmall=0, big.mark=",")  # 1,000.6
                mval <- paste0("$", mval, "/year")

              }
              valueBox(
                mval, "Total FDL Value ($)",
                icon = icon("unchecked", lib = "glyphicon"),
                color = "green"
              )
            })



            # Value summary boxes

            output$median_rate <- renderValueBox({

              dt_data <- parcel_report_table()


              if (is.null(dt_data)) {
                mval <- 0
              } else {

                mval <- median(dt_data$`Value Per Acre`, na.rm = TRUE)
                mval <- round(mval, 2)
                mval <- format(mval, nsmall=2, big.mark=",")  # 1,000.6
                mval <- paste0("$", mval, "/Acre")

              }
              valueBox(
                mval, "Median Rate ($/Acre)",
                icon = icon("unchecked", lib = "glyphicon"),
                color = "green"
              )
            })



            output$median_rent <- renderValueBox({

              dt_data <- parcel_report_table()

              if (is.null(dt_data)) {
                mval <- 0
              } else {

                mval <- median(dt_data$`Total Value`, na.rm = TRUE)
                mval <- round(mval, 0)
                mval <- format(mval, nsmall=0, big.mark=",")  # 1,000.6
                mval <- paste0("$", mval, "/FDL Parcel")

              }
              valueBox(
                mval, "Median Rent ($/parcel)",
                icon = icon("unchecked", lib = "glyphicon"),
                color = "green"
              )
            })




            # ------------------------------------------------------------
            # render plot button clicked -
            output$plotly_output <- renderPlotly({

              req(parcel_report_table())

              data <- parcel_report_table()

              if (nrow(data) == 0) {
                return()
              }

              print("rendering renderPlotly in PF...")

              data <- data[!(is.na(data$`Value Per Acre`)), ]
              data <- data[order(data$`Value Per Acre`), ]
              data <- data[, c("FDL", "AFSC Crop Risk Zone",
                               "Total Acres (GIS FDL Boundary)", "Not Cultivated Acres",
                               "Total Value", "Value Per Acre")]
              data$`Total Acres (GIS FDL Boundary)` <- round(data$`Total Acres (GIS FDL Boundary)`, 1)
              data$`Not Cultivated Acres` <- round(data$`Not Cultivated Acres`, 1)
              data$`Value Per Acre` <- round(data$`Value Per Acre`, 2)
              data$`Total Value` <- round(data$`Total Value`, 2)

              p <- ggplot(
                data = data,
                aes(
                  x = `Total Value`,
                  y = `Value Per Acre`,
                  col = `AFSC Crop Risk Zone`,
                  label = FDL
                )
              ) +
                geom_point()

              ggplotly(p)

            })







            # Other stuff...
        }
    )
}
