# Excel Upload Module
# rm(list = ls()); library(shiny); runApp()
# Bad file: C:\Users\mattj\Dropbox\Projects\EN2739 CCIRA\ccira-watersheds\data
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_average_yields_ui <- function(id) {
    ns <- NS(id)

    tagList(
        shinydashboard::box(
            width = 12,
            fluidRow(
                column(
                    width = 12,
                    tags$h1("Average Yield Input Data", class = "custom-h1"),
                    tags$p(
                        "The following table shows the Average Yield input data. These values should be updated in the original Excel workbook prior to being uploaded to the app, but it’s also possible review and edit values on this page within the current session. The Average Yield input data is broken down by year, risk area (from the AFSC reporting criteria), crop name (from the FDL dataset), agricultural input (Dry: Dryland or Irr: Irrigated).",
                        class = "clean-print"
                    ),
                    tags$p(
                        "Summarized values include the average yield (kg/acre) and the total crop coverage (in acres). Some crop types will have two or more rows of data for a given year if grown across different risk zones and agricultural input types. Refer to the Agriculture Financial Services Corporation (AFSC, https://afsc.ca/?s=yield) for additional information.",
                        class = "clean-print"
                    ),
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("Average Yield Data", class = "custom-h2"),
                    div("No data available. Please upload an Excel workbook first to populate this table.", id = "no_data_message"),
                    DT::dataTableOutput(ns("dt_ay_table"))
                ),
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("Average Yield Summary Plots", class = "custom-h2"),
                    tags$p(
                        "Display crop summary plot based on table filter selection. It can be useful to review crop production across Dryland and Irrigated categories to ensure there are no data entry errors (i.e., do values make sense). We also suggest reviewing different varieties of wheat, beans etc. to ensure there are no duplicates (i.e., variety entered twice with spelling error).",
                        class = "clean-print"
                    ),
                    plotlyOutput(ns("plotly_output"))
                ),
            )
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
module_average_yields_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            print("loaded module_module_average_yields_server")
            ns <- session$ns

            # rV_ay_table_trigger
            session$userData$rV_ay_table_trigger <- reactiveVal(0)

            # Listen to Excel data update
            ay_table <- reactive({
                print("ay_table() reactive expression triggered...")
                # browser()
                # Toggle reactive val on user load
                session$userData$rV_ay_table_trigger()
                # Listen to change in object
                all_data <- session$userData$rv_excel_data$rv_excel_data
                ay_data <- all_data$`Average Yield`
                return(ay_data)
            })


            # Rentder the data table
            output$dt_ay_table <- DT::renderDataTable({
                print("render DT style...")
                # Get data from reactive object
                dt_data <- ay_table()

                # Check if data is populated and fix style
                if (!(is.null(dt_data))) {
                    # Set year to integer
                    dt_data$YEAR <- as.integer(dt_data$YEAR)
                    # Set other columns to factor for multi-select
                    dt_data$`AG INPUT` <- as.factor(dt_data$`AG INPUT`)
                    dt_data$`CROP NAME` <- as.factor(dt_data$`CROP NAME`)
                    dt_data$`RISK AREA` <- as.factor(dt_data$`RISK AREA`)
                    shinyjs::hide(id = "no_data_message")
                } else {
                    # browser()
                    # Show no data message
                    shinyjs::show(id = "no_data_message")
                    # Render empty table
                    dt_data <- tibble(
                        YEAR = integer(),
                        `AG INPUT` = factor(),
                        `CROP NAME` = factor(),
                        `RISK AREA` = factor(),
                        `ACRES` = integer(),
                        `AVERAGE YIELD (kg/acre)` = numeric()
                    )
                }

                # Render table
                datatable(
                    dt_data,
                    filter = "top",
                    editable = "cell",
                    rownames = FALSE,
                    selection = "none",
                    options = list(
                        paging = TRUE,
                        pageLength = 10,
                        search = list(regex = TRUE, caseInsensitive = FALSE),
                        columnDefs = list(list(className = "dt-center", targets = "_all"))
                    )
                ) %>%
                    formatRound(columns = c("AVERAGE YIELD (kg/acre)"), digits = 1) %>%
                    formatRound(columns = c("ACRES"), digits = 0)
            })



            # ------------------------------------------------------------
            # Detect cell edits
            observeEvent(input$dt_ay_table_cell_edit, {
                print("dt_ay_table_cell_edit...")
                print(Sys.time())

                cell <- input$dt_ay_table_cell_edit

                row_edit <- cell$row
                col_edit <- cell$col + 1
                new_value <- cell$value

                session$userData$rv_excel_data$rv_excel_data$`Average Yields`[row_edit, col_edit] <- new_value


            })




            # ------------------------------------------------------------
            # gather plot button clicked - gather filtered state of datatable
            plot_data <- reactive({
                print("plot_data <- reactive in AY...")
                print(Sys.time())
                req(ay_table())


                dt_data <- ay_table()

                # Listen to change in table state
                if (is.null(input$dt_ay_table_search_columns)) {
                    return(dt_data)
                }

                # Filter year
                if (input$dt_ay_table_search_columns[1] != "") {
                    mstr <- input$dt_ay_table_search_columns[1]
                    msplit <- strsplit(mstr, " ... ")[[1]]
                    starts <- msplit[1]
                    stop <- msplit[2]
                    dt_data <- dt_data[dt_data$YEAR >= starts & dt_data$YEAR <= stop, ]
                }

                # Filter `RISK AREA`
                if (input$dt_ay_table_search_columns[2] != "") {
                    mstr <- input$dt_ay_table_search_columns[2]
                    msplit <- jsonlite::fromJSON(mstr)
                    dt_data <- dt_data[dt_data$`RISK AREA` %in% c(msplit), ]
                }

                # Filter `CROP NAME`
                if (input$dt_ay_table_search_columns[3] != "") {
                    mstr <- input$dt_ay_table_search_columns[3]
                    msplit <- jsonlite::fromJSON(mstr)
                    dt_data <- dt_data[dt_data$`CROP NAME` %in% c(msplit), ]
                }

                # Filter `AG INPUT`
                if (input$dt_ay_table_search_columns[4] != "") {
                    mstr <- input$dt_ay_table_search_columns[4]
                    msplit <- jsonlite::fromJSON(mstr)
                    dt_data <- dt_data[dt_data$`AG INPUT` %in% c(msplit), ]
                }

                # Filter `AVERAGE YIELD (kg/acre)`
                if (input$dt_ay_table_search_columns[5] != "") {
                    mstr <- input$dt_ay_table_search_columns[5]
                    msplit <- strsplit(mstr, " ... ")[[1]]
                    starts <- msplit[1]
                    stop <- msplit[2]
                    dt_data <- dt_data[dt_data$`AVERAGE YIELD (kg/acre)` >= starts & dt_data$`AVERAGE YIELD (kg/acre)` <= stop, ]
                }

                # Filter ACRES
                if (input$dt_ay_table_search_columns[6] != "") {
                    mstr <- input$dt_ay_table_search_columns[6]
                    msplit <- strsplit(mstr, " ... ")[[1]]
                    starts <- msplit[1]
                    stop <- msplit[2]
                    dt_data <- dt_data[dt_data$ACRES >= starts & dt_data$ACRES <= stop, ]
                }

                return(dt_data)
            })

            # ------------------------------------------------------------
            # render plot button clicked -
            output$plotly_output <- renderPlotly({
                req(plot_data())

                print("rendering renderPlotly in AY...")

                data <- plot_data()

                if(nrow(data) == 0) {
                    return()
                }

                total_acres_grown <- data %>%
                    group_by(`CROP NAME`, YEAR) %>%
                    summarise(`Acres Grown` = sum(ACRES))

                p <- ggplot(
                    data = total_acres_grown,
                    aes(
                        x = YEAR,
                        y = `Acres Grown`,
                        color = as.factor(`CROP NAME`)
                    )
                ) +
                    geom_line() +
                    geom_point() +
                    theme_bw()

                ggplotly(p)
            })

            # Other stuff...
        }
    )
}
