# Price Fractions Module
# rm(list = ls()); library(shiny); runApp()
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_price_fractions_ui <- function(id) {
    ns <- NS(id)

    tagList(
        shinydashboard::box(
            width = 12,
            fluidRow(
                column(
                    width = 12,
                    tags$h1("Price Fractions", class = "custom-h1"),
                    tags$p(
                        "The following table shows the Price Fraction input data ($/kg for each crop type). Ensure that the spelling of the CROP NAME field exactly matching the Average Yields input data. The original name and spelling of each crop variety can remain unmodified in the column 'AFSC Commodity Forecast (Reported As)' column for reference purposes, but it is important that the crop names match between tables. It is possible to include multiple years of Price Fraction data for each crop type for review. However, the calculator will reference the most recent year's data for each crop for reporting. For example, if most crops have Price Fraction up to 2022, but several varieties only have data up to 2018, the FDL toolbox will use the most recent years data for each crop type. Additional modifications should be made in the Excel workbook before uploading. The GRADE column is included for reference purposes only and is not used in any of the calculations.",
                        class = "clean-print"
                    ),
                    tags$p(
                        "FDL toolbox users must ensure that the Price Fraction data matches the Average Yield input data. Watch out for instances where the spelling is slightly different between the two datasheets and make corrections by clicking on the cell and editing values (e.g., Wht-HRS vs Wheat-HRS).",
                        class = "clean-print"
                    ),
                    div(
                        class = "clean-print", style = "color: darkred; text-align: center;
                            padding: 5px;
                            margin: 5px;
                            background-color: #ffebeb;
                            border-radius: 15px;
                            border: 3px solid #9e7474;
                            font-weight: bold;",
                        div(
                            textOutput(ns("txt_crops_not_in_pf")),
                            textOutput(ns("txt_crops_not_in_pf2"))
                        ),
                    ),
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("Price Fraction Input Data", class = "custom-h2"),
                    div("No data available. Please upload an Excel workbook first to populate this table.", id = "no_data_message"),
                    DT::dataTableOutput(ns("dt_pf_table"))
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
module_price_fractions_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            print("loaded module_price_fractions_server")
            ns <- session$ns

            # rV_pf_table_trigger
            session$userData$rV_pf_table_trigger <- reactiveVal(0)

            # Listen to Excel data update
            pf_table <- reactive({
                print("pf_table() reactive expression triggered...")
                # browser()
                # Toggle reactive val on user load
                session$userData$rV_pf_table_trigger()
                # Listen to change in object
                all_data <- session$userData$rv_excel_data$rv_excel_data
                pf_data <- all_data$`Price Fractions`


                # Check spelling match between price fractions
                # and average yields tables
                if (!(is.null(pf_data))) {

                    # Add column for status of crop name
                    pf_data$ok <- TRUE

                    # Get crop names from price fractions table
                    pf_crops <- unique(pf_data$`CROP NAME`)
                    # Get crop names from average yields table
                    ay_crops <- unique(session$userData$rv_excel_data$rv_excel_data$`Average Yields`$`CROP NAME`)
                    ay_crops <- as.character(ay_crops)
                    # Check if all crops in price fractions table are in average yields table
                    if (!(all(pf_crops %in% ay_crops))) {
                        # Get crops that are in price fractions table but not average yields table
                        missing_crops <- pf_crops[!(pf_crops %in% ay_crops)]
                        # Highlight these red in price fraction table
                        if (length(missing_crops) > 0) {
                            session$userData$rv_crop_names$rv_only_in_price_fraction <- missing_crops
                            msg1 <- paste0(
                                "The following crops are in the Price Fractions table but not the Average Yields table: ",
                                paste0(missing_crops, collapse = ", ")
                            )

                            # Update row status indicator
                            pf_data$ok <- ifelse(pf_data$`CROP NAME` %in% missing_crops, FALSE, pf_data$ok)
                        } else {
                            msg1 <- ""
                        }

                        # Get crops that are in average yields table but not price fractions table
                        extra_crops <- ay_crops[!(ay_crops %in% pf_crops)]
                        if (length(extra_crops) > 0) {
                            session$userData$rv_crop_names$rv_only_in_avg_yeild <- extra_crops
                            msg2 <- paste0(
                                "The following crops are in the Average Yields table but not the Price Fractions table: ",
                                paste0(extra_crops, collapse = ", ")
                            )

                            # Calculate the total missing area - how bad is it?
                            tmp_dat <- session$userData$rv_excel_data$rv_excel_data$`Average Yields`
                            tmp_dat <- tmp_dat[which(tmp_dat$YEAR == max(tmp_dat$YEAR, na.rm = TRUE)), ]
                            total_area <- sum(tmp_dat$ACRES, na.rm = TRUE)
                            tmp_dat <- tmp_dat[which(tmp_dat$`CROP NAME` %in% extra_crops), ]
                            missing_area <- sum(tmp_dat$ACRES, na.rm = TRUE)
                            msg3 <- paste0("Collectivly, these crops appearing only in the Average Yield table account for ", round(missing_area / total_area * 100, 2), "% of the total crop area (in ", max(tmp_dat$YEAR, na.rm = TRUE), "). ")

                            # Add message to reactive for reference
                            session$userData$rv_crop_names$rv_total_missing_area <- msg3
                            msg2 <- paste0(msg2, ". ", msg3)
                        } else {
                            msg2 <- ""
                        }

                        # Create message
                        message <- paste0(
                            "WARNING: ", msg1, "... ", msg2,
                            ". Please ensure that the spelling of the CROP NAME field exactly matching the Average Yields input data."
                        )
                        # Show message
                        shiny::showNotification(message, type = "error", duration = 0)
                    }
                }

                # rm(list = ls()); library(shiny); runApp()

                return(pf_data)
            })



            # Display warning text of crops not in Price Fraction table
            output$txt_crops_not_in_pf <- renderText({
                # Get reactive object
                extra_crops <- session$userData$rv_crop_names$rv_only_in_avg_yeild
                # Check if object is populated
                if (!(is.null(extra_crops))) {
                    # Create message
                    message <- paste0(
                        "The following crops are in the Average Yields table but not the Price Fraction table: ",
                        paste0(extra_crops, collapse = ", "),
                        ". Please add data to the Price Fraction sheet in the Excel workbook and reupload. The calculator will assume a value of $0/kg for these crops otherwise."
                    )
                    # Show message
                    return(message)
                } else {
                    return("")
                }
            })


            # Display warning text of crops not in Price Fraction table
            output$txt_crops_not_in_pf2 <- renderText({
                # Get reactive object
                missing_area_txt <- session$userData$rv_crop_names$rv_total_missing_area
                # Check if object is populated
                if (!(is.null(missing_area_txt))) {
                    return(missing_area_txt)
                } else {
                    return("")
                }
            })




            # Rentder the data table
            output$dt_pf_table <- DT::renderDataTable({
                print("render DT style for PF...")
                # Get data from reactive object
                dt_data <- pf_table()

                # Check if data is populated and fix style
                if (!(is.null(dt_data))) {
                    # Set year to integer
                    dt_data$YEAR <- as.integer(dt_data$YEAR)
                    # Set other columns to factor for multi-select
                    dt_data$`CROP NAME` <- as.character(dt_data$`CROP NAME`)
                    dt_data$`AFSC Commodity Forecast (Reported As)` <- as.character(dt_data$`AFSC Commodity Forecast (Reported As)`)
                    dt_data$GRADE <- as.character(dt_data$GRADE)
                    dt_data$`PRICE ($/kg)` <- as.numeric(as.character(dt_data$`PRICE ($/kg)`))

                    shinyjs::hide(id = "no_data_message")
                } else {
                    # browser()
                    # Show no data message
                    shinyjs::show(id = "no_data_message")
                    # Render empty table
                    dt_data <- tibble(
                        YEAR = integer(),
                        `CROP NAME` = as.character(),
                        `AFSC Commodity Forecast (Reported As)` = as.character(),
                        `GRADE` = as.character(),
                        `PRICE ($/kg)` = numeric(),
                        ok = logical()
                    )
                }

                # rm(list = ls()); library(shiny); runApp()


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
                    formatRound(columns = c("PRICE ($/kg)"), digits = 3) %>%
                    formatStyle(
                        "ok",
                        backgroundColor = styleEqual(c(FALSE), c("#DE8989"))
                    )
            })



            # ------------------------------------------------------------
            # Detect cell edits
            observeEvent(input$dt_pf_table_cell_edit, {
                print("dt_pf_table_cell_edit...")
                print(Sys.time())

                cell <- input$dt_pf_table_cell_edit

                row_edit <- cell$row
                col_edit <- cell$col + 1
                new_value <- cell$value

                # rm(list = ls()); library(shiny); runApp()

                session$userData$rv_excel_data$rv_excel_data$`Price Fractions`[row_edit, col_edit] <- new_value

                # Toggle reactive val on user load
                newvalue <- session$userData$rV_pf_table_trigger() + 1
                session$userData$rV_pf_table_trigger(newvalue)
            })




            # ------------------------------------------------------------
            # gather plot button clicked - gather filtered state of datatable
            plot_data <- reactive({
                print("plot_data <- reactive in AY...")
                print(Sys.time())
                req(pf_table())

                dt_data <- pf_table()

                # Check rows greater than zero
                if (nrow(dt_data) == 0) {
                    return(NULL)
                }

                if (nrow(dt_data) > 0) {
                    # Get the most recent year by crop type
                    dt2 <- dt_data %>%
                        group_by(`CROP NAME`) %>%
                        top_n(1, YEAR)
                    return(dt2)
                }

                return(dt_data)
            })

            # ------------------------------------------------------------
            # render plot button clicked -
            output$plotly_output <- renderPlotly({
                req(plot_data())

                print("rendering renderPlotly in PF...")

                data <- plot_data()

                if (nrow(data) == 0) {
                    return()
                }

                data <- data[!(is.na(data$`PRICE ($/kg)`)), ]
                data <- data[order(data$`PRICE ($/kg)`), ]

                data$`CROP NAME` <- factor(data$`CROP NAME`, levels = data$`CROP NAME`)

                data$`PRICE ($/kg)` <- round(data$`PRICE ($/kg)`, 3)

                p <- ggplot(
                    data = data,
                    aes(
                        x = `CROP NAME`,
                        y = `PRICE ($/kg)`
                    )
                ) +
                    geom_bar(stat = "identity") +
                    coord_flip()


                ggplotly(p)
            })

            # Other stuff...
        }
    )
}
