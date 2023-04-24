# LTAY Module
# rm(list = ls()); library(shiny); runApp()
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_ltay_ui <- function(id) {
    ns <- NS(id)

    tagList(
        shinydashboard::box(
            width = 12,
            fluidRow(
                column(
                    width = 12,
                    tags$h1("Long Term Average Yield (LTAY)", class = "custom-h1"),
                    tags$p(
                        "Placeholder text for LTAY module....",
                        class = "clean-print"
                    ),
                    tags$p("Placeholder text for LTAY module....",
                        class = "clean-print"
                    ),
                    tags$p(
                        "Placeholder text for LTAY module....",
                        class = "clean-print"
                    ),
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("LTAY By Crop", class = "custom-h2"),
                    tags$p(
                        "Placeholder text for LTAY module by crop type...",
                        class = "clean-print"
                    ),
                    DT::dataTableOutput(ns("dt_ltay_table_crop"))
                ),
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("LTAY by Risk Zone", class = "custom-h2"),
                    tags$p(
                        "Placeholder text for LTAY module by risk zone...",
                        class = "clean-print"
                    ),
                    DT::dataTableOutput(ns("dt_ltay_table_zone"))
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
module_ltay_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            print("loaded module_ltay_server")
            ns <- session$ns

            # rV_ltay_table_trigger
            session$userData$rV_ltay_table_trigger <- reactiveVal(0)


            # Listen to Excel data update
            ltay_table <- reactive({
                print("ltay_table() reactive expression triggered...")

                # rm(list = ls()); library(shiny); runApp()

                # Cant run until select crop data is ready
                req(session$userData$rv_crop_selection$rv_crop_selection)

                # Toggle reactive val on user load
                session$userData$rV_ltay_table_trigger()

                # Listen to change in input object
                all_data <- session$userData$rv_excel_data$rv_excel_data
                crops_selected <- session$userData$rv_crop_selection$rv_crop_selection

                # Wach for updates to inputs
                start_year <- session$userData$rv_crop_selection_inputs$start_year
                stop_year <- session$userData$rv_crop_selection_inputs$stop_year

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

                return(ltay)
            })



            # Rentder the data table
            output$dt_ltay_table_crop <- DT::renderDataTable({
                print("render DT style for ltay...")
                # Get data from reactive object
                dt_data <- ltay_table()

                # Check if data is populated and fix style
                if (!(is.null(dt_data))) {

                    # rm(list = ls()); library(shiny); runApp()
                    # Get object for crops
                    dt_data <- dt_data[["ltay"]]

                    # Set year to integer
                    # Set other columns to factor for multi-select
                    dt_data$`RISK AREA` <- as.factor(dt_data$`RISK AREA`)
                    dt_data$`CROP NAME` <- as.factor(dt_data$`CROP NAME`)
                    dt_data$`AG INPUT` <- as.factor(dt_data$`AG INPUT`)

                    # Rounding
                    dt_data <- dt_data %>% mutate_if(is.numeric, round, 3)
                    c_index <- colnames(dt_data)[grepl("ay_", colnames(dt_data))]
                    dt_data <- dt_data %>% mutate_at(c_index, round, 1)

                    # Colnames
                    colnames(dt_data) <- gsub("ay_", "Annual Yields (kg/Ac) ", colnames(dt_data))
                    colnames(dt_data) <- gsub("ag_", "Acres Grown (Ac, per risk zone) ", colnames(dt_data))

                    colnames(dt_data) <- gsub("average_yield_kg_ac", "Yield (kg/Ac, 5-yr Avg)", colnames(dt_data))
                    colnames(dt_data) <- gsub("average_acres_grown", "Acres Grown (Ac, 5-yr Avg)", colnames(dt_data))
                    colnames(dt_data) <- gsub("acres_per_risk", "Acres per Risk Zone", colnames(dt_data))
                    colnames(dt_data) <- gsub("average_crop_compostion", "Average Crop Compostion", colnames(dt_data))
                    colnames(dt_data) <- gsub("price", "Price ($/Kg)", colnames(dt_data))
                    colnames(dt_data) <- gsub("grpcpa", "Gross Revenue Per Commodity Per Acre ($)", colnames(dt_data))

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





            # Rentder the data table
            output$dt_ltay_table_zone <- DT::renderDataTable({
                print("render DT style for ltay...")
                # Get data from reactive object
                dt_data <- ltay_table()

                # Check if data is populated and fix style
                if (!(is.null(dt_data))) {

                    # rm(list = ls()); library(shiny); runApp()
                    # Get object for crops
                    dt_data <- dt_data[["risk_area_lsrs"]]

                    # Set year to integer
                    # Set other columns to factor for multi-select
                    dt_data$`RISK AREA` <- as.factor(dt_data$`RISK AREA`)

                    # Rounding
                    dt_data <- dt_data %>% mutate_if(is.numeric, round, 3)


                    c_index <- colnames(dt_data)[grepl("dry_LSRS", colnames(dt_data))]
                    dt_data <- dt_data %>% mutate_at(c_index, round, 2)

                    c_index <- colnames(dt_data)[grepl("irr_LSRS", colnames(dt_data))]
                    dt_data <- dt_data %>% mutate_at(c_index, round, 2)

                    # Colnames
                    colnames(dt_data) <- gsub("dry_LSRS", "Rental Rate ($) LSRS", colnames(dt_data))
                    colnames(dt_data) <- gsub("irr_LSRS", "Rental Rate ($) LSRS", colnames(dt_data))

                    colnames(dt_data) <- gsub("dry_gross_rev", "Gross Revenue Dry ($/Acre)", colnames(dt_data))
                    colnames(dt_data) <- gsub("irr_gross_rev", "Gross Revenue Irr ($/Acre)", colnames(dt_data))


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



            # Other stuff...
        }
    )
}
