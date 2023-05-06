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
                        "The long-term average yield summary tables are broken down by crop type and risk zone. Use the filters available in each table to manually check and review several values. Once satisfied, move onto the next module for calculations of rental rates per FDL parcel unit.",
                        class = "clean-print"
                    ),
                    tags$p("Cell values in the LTAY table are not editable since they are derived calculations from the input data. If all values are blank and no tables appear (but data has been uploaded), then go back to the crop selector module and ensure that there are enough representative crops for each risk zone.",
                        class = "clean-print"
                    ),
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("LTAY By Crop", class = "custom-h2"),
                    tags$p(
                        "LTAY summary table by crop type. These estimates are compiled to produce of the next table of rates by risk zone and LSRS class.",
                        class = "clean-print"
                    ),
                    tags$p(
                      "It makes the most sense to review the LTAY table on a per risk zone basis. The Average Crop Composition is reported per risk zone and sums to 1 across each risk zone. The Gross Revenue Per Commodity Per Acre (GRPCPA) estimates to dominate crop composition (by value) for each risk zone. The actual value (per acre of land) Gross Revenue ($/Acre) is then estimated by taking the sum of the GRPCPA for each risk zone. Gross revenue per acre is reported on the subsequent table.",
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
                        "These values are used as underlying the rates for each of the FDL parcel calculations. For example, if an FDL parcel consisted of 1 acre of LSRS Class X with a rate of $50/acre then the rental rate would be $50.",
                        class = "clean-print"
                    ),
                    tags$p(
                      "LSRS classes are discounted from the total Gross Revenue ($/Acre) based on the land suitability soil class rating. For example, if you return to the LSRS module and set all suitability percentage scores to 100%, then the Gross Revenue will be equal to each of the LSRS class columns.",
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
                    colnames(dt_data) <- gsub("grpcpa", "Gross Revenue Per Commodity Per Acre ($/Ac)", colnames(dt_data))

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
                    colnames(dt_data) <- gsub("dry_LSRS", "Rental Rate ($) Dry LSRS", colnames(dt_data))
                    colnames(dt_data) <- gsub("irr_LSRS", "Rental Rate ($) Irr LSRS", colnames(dt_data))

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
