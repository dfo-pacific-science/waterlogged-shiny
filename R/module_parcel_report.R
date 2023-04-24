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
                        "Placeholder text for parcel_report module....",
                        class = "clean-print"
                    ),
                    tags$p("Placeholder text for parcel_report module....",
                        class = "clean-print"
                    ),
                    tags$p(
                        "Placeholder text for parcel_report module....",
                        class = "clean-print"
                    ),
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("parcel_report By Crop", class = "custom-h2"),
                    tags$p(
                        "Placeholder text for parcel_report module by crop type...",
                        class = "clean-print"
                    ),
                    DT::dataTableOutput(ns("dt_parcel_report_table"))
                ),
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






            # Other stuff...
        }
    )
}
