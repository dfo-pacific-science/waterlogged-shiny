# Parcel GIS
# rm(list = ls()); library(shiny); runApp()
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_parcel_gis_ui <- function(id) {
    ns <- NS(id)

    tagList(
        shinydashboard::box(
            width = 12,
            fluidRow(
                column(
                    width = 12,
                    tags$h1("FDL Parcel Data", class = "custom-h1"),
                    tags$p(
                        "The table below shows summary input data for FDL parcels. First, each FDL parcel (or property) is given a unique FDL number for identification. This ID links to other external datasets, including lease information and GIS files. Additional columns then show the breakdown of each parcel according to the AFSC Crop Risk Zone, Agricultural Input Type (Dry/Irrigated), LSRS risk rating and non-cultivated land.",
                        class = "clean-print"
                    ),
                    tags$p("Each parcel is given an AFSC Crop Risk Zone number. Each parcel must be in a single Risk Zone. If a parcel overlaps multiple risk zones on a boundary, choose the most representative Risk Zone.",
                        class = "clean-print"
                    ),
                    tags$p(
                        "The total area of each parcel is provided in the column 'Total Acres (GIS FDL Boundary)'; this value is then subdivided according to non-cultivated land, LSRS risk rating and Agricultural Input Type (Dry/Irrigated). The column 'Not Cultivated Acres' specifies the total area (in Acres) that is not cultivated. Subtracting the total acres from the non-cultivated acres provides the remaining cultivated area. This area is broken down across each of the LSRS risk rating classes for Agricultural Input Type: Dry and Agricultural Input Type: Irr (Irrigated). It is rare for any of the FDL leases to be irrigated except under special circumstances. Therefore, in most instances, each of the LSRS columns for Irrigated (Irr) class will be zero. The remaining cultivated land in the Dry class is then distributed across each of the LSRS columns.",
                        class = "clean-print"
                    ),
                    tags$p(
                        "The sum of the non-cultivated area and LSRS columns should (roughly) equal the 'Total Acres (GIS FDL Boundary)', but rounding errors and combined field, desktop and GIS methodologies may result in slightly different values. The FDL toolbox references the LSRS columns for all calculations, so please ensure that those are as accurate as possible.",
                        class = "clean-print"
                    ),
                )
            ),
            fluidRow(
                column(
                    width = 4,
                    valueBoxOutput(ns("total_parcel_count"), width = 12),
                ),
                column(
                    width = 4,
                    valueBoxOutput(ns("total_area"), width = 12),
                ),
                column(
                    width = 4,
                    valueBoxOutput(ns("total_cultivated_area"), width = 12),
                ),
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("FDL Parcel GIS Input Data", class = "custom-h2"),
                    div("No data available. Please upload an Excel workbook first to populate this table.", id = "no_data_message"),
                    DT::dataTableOutput(ns("dt_parcel_gis_table"))
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
module_parcel_gis_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            print("loaded module_parcel_gis_server")
            ns <- session$ns

            # rV_parcel_gis_table_trigger
            session$userData$rV_parcel_gis_table_trigger <- reactiveVal(0)

            # Listen to Excel data update
            parcel_gis_table <- reactive({
                print("parcel_gis_table() reactive expression triggered...")
                # browser()
                # Toggle reactive val on user load
                session$userData$rV_parcel_gis_table_trigger()
                # Listen to change in object
                all_data <- session$userData$rv_excel_data$rv_excel_data
                parcel_gis_data <- all_data$`FDL GIS`
                return(parcel_gis_data)
            })



            # Update info boxes
            output$total_parcel_count <- renderValueBox({
                dt_data <- parcel_gis_table()
                if (is.null(dt_data)) {
                    mval <- 0
                } else {
                    # Total count of all FDL Parcels
                    mval <- length(unique(dt_data$FDL))
                    mval <- round(mval, 1)
                }
                valueBox(
                    mval, "FDL Parcels",
                    icon = icon("map-marker", lib = "glyphicon"),
                    color = "green"
                )
            })

            output$total_area <- renderValueBox({
                dt_data <- parcel_gis_table()
                if (is.null(dt_data)) {
                    mval <- 0
                } else {
                    # Total Acres of FDL Parcels
                    mval <- sum(dt_data$`Total Acres (GIS FDL Boundary)`, na.rm = TRUE)
                    mval <- round(mval, 1)
                }
                valueBox(
                    mval, "Total FDL Acres",
                    icon = icon("unchecked", lib = "glyphicon"),
                    color = "green"
                )
            })

            output$total_cultivated_area <- renderValueBox({
                dt_data <- parcel_gis_table()
                if (is.null(dt_data)) {
                    mval <- 0
                } else {
                    # Total total_cultivated_area Acres of FDL Parcels
                    df_sub <- dt_data %>% select(contains("LSRS"))
                    mval <- sum(df_sub, na.rm = TRUE)
                    mval <- round(mval, 1)
                }
                valueBox(
                    mval, "Total FDL Cultivated Acres",
                    icon = icon("check", lib = "glyphicon"),
                    color = "green"
                )
            })





            # Rentder the data table
            output$dt_parcel_gis_table <- DT::renderDataTable({
                print("render DT style for parcel_gis...")
                # Get data from reactive object
                dt_data <- parcel_gis_table()

                # Check if data is populated and fix style
                if (!(is.null(dt_data))) {
                    # Set year to integer
                    dt_data$FDL <- as.character(dt_data$FDL)
                    # Set other columns to factor for multi-select
                    dt_data$`AFSC Crop Risk Zone` <- as.factor(dt_data$`AFSC Crop Risk Zone`)
                    # Round columns
                    dt_data <- dt_data %>% mutate_if(is.numeric, round, 3)
                    shinyjs::hide(id = "no_data_message")
                } else {
                    # browser()
                    # Show no data message
                    shinyjs::show(id = "no_data_message")
                    # Render empty table
                    dt_data <- tibble()
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
                        scrollX = TRUE,
                        search = list(regex = TRUE, caseInsensitive = FALSE),
                        columnDefs = list(list(className = "dt-center", targets = "_all"))
                    )
                )
            })



            # ------------------------------------------------------------
            # Detect cell edits
            observeEvent(input$dt_parcel_gis_table_cell_edit, {
                print("dt_parcel_gis_table_cell_edit...")
                print(Sys.time())

                cell <- input$dt_parcel_gis_table_cell_edit

                row_edit <- cell$row
                col_edit <- cell$col + 1
                new_value <- cell$value

                # rm(list = ls()); library(shiny); runApp()

                session$userData$rv_excel_data$rv_excel_data$`FDL GIS`[row_edit, col_edit] <- new_value

                # Toggle reactive val on user load
                newvalue <- session$userData$rV_parcel_gis_table_trigger() + 1
                session$userData$rV_parcel_gis_table_trigger(newvalue)
            })



            # Other stuff...
        }
    )
}
