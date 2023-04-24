# Price Fractions Module
# rm(list = ls()); library(shiny); runApp()
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_lsrs_ui <- function(id) {
    ns <- NS(id)

    tagList(
        shinydashboard::box(
            width = 12,
            fluidRow(
                column(
                    width = 12,
                    tags$h1("Land Suitability Rating System (LSRS)", class = "custom-h1"),
                    tags$p(
                        "The land suitability rating system (LSRS) is a procedure for rating the suitability of land for agricultural spring-seeded small grains and hardy oilseeds. The system is based on the soil, climate, and landscape potential. It is an improvement on the Canada Land Inventory capability rating that it replaces. This one-page reference tool offers guidance for interpreting an LSRS rating and provides a link to a more detailed discussion of the rating.",
                        class = "clean-print"
                    ),
                    tags$p(
                        "See explainers of the LSRS system here:",
                        class = "clean-print"
                    ),
                    tags$div(
                        class = "clean-print",
                        a("Land Suitability Rating System (LSRS)", href = "https://open.alberta.ca/publications/land-suitability-rating-system#:~:text=The%20land%20suitability%20rating%20system,small%20grains%20and%20hardy%20oilseeds", target = "_blank"),
                    ),
                    tags$p(
                        "Suitability definitions are classed into different levels from 1 being the most suitable and 7 being the least suitable. LSRS is a national rating system and, due to climate, Alberta has no Class 1 land, so the scale starts at Class 2. Class 2 limitations: none to slight, Class 3 limitations: moderate, Class 4 limitations: severe, Class 5 limitations: very severe, Class 6 limitations extremely severe & Class 7 limitations: unsuitable. Class 7 limitations will always be classified as Not Cultivated Acres.",
                        class = "clean-print"
                    ),
                    tags$p(
                        "In the FDL toolbox, each of the FDL parcels are subdivided by percent allocations to each LSRS rating class. They are further subdivided according to their Agricultural Input Type: Dry (dry) or Irr (irrigated), such that each FDL parcel will have as estimate of Dry Acres and Irr Acres for each LSRS class.",
                        class = "clean-print"
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("LSRS Input Data", class = "custom-h2"),
                    div("No data available. Please upload an Excel workbook first to populate this table.", id = "no_data_message"),
                    DT::dataTableOutput(ns("dt_lsrs_table"))
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
module_lsrs_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            print("loaded module_lsrs_server")
            ns <- session$ns

            # rV_lsrs_table_trigger
            session$userData$rV_lsrs_table_trigger <- reactiveVal(0)

            # Listen to Excel data update
            lsrs_table <- reactive({
                print("lsrs_table() reactive expression triggered...")
                # browser()
                # Toggle reactive val on user load
                session$userData$rV_lsrs_table_trigger()
                # Listen to change in object
                all_data <- session$userData$rv_excel_data$rv_excel_data
                lsrs_data <- all_data$LSRS

                # rm(list = ls()); library(shiny); runApp()

                return(lsrs_data)
            })



            # Rentder the data table
            output$dt_lsrs_table <- DT::renderDataTable({
                print("render DT style for lsrs...")
                # Get data from reactive object
                dt_data <- lsrs_table()

                # Check if data is populated and fix style
                if (!(is.null(dt_data))) {

                    # Set year to integer
                    # Set other columns to factor for multi-select
                    dt_data$`Reference ID` <- as.character(dt_data$`Reference ID`)
                    dt_data$`Full Name` <- as.character(dt_data$`Full Name`)
                    dt_data$`Percentage (%)` <- as.numeric(as.character(dt_data$`Percentage (%)`))
                    dt_data$Limitation <- as.character(dt_data$Limitation)
                    shinyjs::hide(id = "no_data_message")
                } else {
                    # browser()
                    # Show no data message
                    shinyjs::show(id = "no_data_message")
                    # Render empty table
                    dt_data <- tibble(
                        `Reference ID` = as.character(),
                        `Full Name` = as.character(),
                        `Percentage (%)` = as.character(),
                        `Limitation` = as.character()
                    )
                }

                # rm(list = ls()); library(shiny); runApp()


                # Render table
                datatable(
                    dt_data,
                    # filter = "top",
                    editable = "cell",
                    rownames = FALSE,
                    selection = "none",
                    options = list(
                        paging = TRUE,
                        pageLength = 10,
                        # search = list(regex = TRUE, caseInsensitive = FALSE),
                        columnDefs = list(list(className = "dt-center", targets = "_all"))
                    )
                )
            })



            # ------------------------------------------------------------
            # Detect cell edits
            observeEvent(input$dt_lsrs_table_cell_edit, {
                print("dt_lsrs_table_cell_edit...")
                print(Sys.time())

                cell <- input$dt_lsrs_table_cell_edit

                row_edit <- cell$row
                col_edit <- cell$col + 1
                # can only be 3
                new_value <- cell$value

                if (col_edit == 3) {
                    session$userData$rv_excel_data$rv_excel_data$LSRS[row_edit, 3] <- new_value

                    # Toggle reactive val on user load
                    newvalue <- session$userData$rV_lsrs_table_trigger() + 1
                    session$userData$rV_lsrs_table_trigger(newvalue)
                }

                # rm(list = ls()); library(shiny); runApp()
            })



            # Other stuff...
        }
    )
}
