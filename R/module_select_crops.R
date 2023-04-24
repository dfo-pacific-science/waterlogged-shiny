# Select Crops Module
# rm(list = ls()); library(shiny); runApp()
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_select_crops_ui <- function(id) {
    ns <- NS(id)

    tagList(
        shinydashboard::box(
            width = 12,
            fluidRow(
                column(
                    width = 12,
                    tags$h1("Select Crops", class = "custom-h1"),
                    tags$p(
                        "Choose which crops should be carried forward in the Long-Term Average Yield (LTAY) analysis. The goal of this exercise is to ensure that crop data is representative of each of the AFSC risk zones in the FDL cultivated rate calculation. We want to avoid crops with limited coverage, spatial or temporary, as they can skew the results. Ideally, we want to identify at least five representative crops for each AFSC risk zone. Avoid including crops with less than three years of data (if possible) or crops making up less than 1% of the risk area composition. These crops will have low statistical significance, and, in most cases, the limited data available on these less frequently grown (insured) crops are less reliable. However, regardless of the coverage and years of data, each area should be represented by at least five crop types.",
                        class = "clean-print"
                    ),
                    tags$p("Users should also decide whether Irr (Irrigated) crops will be included in the cultivated rate calculation. Irrigated areas make up only a small percentage of the FDL units, so it may be more practical for focus on the Agricultural Input Type: Dry and omit irrigated areas from the calculation.",
                        class = "clean-print"
                    ),
                    tags$p(
                        "Use the input values below to select a threshold for crop percent coverage and years of data. The crop selector table will be updated according to your preferences. Make additional manual edits (where needed) by selecting on a crop to include or exclude it from the analysis.",
                        class = "clean-print"
                    ),
                )
            ),
            fluidRow(
                column(
                    width = 6,
                    numericInput(ns("pct_coverage"), "Minimum Percent Coverage (%):", value = 1, step = 1, min = 0, max = 100, width = "100%"),
                ),
                column(
                    width = 6,
                    numericInput(ns("yrs_dat"), "Years of Data (n):", value = 3, step = 1, min = 0, max = 100, width = "100%"),
                )
            ),
            fluidRow(
                column(
                    width = 6,
                    numericInput(ns("start_year"), "Start Year:", value = NA, step = 1, min = 0, max = 3000, width = "100%"),
                ),
                column(
                    width = 6,
                    numericInput(ns("stop_year"), "End Year:", value = NA, step = 1, min = 0, max = 3000, width = "100%"),
                ),
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("AFSC Zone Summary", class = "custom-h2"),
                    tags$p("Summary of selected crops across AFSC Risk Zones for Agricultural Input Type: Dry."),
                    tableOutput(ns("afsc_summary")),
                    tags$p("Summary of selected crops across AFSC Risk Zones for Agricultural Input Type: Irr."),
                    tableOutput(ns("afsc_summary_irr"))
                ),
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h2("Crop Selection Table", class = "custom-h2"),
                    div("No data available. Please upload an Excel workbook first to populate this table.", id = "no_data_message"),
                    DT::dataTableOutput(ns("dt_select_crops_table"))
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
module_select_crops_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            print("loaded module_select_crops_server")
            ns <- session$ns

            # rV_select_crops_table_trigger
            session$userData$rV_select_crops_table_trigger <- reactiveVal(0)


            observeEvent(input$pct_coverage, {
                print("Updating pct_coverage...")
                session$userData$rv_crop_selection_inputs$pct_coverage <- input$pct_coverage
            })
            observeEvent(input$yrs_dat, {
                print("Updating yrs_dat...")
                session$userData$rv_crop_selection_inputs$yrs_dat <- input$yrs_dat
            })
            observeEvent(input$start_year, {
                print("Updating start_year...")
                session$userData$rv_crop_selection_inputs$start_year <- input$start_year
            })
            observeEvent(input$stop_year, {
                print("Updating stop_year...")
                session$userData$rv_crop_selection_inputs$stop_year <- input$stop_year
            })




            # Listen to Excel data update
            select_crops_table <- reactive({
                print("select_crops_table() reactive expression triggered...")

                # rm(list = ls()); library(shiny); runApp()

                # Toggle reactive val on user load
                session$userData$rV_select_crops_table_trigger()
                # Listen to change in object
                all_data <- session$userData$rv_excel_data$rv_excel_data

                # Wach for updates to inputs
                pct_coverage <- session$userData$rv_crop_selection_inputs$pct_coverage
                yrs_dat <- session$userData$rv_crop_selection_inputs$yrs_dat
                start_year <- session$userData$rv_crop_selection_inputs$start_year
                stop_year <- session$userData$rv_crop_selection_inputs$stop_year
                # browser()

                # Set to NULL to reinitate
                session$userData$rv_crop_selection$rv_crop_selection <- NULL

                if (is.null(all_data)) {
                    return(NULL)
                }

                cs <- suppressWarnings({
                    crop_selector(
                        excel_data = all_data,
                        start_year = start_year,
                        stop_year = stop_year,
                        target_years = yrs_dat,
                        target_coverage_pct = pct_coverage
                    )
                })

                # Set default crop selection
                session$userData$rv_crop_selection$rv_crop_selection <- cs

                return(cs)
            })



            # Rentder the data table
            output$dt_select_crops_table <- DT::renderDataTable({
                print("render DT style for select_crops...")
                # Get data from reactive object

                dt_data <- select_crops_table()

                # rm(list = ls()); library(shiny); runApp()

                # Check if data is populated and fix style
                if (!(is.null(dt_data))) {
                    # Set year to integer
                    # Set other columns to factor for multi-select
                    dt_data$`RISK AREA` <- as.factor(dt_data$`RISK AREA`)
                    dt_data$`CROP NAME` <- as.factor(dt_data$`CROP NAME`)
                    dt_data$`AG INPUT` <- as.factor(dt_data$`AG INPUT`)

                    dt_data$pct_coverage <- round(dt_data$pct_coverage, 1)
                    dt_data$crop_coverage <- round(dt_data$crop_coverage, 1)

                    dt_data$proposed <- NULL

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
                        colnames = c("RISK AREA", "CROP NAME", "AG INPUT", "N YEARS", "ACRES", "% COVERAGE", "SUGGESTED"),
                        selection = list(mode = "multiple", selected = which(dt_data$selected == 1), target = "row"),
                        options = list(
                            paging = TRUE,
                            pageLength = 50,
                            search = list(regex = TRUE, caseInsensitive = FALSE),
                            # columnDefs = list(visible = FALSE, targets = c(7)),
                            columnDefs = list(list(className = "dt-center", targets = "_all"))
                        )
                    ) %>% formatRound(columns = c("crop_coverage"), digits = 0)
                }
            })



            observeEvent(input$dt_select_crops_table_rows_selected, ignoreNULL = FALSE, {
                if (
                    !(is.null(input$dt_select_crops_table_rows_selected))) {
                    print("Row selection clicked... ")
                    row_ids <- input$dt_select_crops_table_rows_selected
                    session$userData$rv_crop_selection$rv_crop_selection$selected <- 0
                    session$userData$rv_crop_selection$rv_crop_selection$selected[row_ids] <- 1
                }
            })


            output$afsc_summary <- renderTable(
                {
                    session$userData$rv_crop_selection$rv_crop_selection$selected
                    cs <- session$userData$rv_crop_selection$rv_crop_selection

                    if (is.null(cs)) {
                        return(NULL)
                    } else {
                       if(nrow(cs) == 0) {
                            return(NULL)
                        }

                        tmp_sum <- cs %>%
                            filter(`AG INPUT` == "Dry") %>%
                            group_by(`RISK AREA`) %>%
                            summarise(n_crops = sum(selected, na.rm = TRUE))
                        tmp_sum$`RISK AREA` <- as.numeric(as.character(tmp_sum$`RISK AREA`))
                        tmp_sum <- tmp_sum[order(tmp_sum$`RISK AREA`), ]
                        tmp_sum$`RISK AREA` <- round(tmp_sum$`RISK AREA`, 0)
                        tmp_sum$n_crops <- round(tmp_sum$n_crops, 0)

                        tmp_sum <- as.data.frame(t(tmp_sum))
                        row.names(tmp_sum) <- c("RISK AREA", "Selected Crops (n)")
                        tmp_sum
                    }
                },
                rownames = TRUE,
                colnames = FALSE,
                striped = TRUE,
                hover = TRUE,
                digits = 0
            )



            output$afsc_summary_irr <- renderTable(
                {
                    session$userData$rv_crop_selection$rv_crop_selection$selected
                    cs <- session$userData$rv_crop_selection$rv_crop_selection

                    if (is.null(cs)) {
                        return(NULL)
                    } else {
                        if(nrow(cs) == 0) {
                            return(NULL)
                        }

                        tmp_sum <- cs %>%
                            filter(`AG INPUT` == "Irr") %>%
                            group_by(`RISK AREA`) %>%
                            summarise(n_crops = sum(selected, na.rm = TRUE))
                        tmp_sum$`RISK AREA` <- as.numeric(as.character(tmp_sum$`RISK AREA`))
                        tmp_sum <- tmp_sum[order(tmp_sum$`RISK AREA`), ]
                        tmp_sum$`RISK AREA` <- round(tmp_sum$`RISK AREA`, 0)
                        tmp_sum$n_crops <- round(tmp_sum$n_crops, 0)

                        tmp_sum <- as.data.frame(t(tmp_sum))
                        row.names(tmp_sum) <- c("RISK AREA", "Selected Crops (n)")
                        tmp_sum
                    }
                },
                rownames = TRUE,
                colnames = FALSE,
                striped = TRUE,
                hover = TRUE,
                digits = 0
            )







            # Other stuff...
        }
    )
}
