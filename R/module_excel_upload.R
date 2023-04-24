# Excel Upload Module
# rm(list = ls()); library(shiny); runApp()
# Bad file: C:\Users\mattj\Dropbox\Projects\EN2739 CCIRA\ccira-watersheds\data
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_excel_upload_ui <- function(id) {
    ns <- NS(id)

    tagList(
        a(href = "demo_data.xlsx", "Download Sample Excel Template (.xlsx)", download = NA, target = "_blank"),
        fileInput(ns("input_file"), "Upload Excel Workbook (.xlsx, .xls)"),
        textOutput(ns("output_message")),
        valueBoxOutput(ns("approvalBox"))
    )
}

# Define server module for Excel workbook upload
module_excel_upload_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            print("loaded module_excel_upload_server")
            ns <- session$ns

            # Function to read and display uploaded Excel workbook
            read_excel_file <- function(file_path) {
                # Run checks to make sure file is ok
                error_message <- "Upload and Excel File..."

                extension <- basename(file_path)
                extension <- substr(extension, nchar(extension) - 3, nchar(extension))
                extension <- gsub("\\.", "", (extension))

                if (!(grepl("xlsx|xls", extension))) {
                    msg <- "Please upload a valid Excel Workbook (.xlsx, .xls) file."
                    print(msg)
                    return(msg)
                }

                # Upload excel file with custom function
                dat <- suppressWarnings({
                    load_data_from_excel(file_path)
                    # dat <- load_data_from_excel(file_path)
                })

                if (class(dat)[1] == "character") {
                    print(dat)
                    return(dat)
                }

                return(dat)
            }


            # Observe changes in uploaded file input
            observeEvent(input$input_file, {
                file_name <- input$input_file$name
                file_path <- input$input_file$datapath

                # Load FDL excel workbook
                dat <- read_excel_file(file_path)

                if (class(dat)[1] == "character") {
                    show_toast(
                        title = "Data Upload Error",
                        text = dat,
                        type = "error"
                    )
                    # Read and display uploaded Excel workbook
                    output$output_message <- renderText({
                        "Data formatting error in Excel file. Please fix and reupload."
                    })

                    output$approvalBox <- renderValueBox({
                        valueBox(
                            "0%", "Upload Failed",
                            icon = icon("thumbs-down", lib = "glyphicon"),
                            color = "red"
                        )
                    })
                }

                if (class(dat)[1] == "list") {

                    # If no error messages update reactive values
                    print("set rv_initial_load to TRUE")
                    session$userData$rv_initial_load(1)
                    print("update rv_excel_data")
                    session$userData$rv_excel_data$rv_excel_data <- dat


                    pct_coverage <- isolate({
                        session$userData$rv_crop_selection_inputs$pct_coverage
                    })
                    yrs_dat <- isolate({
                        session$userData$rv_crop_selection_inputs$yrs_dat
                    })
                    start_year <- isolate({
                        session$userData$rv_crop_selection_inputs$start_year
                    })
                    stop_year <- isolate({
                        session$userData$rv_crop_selection_inputs$stop_year
                    })


                    # Run the crop selection function
                    cs <- suppressWarnings({
                        crop_selector(
                            excel_data = dat,
                            start_year = start_year,
                            stop_year = stop_year,
                            target_years = yrs_dat,
                            target_coverage_pct = pct_coverage
                        )
                    })

                    # Set default crop selection
                    session$userData$rv_crop_selection$rv_crop_selection <- cs

                    # Run LTAY calculation
                    if (!(all(cs$selected) == 0)) {
                        ltay <- suppressWarnings({
                            build_ltay_tables(
                                excel_data = dat,
                                crops_selected = cs,
                                start_year = start_year,
                                stop_year = stop_year
                            )
                        })
                        # Set default crop selection
                        session$userData$rv_ltay$rv_ltay <- ltay

                        parcel_data <- parcel_data_gis(
                            excel_data = dat,
                            ltay_tables = ltay
                        )

                        # Set default crop selection
                        session$userData$rv_parcel_data$rv_parcel_data$dat <- parcel_data
                    }


                    show_toast(
                        title = "Data Imported",
                        text = "Excel data imported successfully.",
                        type = "success"
                    )

                    output$approvalBox <- renderValueBox({
                        valueBox(
                            "100%", "Upload Success",
                            icon = icon("thumbs-up", lib = "glyphicon"),
                            color = "green"
                        )
                    })

                    # Read and display uploaded Excel workbook
                    output$output_message <- renderText({
                        "Excel data imported successfully."
                    })
                }
            })


            # Other stuff...
        }
    )
}
