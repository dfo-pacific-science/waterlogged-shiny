# Excel Upload Module
# rm(list = ls()); library(shiny); runApp()
# Bad file: C:\Users\mattj\Dropbox\Projects\EN2739 CCIRA\ccira-watersheds\data
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_select_crops_ui <- function(id) {
    ns <- NS(id)

    tagList(

  a(href="demo_data.xlsx", "Download Sample Excel Template (.xlsx)", download=NA, target="_blank"),


        fileInput(ns("input_file"), "Upload Excel Workbook (.xlsx, .xls)"),
        textOutput(ns("output_message")),
        valueBoxOutput(ns("approvalBox"))
    )

}

# Define server module for Excel workbook upload
module_select_crops_server <- function(id) {
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
