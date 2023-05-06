# Download Excel

# Define UI module for Excel workbook upload
module_download_excel_ui <- function(id) {
  ns <- NS(id)

  tagList(shinydashboard::box(width = 12,
                              fluidRow(column(
                                width = 12,
                                tags$h1("Download Excel", class = "custom-h1"),
                                tags$p("Click the download button after you have worked through all previous tabs. Clicking download will export an Excel workbook with worksheets for each component including the raw input worksheets, crop selection, the LTAY summary tables and the FDL parcel rental summaries."),
                              )),
                              fluidRow(column(
                                width = 4,
                                downloadButton(ns("dl"), "Download Excel Data"),
                              ), ), ),

          # Below box
          fluidRow(column(
            width = 12,
            tags$p(
              "Farm Development Leases (FDL) Toolbox | contact mjbayly@mjbayly.com for more information",
              style = "text-align: center; color: #8A939A; margin-top: -15px; font-size: 12px;"
            )
          )),
  )
}

# Define server module
module_download_excel_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 print("loaded module_download_excel_server")
                 ns <- session$ns

                 # Exort excel data
                 output$dl <- downloadHandler(
                   filename = function() {
                     paste("FDL_Rental_Rates_Outputs_",
                           format(Sys.Date(), "%Y"),
                           ".xlsx",
                           sep = "")
                   },
                   content = function(file) {
                     print("Exort excel data....")

                     excel_in <-
                       session$userData$rv_excel_data$rv_excel_data

                     if (is.null(excel_in)) {

                       write_xlsx(data.frame(), path = file)

                     } else {

                       print("Export data...")

                       # Rerun analysis


                       exp_inputs <-
                         session$userData$rv_excel_data$rv_excel_data

                       all_out <- list()

                       all_out[["Average Yields"]]   <- exp_inputs[["Average Yields"]]
                       all_out[["Price Fractions"]]  <- exp_inputs[["Price Fractions"]]
                       all_out[["FDL GIS"]]          <- exp_inputs[["FDL GIS"]]
                       all_out[["LSRS"]]             <- exp_inputs[["LSRS"]]

                       cs <- session$userData$rv_crop_selection$rv_crop_selection
                       cs$proposed <- NULL
                       all_out[["Crops"]]             <- cs

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

                       print("running parcel_data_gis...")
                       # rm(list = ls()); library(shiny); runApp()

                       parcel_data <- parcel_data_gis(
                         excel_data = all_data,
                         ltay_tables = ltay
                       )


                       all_out[["LTAY Crop"]]             <- ltay$ltay
                       all_out[["LTAY Zone"]]             <- ltay$risk_area_lsrs

                       # parcel
                       all_out[["FDL Summary"]]             <- parcel_data
                       write_xlsx(all_out, path = file)

                     }

                   }
                 )



                 # Other stuff...
               })
}
