# Excel Upload Module
# rm(list = ls()); library(shiny); runApp()
# Bad file: C:\Users\mattj\Dropbox\Projects\EN2739 CCIRA\ccira-watersheds\data
# Good file: C:\Users\mattj\Dropbox\Projects\PN0042 FDL NR\06 Tool\FDLToolbox\inst\extdata\02_sample_data

# Define UI module for Excel workbook upload
module_excel_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Excel Upload", class = "custom-h1"),
      tags$h2("Input Raw Data", class = "custom-h2"),
      tags$p(
        "Upload your Excel Workbook of Average Yields, Price Fractions FDL area (GIS) data, and LSRS summaries here. Click the ‘Browse…’ Button to upload the raw Excel Workbook from your computer. If there are no errors in the formatting of each worksheet, a green box will appear below with the message ‘100% Upload Success’. Proceed to the next module when this message appears. However, if a red error message appears saying ‘Upload failed’ then check for formatting errors in Excel. Click the link below to download a sample Excel template (.xlsx) and use it to compare and contrast and help diagnose any potential formatting issues.",
        class = "clean-print"
      ),

      tags$p(
        class = "clean-print",
        a(
          href = "demo_data.xlsx",
          "Download Sample Excel Template (.xlsx)",
          download = NA,
          style = "color: 'blue';",
          target = "_blank",
          class = "clean-print"
        )
      ),

      tags$br(),

      fluidRow(column(
        width = 12,
        fileInput(
          ns("input_file"),
          "Upload Raw Excel Workbook Data (.xlsx, .xls)"
        ),
        textOutput(ns("output_message")),
        valueBoxOutput(ns("approvalBox"))
      )),

      tags$h2("Formatting the Input Workbook Data", class = "custom-h2"),

      tags$p(
        "The input Excel workbook must be formatted correctly. The raw input data must be uploaded as a .xlsx or .xls (Excel) file with the worksheets ‘Average Yields’, ‘Price Fractions’, ‘FDL GIS’ and ‘LSRS’. We suggest keeping separate Excel workbooks for each assessment year on your local file system (e.g., FDL_ Inputs_2019.xlsx, FDL_ Inputs_2020.xlsx, FDL_ Inputs_2021.xlsx…). Doing so will allow analyses from previous years to be rerun (if needed).",
        class = "clean-print"
      ),

      tags$p(
        class = "clean-print",
        a(
          href = "demo_data.xlsx",
          "Download Sample Excel Template (.xlsx)",
          style = "color: blue;",
          download = NA,
          target = "_blank",
          class = "clean-print"
        )
      ),


      tags$p(
        "See the notes below for the formatting of each worksheet (click on each tab).",
        class = "clean-print"
      ),



      tags$h3("Average Yields", class = "custom-h2"),



      fluidRow(column(
        width = 12,

        tagList(
          tags$p(
            "The average yield worksheet contains annual yield data for each crop type (e.g., Barley, Canola etc.) across each AFSC risk areas (e.g., zones 1, 2, 3) for each agricultural input type (e.g., Dry vs Irr). Yield data is reported as the total acres growing and the average crop yield (kg) per acre.",
            class = "clean-print"
          ),

          tags$p(
            "Average yield data is added annually from the Agriculture Financial Services Corporation (AFSC) https://afsc.ca/?s=yield reporting records. We suggest using the average yield worksheet from the previous year's assessment and adding new records cumulatively to the dataset. The goal is to set up the average yield worksheet such that each crop is represented by five years of data. Older records can either be removed manually in Excel or left in the dataset and filtered out automatically by setting start year and end year fields on the Crop Selector tab in this app.",
            class = "clean-print"
          ),

          tags$p(
            "The Average Yields worksheet must include the following columns:",
            class = "clean-print"
          ),

          tags$ul(
            class = "clean-print",
            tags$li(
              "YEAR - Add data for the new year following the same format as subsequent years. Please keep values from previous years in the spreadsheet for reference purposes. Source: https://afsc.ca/?s=yield"
            ),
            tags$li("RISK AREA: Crops Risk Areas from AFSC reporting"),
            tags$li(
              "CROP NAME: Crop name for FDL tool. Try to match the spelling of the crop name in previous years. Also try to match spelling in the crop price fraction worksheet. The FDL rental rate tool has embedded functionality to help ensure that names match."
            ),
            tags$li(
              "AG INPUT: Dryland or irrigated recorded as 'Dry' and 'Irr' respectively. Some crop types will require two rows of data for a given year."
            ),
            tags$li(
              "AVERAGE YIELD (kg/acre): Crop yield (production) per crop, per year, per risk area and Ag input. Units are kilograms per acre. AFSC reports may report yield as bu/ac (bushels per acre) if so, please adjust using crop-specific conversion ratios."
            ),
            tags$li(
              "ACRES: Crop coverage (in acres) for year, risk area and Ag input type."
            )
          ),
        )
      )),




      tags$h3("Price Fractions", class = "custom-h2"),

      fluidRow(column(
        width = 12,

          tags$p("The price fractions worksheet specifies the price ($/kg) per crop type. The critical columns within the Price Fractions worksheet include the YEAR, CROP NAME and PRICE ($/kg). Additional columns (AFSC Commodity Forecast (Reported As) and GRADE) are included in the worksheet for reference purposes only and are not used by the FDL calculator.",
            class = "clean-print"
          ),
        tags$p("The CROP NAME in the Price Fractions worksheet must match the spelling of the CROP NAME in the Average Yields worksheet. The Price Fractions module (tab) in the FDL calculator will specify any mismatches that may be present in the data. We recommended checking for mismatches in the app, correcting the data in Excel and then reuploading the corrected workbook. Changes made within the app session may make it harder to repeat the analysis in the future.",
               class = "clean-print"
        ),
        tags$p("It is possible to have multiple price records for each crop type across years. However, if multiple records exist, the FDL calculator will filter the most recent record for each crop.",
               class = "clean-print"
        ),

        tags$ul(
          class = "clean-print",
          tags$li("YEAR: Reference year for the price quoted from AFSC tables. If multiple records are included for a given crop type, the FDL calculator will filter the most recent year for each crop type."),
          tags$li("CROP NAME: Crop name for FDL tool. Try to match the spelling of the crop name in previous years. Also, try to check spelling in the Average Yields worksheet. The FDL rental rate tool has embedded functionality to help ensure that words match."),
          tags$li("AFSC Commodity Forecast (Reported As): Original unmodified crop name reported in AFSC reference documents. The column is used for reference purposes only and is not included in the FDL calculations."),
          tags$li("GRADE: The column is also used for internal reference purposes and is not incorporated into any FDL calculations."),
          tags$li("PRICE ($/kg): Market price reported in AFSC reference tables: https://afsc.ca/crop-insurance/insurance-prices/.")
        ),

        )),


      tags$h3("FDL GIS", class = "custom-h2"),

      fluidRow(column(
        width = 12,

        tags$p("The FDL GIS worksheet specifies the cultivated and non cultivated areas of each FDL unit (parcel). The values for this worksheet are obtained from GIS summaries and field surveys. To populate or update the FDL GIS worksheet start with the total acres for each parcel then subtract the non-cultivated acres to estimate the remaining cultivated acres. The cultivated acres are then subdivided into each of the LSRS classes for Dry and Irr Ag Input types. Therefore, if the LSRS classes are updated add or remove columns from this worksheet so that there are unique columns for each LSRS class for dry (e.g., LSRS 1 Dry Acres, LSRS 2 Dry Acres, LSRS 3 Dry Acres…) and (e.g., LSRS 1 Irr Acres, LSRS 2 Irr Acres, LSRS 3 Irr Acres…) input types. The sum or all the LSRS columns and the non-cultivated acres should roughly equal the total acres for each parcel.",
               class = "clean-print"
        ),
        tags$p("Values within the Irr columns will likely consist of all zeros with the exception of some special cases with FDL units are irrigated.",
               class = "clean-print"
        ),

        tags$ul(
          class = "clean-print",
          tags$li("FDL: FDL Farm development lease reference ID (there should be no duplicates in this column)"),
          tags$li("LSRS 1 Dry Acres, LSRS 2 Dry Acres, LSRS 3 Dry Acres: Area of parcel (in acres) in Land Suitability Rating System (LSRS) Class # and of Ag Type 'Dry' (unirrigated). (Note that the LSRS in a national scoring system in Alberta does not have any Class 1 land. If the LSRS 1 columns are populated with non-zero values consult with GIS technician to ensure that there were no errors in calculations)."),
          tags$li("LSRS 1 Irr Acres, LSRS 2 Irr Acres, LSRS 3 Irr Acres: Area of parcel (in acres) in Land Suitability Rating System (LSRS) Class 2 and of Ag Type 'Irr' (irrigated)."),
          tags$li("Not Cultivated Acres: Remaining area of parcel (in acres) as non-cultivated."),
          tags$li("Total Acres (GIS FDL Boundary): Total area from GIS polygon. Ensure area estimates are generated with appropriate spatial projection. Sum of other columns should equal the total area."),
          tags$li("AFSC Crop Risk Zone: Primary Risk Zone for FDL parcel. If the FDL parcels covers multiple risk zones that enter the value of the most dominant risk zone.")
        ),

      )),


      tags$h3("LSRS", class = "custom-h2"),

      fluidRow(column(
        width = 12,

        tags$p("The land suitability rating system (LSRS) is a procedure for rating the suitability of land for agricultural spring-seeded small grains and hardy oilseeds. The system is based on the soil–climate–landscape potential and is an improvement on the Canada Land Inventory capability rating that it replaces. See this guide online for more details: https://open.alberta.ca/publications/land-suitability-rating-system.",
               class = "clean-print"
        ),
        tags$p("It is unlikely that FDL toolbox users will need to change and update these values. However, a professional agrologist may suggest changing the suitability percentage rating for each LSRS class. Make these adjustments carefully, as changing the percentages (S) will affect the New Cultivation Rent Formula (NCRF).",
               class = "clean-print"
        ),

        tags$ul(
          class = "clean-print",
          tags$li("Reference ID: Reference ID to link to column names in the previous worksheet."),
          tags$li("Full Name: Full name for reporting (e.g., LSRS 1 (CLI 1))"),
          tags$li("Percentage (%): These values are constant across years. Consult Agrologist prior to changing. These values are referenced in the NCRF as the S (suitability) term."),
          tags$li("Limitation: See definitions from provincial reference.")
        ),

      )),




    ),

    fluidRow(column(
      width = 12,
      tags$p(
        "Farm Development Leases (FDL) Toolbox | contact mjbayly@mjbayly.com for more information",
        style = "text-align: center; color: #8A939A; margin-top: -15px; font-size: 12px;"
      )
    )),


  )
}

# Define server module for Excel workbook upload
module_excel_upload_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 print("loaded module_excel_upload_server")
                 ns <- session$ns

                 # Function to read and display uploaded Excel workbook
                 read_excel_file <- function(file_path) {
                   # Run checks to make sure file is ok
                   error_message <- "Upload and Excel File..."

                   extension <- basename(file_path)
                   extension <-
                     substr(extension, nchar(extension) - 3, nchar(extension))
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
                     show_toast(title = "Data Upload Error",
                                text = dat,
                                type = "error")
                     # Read and display uploaded Excel workbook
                     output$output_message <- renderText({
                       "Data formatting error in Excel file. Please fix and reupload."
                     })

                     output$approvalBox <- renderValueBox({
                       valueBox(
                         "0%",
                         "Upload Failed",
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
                     session$userData$rv_excel_data$rv_excel_data <-
                       dat


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
                     max_select <- isolate({
                       session$userData$rv_crop_selection_inputs$max_select
                     })


                     # Run the crop selection function
                     cs <- suppressWarnings({
                       crop_selector(
                         excel_data = dat,
                         start_year = start_year,
                         stop_year = stop_year,
                         target_years = yrs_dat,
                         target_coverage_pct = pct_coverage,
                         max_select = max_select
                       )
                     })

                     # Set default crop selection
                     session$userData$rv_crop_selection$rv_crop_selection <-
                       cs

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

                       parcel_data <-
                         parcel_data_gis(excel_data = dat,
                                         ltay_tables = ltay)

                       # Set default crop selection
                       session$userData$rv_parcel_data$rv_parcel_data$dat <-
                         parcel_data
                     }


                     show_toast(title = "Data Imported",
                                text = "Excel data imported successfully.",
                                type = "success")

                     output$approvalBox <- renderValueBox({
                       valueBox(
                         "100%",
                         "Upload Success",
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
               })
}
