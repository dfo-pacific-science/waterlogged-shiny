#' Overview
module_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h1("Farm Development Leases (FDL) Toolbox", class = "custom-h1"),
      tags$h2("Scope", class = "custom-h2"),
      tags$p(
        "AEP manages 617 Farm Development Leases encompassing 97,443 acres of Crown Land The Farm Development Leases are renewed every 10 years and are charged a Yearly Cash Rental. The Yearly Cash Rental rates are reviewed and set every 5 years.
",
        class = "clean-print"
      ),

      tags$p(
        "Historically, rental rates have proved difficult to set based on available market data and the ability of each operational staff to gather accurate information that reflects the region. In 2017, research was completed to determine appropriate methods of determining rental rates. Of the methods analyzed, the New Cultivation Rent Formula (NCRF), implemented by the Government of Saskatchewan proved to be the most valuable accurate predictor of cultivated lease lands in Alberta.
",
        class = "clean-print"
      ),

      tags$p(
        "NCRF  uses yield data and price forecasts reported by crop insurance to generate a weighted average Gross Revenue per acre, per risk area. The formula factors in the top 5 insurable crops grown and indicates exact crop rotation and climate factors per region. Furthermore, rental rates are double discounted based on soil rating (LSRS) and climate/area factors which are represented in the five year yield average. The formula is summarized below:
",
        class = "clean-print"
      ),

      tags$div(
        class = "img-fit container",
        tags$img(
          src = "./img/ncrf.png",
          width = "100%",
          style = "max-width: 250px; margin: 15px"
        )
      ),

      tags$div(class = "clean-print",
        tags$ul(
          tags$li("Ci = All insured acres of commodity in risk zone"),
          tags$li("Pi = Commodity price per kg (forecast)"),
          tags$li(
            "Yi = Average annual yield (5-year moving window) per risk zone per commodity (kg/ac)"
          ),
          tags$li("A = Total acres in Risk Zone"),
          tags$li("S = Soil Coefficient (0.18 – 0.12)")
        )),

      tags$p(
        "Data from Agriculture Financial Services Corporation is used. This includes a 5-year average yield of insurable crops reported per risk area of most prominent crops grown. Price forecasts are used for the calculation which reflects commodity prices producers are able to insure crops at the designated grade. In most cases, the median grade determined by AFSC is used to accurately and fairly price each commodity. Premiums above the median grade may be perceived by the producer, but are not included in this calculation and rent is not calculated based on yielding the premium grade.",
        class = "clean-print"
      ),

      tags$h2("Getting Started: Data Retrieval", class = "custom-h2"),

      tags$h4("Updating FDL Input Workbook (Excel)"),

      tags$p(
        "After the Yield Report is published from the prior year (Example, Yield 2018 will be published early 2019), request the yield data per risk area from AFSC by emailing one of the contacts below. It is optimal to wait unitl the yield report is published because the data is then publicly available and the request should be well received. Request yield data and acres grown of each commodity in excel format for the prior year correlating to risk areas. Yields were initially provided per risk area in Kg/acre. Therefore it is best to request this unit of measurement annually to prevent conversions. Retrieve the current years price forecasts from the AFSC external website. https://afsc.ca/Default.aspx?cid=3817&lang=1. Some crops will not be eligible for the variable price benefits (Potatoes, etc.). Therefore, data may need to be retrieved from 2 separate documents. Save these documents in an FDL folder for the corresponding year.",
        class = "clean-print"
      ),

      tags$div(class = "clean-print",
               tags$ul(style = "list-style-type: none;",
                 tags$li("Donald Wildeboer"),
                 tags$li("Buisness Intellegence Analyst"),
                 tags$li("Donald.wildeboer@afsc.ca")
               )),


      tags$h4("Integrate New Year's Yield Data Into Historical Yields Raw Data"),

      tags$p(
        "The purpose of this step is to determine which top five crops will be brought forward to be used in calculating the FDL cultivated rate for each risk area. First upload the raw Excel input data (see Excel upload tab for formatting requirements). Review and modify the input data (as needed) in the Average Yield, Price Fractions, LSRS, and Parcel GIS tabs. Review the summary plots on each tab to help ensure there are no data entry errors.",
        class = "clean-print"
      ),

      tags$p(
        "Once satisfied with the raw input data, navigate to the Select Crops tab. Crop selection will be pre-populated for each risk zone based on percent coverage and years of data available. The goal is to have at least five representative crops for each of the AFSC risk zones. Default selection criteria highlights crops with at least three years of data and at least 1% of the risk area composition. Change these values as needed until satisfied that a representative selection of crops is achieved. The Crop Selection Table on the Selection Tab can also be used to manually include/exclude individual crops by clicking on each row. Start and end year inputs can be used to target a specific time period (if the raw input dataset has an extended time series beyond 5 years).",
        class = "clean-print"
      ),

      tags$h2("Review Outputs", class = "custom-h2"),

      tags$p(
        "After satisfied with crop selection navigate to the Long-Term Average Yield (LTAY) tab. Review the summary results by crop type and risk zone. Try to identify any issues or anomalies and (if present) correct the input data to recalculate the results. FDL crops are broken down by Agricultural Input Type (Dry or Irr - irrigated). It is very rare for FDL parcels to have irrigated crops making up a significant portion of their composition. In previous years, irrigated crops were excluded from the analysis or added on after the calculations. The FDL toolbox calculates rental rates for both the dry portion, irrigated portion, and the cumulative rental rate for each parcel. Irrigated portions are defined in the Parcel GIS tab and corresponding input data (Irr acres vs Dry acres per LSRS class).",
        class = "clean-print"
      ),

      tags$p(
        "Finally, the parcel rental rates can be reviewed on the Parcel Rates Report tab. This data is ultimately exported by the user to set the rental rates, but can be reviewed in the app with functionality to check for errors, issues and anomalies. Review the total value ($), value per acre ($/ac) and partitions to dry and irrigated portions. Once satisfied with the calculated rates navigate to the Excel Download tab to export an Excel Workbook of the analysis.",
        class = "clean-print"
      ),

      tags$h2("Export Data", class = "custom-h2"),

      tags$p(
        "The Excel Download tab allows users to export an Excel workbook of the input data, crop selection LTAY tables and parcel rate calculations. The exported data should be stored in an appropriate location in the FDL program folder. We recommend storing the input and output excel files together in the same directory. If rental rate calculations are disputed, analysis can be diagnosed and rerun with the same crop selection and filters. Example:",
        class = "clean-print"
      ),

      tags$div(class = "clean-print",
               tags$ul(style = "list-style-type: none;",
                       tags$li("FDL_Rental_Rates_Inputs_2021.xlsx"),
                       tags$li("FDL_Rental_Rates_Outputs_2021.xlsx")
               )),

      tags$p(
        "Although it is possible to edit the average yields, price fractions, LSRS and parcel GIS data within the app session, we recommend making any permanent changes to the original Excel input file (rather than in the app). Changes made in the app cause the results to be recalculated and updated. This is useful for exploratory and diagnostic purposes, but for program consistency in future years, it is better to update the input data (e.g., FLD parcel FDL123 non-cultivated area changes from 40 Ac to 60 Ac etc.). The Farm Development Leases (FDL) Toolbox simply acts as an online calculator. Changes to the data made in the app session affect only the outputs. Data is not saved anywhere online (outside of the app session). Therefore, when the web browser is closed any online edits will be lost unless the data is exported using the Excel Download tab.",
        class = "clean-print"
      ),


      tags$h2("Support", class = "custom-h2"),

      tags$p(
        "Review the support tab to help resolve common errors and issues. If problems persist please contact Matthew Bayly (M.J. Bayly Analytics Ltd.) for support.",
        class = "clean-print"
      ),

      tags$br(),


      tags$div(class = "clean-print",
               tags$ul(style = "list-style-type: none;",
                       tags$li("Prepared For:", style = "font-weight: bold;"),
                       tags$li("Natasha Rinas, Darrel Smith"),
                       tags$li("Alberta Forestry, Parks and Tourism"),
                       tags$li("Land Division"),
                       tags$li("Edmonton, Alberta T5K 2G8"),
                       tags$li("Canada")
               )),

      tags$div(class = "clean-print",
               tags$ul(style = "list-style-type: none;",
                       tags$li("Prepared By:", style = "font-weight: bold;"),
                       tags$li("Matthew Bayly, Quantitative Biologist"),
                       tags$li("M.J. Bayly Analytics Ltd."),
                       tags$li("210-1055 Millar Creek Road"),
                       tags$li("Whistler, British Columbia V8E 0K7"),
                       tags$li("Canada"),
                       tags$li("mjbayly@mjbayly.com")
               )),

      tags$br(),


      tags$div(class = "clean-print",
               tags$ul(style = "list-style-type: none;",
                       tags$li("Suggested Citation:", style = "font-weight: bold;"),
                       tags$li("Bayly, M. Rinas, N. Smith, D. (2023). FDLToolbox: Farm Development Leases (FDL) Toolbox. R-package and R-shiny applcation. Prepared by M.J. Bayly Analytics Ltd. for Government of Alberta. https://github.com/mattjbayly/FDLToolbox (private release: May 6, 2023).")
               )),

      tags$div(
        class = "img-fit container",
        tags$img(
          src = "./img/logo_small.svg",
          width = "80%",
          style = "max-width: 150px; margin: 15px"
        ),
        tags$img(
          src = "./img/goa_logo.svg",
          width = "80%",
          style = "max-width: 150px; margin: 15px"
        )
      ),



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
