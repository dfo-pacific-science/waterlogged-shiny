# ------------------------------------------------
# Setup server function for Shiny app
server <- function(input, output, session) {
  # Prepare reactive values for session and use in modules
  module_setup_reactives(session)



  # Load moules
  module_excel_upload_server("excel_upload")
  module_average_yields_server("average_yields")
  module_price_fractions_server("price_fractions")
  module_lsrs_server("lsrs")
  module_parcel_gis_server("parcel_gis")
  module_select_crops_server("select_crops")
  module_ltay_server("ltay")
  module_parcel_report_server("parcel_report")
  module_download_excel_server("download_excel")


}
