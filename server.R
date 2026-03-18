# server.R
# Waterlogged Shiny Application

# ------------------------------------------------
# Setup server function for Shiny app
server <- function(input, output, session) {

  # Prepare reactive values for session and use in modules
  module_setup_reactives(session)

  # Welcome
  module_welcome_server("welcome")

  # Temperature
  module_temp_upload_server("temp_upload")
  module_temp_snap_server("temp_snap")
  module_temp_baro_server("temp_baro")
  module_temp_qc_air_server("temp_qc_air")
  module_temp_qc_water_server("temp_qc_water")

  # Barometric Pressure
  module_baro_upload_server("baro_upload")
  module_baro_qc_server("baro_qc")

  # Water Level (Pressure)
  module_wl_upload_server("wl_upload")
  module_wl_convert_server("wl_convert")
  module_wl_qaqc_server("wl_qaqc")

  # Dissolved Oxygen
  module_do_upload_server("do_upload")
  module_do_convert_server("do_convert")
  module_do_qc_server("do_qc")

  # Conductivity
  module_cond_upload_server("cond_upload")
  module_cond_convert_server("cond_convert")
  module_cond_qc_server("cond_qc")

}
