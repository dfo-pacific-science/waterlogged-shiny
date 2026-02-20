# server.R
# Waterlogged Shiny Application

# ------------------------------------------------
# Setup server function for Shiny app
server <- function(input, output, session) {

  # Prepare reactive values for session and use in modules
  module_setup_reactives(session)

  # Load modules

  module_welcome_server("welcome")
  module_upload_data_server("upload_data")

  # Step 1 subtab modules
  module_step1_baro_server("step1_baro")
  module_step1_wl_server("step1_wl")
  module_step1_do_server("step1_do")
  module_step1_cond_server("step1_cond")
  module_step1_overview_server("step1_overview")

  # Step 2: Barometric QAQC
  module_step2_baro_qaqc_server("step2_baro_qaqc")

  # Step 3: Water Level subtab modules
  module_step3_wl_baro_server("step3_wl_baro")
  module_step3_wl_convert_server("step3_wl_convert")
  module_step3_wl_qaqc_server("step3_wl_qaqc")

  # Step 4: Dissolved Oxygen subtab modules
  module_step4_do_baro_server("step4_do_baro")
  module_step4_do_convert_server("step4_do_convert")
  module_step4_do_qaqc_server("step4_do_qaqc")
  module_step4_do_drift_server("step4_do_drift")

  # Step 5: Conductivity subtab modules
  module_step5_cond_qaqc_server("step5_cond_qaqc")
  module_step5_cond_drift_server("step5_cond_drift")
  module_step5_cond_tempcomp_server("step5_cond_tempcomp")

  # Remaining step modules
  module_step4_save_server("step4_save")
  module_step5_summarize_server("step5_summarize")

}
