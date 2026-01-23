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

  # Remaining step modules
  module_step2_convert_server("step2_convert")
  module_step3_qaqc_server("step3_qaqc")
  module_step4_save_server("step4_save")
  module_step5_summarize_server("step5_summarize")

}
