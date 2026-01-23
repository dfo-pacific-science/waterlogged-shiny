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
  module_step1_bind_server("step1_bind")
  module_step2_convert_server("step2_convert")
  module_step3_qaqc_server("step3_qaqc")
  module_step4_save_server("step4_save")
  module_step5_summarize_server("step5_summarize")

}
