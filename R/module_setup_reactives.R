#' Setup Reactive Values for Waterlogged Session
#'
#' Initialize reactive values for the user session.
#' These values are shared across modules via session$userData.
#'
module_setup_reactives <- function(session = NA) {
  print("Setting up reactive values for user session...")

  # Initial load flag
  session$userData$rv_initial_load <- reactiveVal(NULL)

  # Raw data storage
  rv_raw_data <- reactiveValues(
    baro_data = NULL,
    waterlevel_data = NULL,
    do_data = NULL,
    conductivity_data = NULL,
    metadata = NULL
  )
  session$userData$rv_raw_data <- rv_raw_data

  # Bound data storage (Step 1 outputs)
  rv_bound_data <- reactiveValues(
    baro_bound = NULL,
    waterlevel_bound = NULL,
    do_bound = NULL,
    conductivity_bound = NULL
  )
  session$userData$rv_bound_data <- rv_bound_data

  # Converted data storage (Step 2 outputs)
  rv_converted_data <- reactiveValues(
    waterlevel_converted = NULL,
    do_converted = NULL
  )
  session$userData$rv_converted_data <- rv_converted_data

  # QAQC data storage (Step 3 outputs)
  rv_qaqc_data <- reactiveValues(
    waterlevel_qaqc = NULL,
    do_qaqc = NULL,
    qaqc_flags = NULL
  )
  session$userData$rv_qaqc_data <- rv_qaqc_data

  # Clean data storage (Step 4 outputs)
  rv_clean_data <- reactiveValues(
    waterlevel_clean = NULL,
    do_clean = NULL
  )
  session$userData$rv_clean_data <- rv_clean_data

  # Summary data storage (Step 5 outputs)
  rv_summary_data <- reactiveValues(
    daily_summary = NULL,
    monthly_summary = NULL,
    annual_summary = NULL
  )
  session$userData$rv_summary_data <- rv_summary_data

  # Current station/file selection
  rv_selection <- reactiveValues(
    current_station = NULL,
    current_logger_type = NULL,
    selected_files = NULL
  )
  session$userData$rv_selection <- rv_selection
}
