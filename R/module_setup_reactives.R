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
    metadata = NULL,
    wl_metadata = NULL,
    stage_readings = NULL,
    field_references = NULL,
    benchmarks = NULL
  )
  session$userData$rv_raw_data <- rv_raw_data

  # Populate with demo data from global.R so app works immediately
  rv_raw_data$metadata <- demo_metadata
  rv_raw_data$wl_metadata <- demo_metadata
  rv_raw_data$baro_data <- demo_baro_list
  rv_raw_data$waterlevel_data <- demo_waterlevel_list
  rv_raw_data$do_data <- demo_dissox_list
  rv_raw_data$conductivity_data <- demo_conductivity_list
  rv_raw_data$stage_readings <- demo_stage_readings
  rv_raw_data$field_references <- demo_field_references
  rv_raw_data$benchmarks <- demo_benchmarks

  # Bound data storage (Step 1 outputs)
  rv_bound_data <- reactiveValues(
    baro_bound = NULL,
    waterlevel_bound = NULL,
    waterlevel_with_baro = NULL,
    do_bound = NULL,
    do_with_baro = NULL,
    conductivity_bound = NULL
  )
  session$userData$rv_bound_data <- rv_bound_data

  # Converted data storage (Step 2 outputs)
  rv_converted_data <- reactiveValues(
    waterlevel_converted = NULL,
    do_converted = NULL
  )
  session$userData$rv_converted_data <- rv_converted_data

  # QAQC data storage (Step 2 baro + Step 3 outputs)
  rv_qaqc_data <- reactiveValues(
    baro_qaqc = NULL,
    qc_waterlevel = NULL,
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
