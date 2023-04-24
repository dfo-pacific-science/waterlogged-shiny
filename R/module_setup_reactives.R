module_setup_reactives <- function(session = NA) {

    print("Setting up reactive values for user session...")

    # Initial load flag
    session$userData$rv_initial_load <- reactiveVal(NULL)

    # Excel data object
    rv_excel_data <- reactiveValues(
        rv_excel_data = NULL
    )
    # Add to userData object for session
    session$userData$rv_excel_data <- rv_excel_data

    # Bad crop names
    rv_crop_names <- reactiveValues(
        rv_only_in_price_fraction = NULL,
        rv_only_in_avg_yeild = NULL,
        rv_total_missing_area = NULL
    )
    session$userData$rv_crop_names <- rv_crop_names



}
