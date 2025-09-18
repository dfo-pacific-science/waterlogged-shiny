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

    # Crop selection
    rv_crop_selection <- reactiveValues(
        rv_crop_selection = NULL
    )
    session$userData$rv_crop_selection <- rv_crop_selection

    # Crop selection inputs
    rv_crop_selection_inputs <- reactiveValues(
        pct_coverage = 1,
        yrs_dat = 3,
        start_year = NULL,
        stop_year = NULL,
        max_select = NULL
    )
    session$userData$rv_crop_selection_inputs <- rv_crop_selection_inputs


    # LTAY tables
    rv_ltay <- reactiveValues(
        crop = NULL,
        zone = NULL
    )
    session$userData$rv_ltay <- rv_ltay


    # Parcel report
    rv_parcel_data <- reactiveValues(
        dat = NULL
    )
    session$userData$rv_parcel_data <- rv_parcel_data
}
