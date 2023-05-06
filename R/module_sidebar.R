#' Sidear navigation tab
page_sidebar_tab_ui <- function(id) {
    ns <- NS(id)

    sidebar <- dashboardSidebar(
        sidebarMenu(

            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            sidebarMenu(
                menuItem("Overview", tabName = "t_overview"),
                menuItem("Excel Upload", tabName = "t_excel_upload", icon = icon("upload")),
                menuItem(
                    "Modify Raw Data",
                    tabName = "t_modify_inputs",
                    startExpanded = TRUE,
                    menuSubItem(
                        "Average Yields",
                        tabName = "t_average_yields",
                        icon = icon("seedling")
                    ),
                    menuSubItem(
                        "Price Fractions",
                        tabName = "t_price_fractions",
                        icon = icon("dollar-sign")
                    ),
                    menuSubItem("LSRS (%)", tabName = "t_lsrs", icon = icon("droplet")),
                    menuSubItem("Parcel GIS", tabName = "t_gis", icon = icon("map"))
                ),
                menuItem(
                    "FDL Calculations",
                    tabName = "t_fdl_calculations",
                    startExpanded = TRUE,
                    menuSubItem(
                        "Select Crops",
                        tabName = "t_select_crops",
                        icon = icon("check")
                    ),
                    menuSubItem("Long Term Average Yield", tabName = "t_ltay", icon = icon("table-list")),
                    menuSubItem(
                        "Parcel Rates Report",
                        tabName = "t_parcel_rates",
                        icon = icon("table")
                    )
                ),
                menuItem(
                    "Excel Download",
                    tabName = "t_excel_download",
                    icon = icon("download")
                )
            )
        )



    )
}
