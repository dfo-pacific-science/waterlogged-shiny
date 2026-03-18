#' Sidebar navigation for Waterlogged app
#'
#' Organized by logger type / metric workflow rather than step numbers.
page_sidebar_tab_ui <- function(id) {
  ns <- NS(id)

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",

      menuItem("Welcome", tabName = "t_welcome", icon = icon("home")),

      # ---- Temperature (TidbiT / air / water temp channels) ----
      menuItem(
        "Temperature",
        icon = icon("thermometer-half"),
        menuSubItem("Import Data",          tabName = "t_temp_upload"),
        menuSubItem("Snap Timestamps",      tabName = "t_temp_snap"),
        menuSubItem("Add Nearest Baro",     tabName = "t_temp_baro"),
        menuSubItem("QC Air Temperature",   tabName = "t_temp_qc_air"),
        menuSubItem("QC Water Temperature", tabName = "t_temp_qc_water")
      ),

      # ---- Barometric Pressure ----
      menuItem(
        "Barometric Pressure",
        icon = icon("cloud"),
        menuSubItem("Import Data",    tabName = "t_baro_upload"),
        menuSubItem("QC Barometric",  tabName = "t_baro_qc")
      ),

      # ---- Water Level (Pressure) ----
      menuItem(
        "Water Level (Pressure)",
        icon = icon("water"),
        menuSubItem("Import Data",          tabName = "t_wl_upload"),
        menuSubItem("Convert to Meters", tabName = "t_wl_convert"),
        menuSubItem("QC Water Level",       tabName = "t_wl_qaqc")
      ),

      # ---- Dissolved Oxygen ----
      menuItem(
        "Dissolved Oxygen",
        icon = icon("lungs"),
        menuSubItem("Import Data",            tabName = "t_do_upload"),
        menuSubItem("Convert to % Sat.",    tabName = "t_do_convert"),
        menuSubItem("QC Dissolved Oxygen",  tabName = "t_do_qc")
      ),

      # ---- Conductivity ----
      menuItem(
        "Conductivity",
        icon = icon("bolt"),
        menuSubItem("Import Data",  tabName = "t_cond_upload"),
        menuSubItem("Convert to Spec. Cond.", tabName = "t_cond_convert"),
        menuSubItem("QC Conductivity", tabName = "t_cond_qc")
      )
    )
  )
}
