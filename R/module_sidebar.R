#' Sidebar navigation tab for Waterlogged app
page_sidebar_tab_ui <- function(id) {
  ns <- NS(id)

  sidebar <- dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      sidebarMenu(
        menuItem("Welcome", tabName = "t_welcome", icon = icon("home")),
        menuItem(
          "Upload Data",
          tabName = "t_upload_data",
          icon = icon("upload")
        ),

        # Expandable Step 1 menu with subtabs
        menuItem(
          "Step 1: Raw Series",
          icon = icon("chart-line"),
          menuSubItem("Barometric", tabName = "t_step1_baro"),
          menuSubItem("Water Level", tabName = "t_step1_wl"),
          menuSubItem("Dissolved Oxygen", tabName = "t_step1_do"),
          menuSubItem("Conductivity", tabName = "t_step1_cond"),
          menuSubItem("Overview", tabName = "t_step1_overview")
        ),

        menuItem(
          "Step 2: Baro QAQC",
          tabName = "t_step2_baro_qaqc",
          icon = icon("filter")
        ),

        # Expandable Step 3 menu with subtabs
        menuItem(
          "Step 3: Water Level",
          icon = icon("water"),
          menuSubItem("Join Baro Data", tabName = "t_step3_wl_baro"),
          menuSubItem("Convert to Metres", tabName = "t_step3_wl_convert"),
          menuSubItem("Water Level QAQC", tabName = "t_step3_wl_qaqc")
        ),

        # Expandable Step 4 menu with subtabs
        menuItem(
          "Step 4: Dissolved Oxygen",
          icon = icon("lungs"),
          menuSubItem("Join Baro Data", tabName = "t_step4_do_baro"),
          menuSubItem("Convert to %Sat", tabName = "t_step4_do_convert"),
          menuSubItem("DO QAQC", tabName = "t_step4_do_qaqc"),
          menuSubItem("Drift Correction", tabName = "t_step4_do_drift")
        ),

        # Expandable Step 5 menu with subtabs
        menuItem(
          "Step 5: Conductivity",
          icon = icon("bolt"),
          menuSubItem("Conductivity QAQC", tabName = "t_step5_cond_qaqc"),
          menuSubItem("Drift Correction", tabName = "t_step5_cond_drift"),
          menuSubItem("Temp Compensation", tabName = "t_step5_cond_tempcomp")
        ),

        menuItem(
          "Step 6: Save Data",
          tabName = "t_step6_save",
          icon = icon("save")
        ),
        menuItem(
          "Step 7: Summarize",
          tabName = "t_step7_summarize",
          icon = icon("chart-bar")
        )
      )
    )
  )
}
