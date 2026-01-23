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
        menuItem(
          "Step 1: Bind Logger Data",
          tabName = "t_step1_bind",
          icon = icon("link")
        ),
        menuItem(
          "Step 2: Convert Metrics",
          tabName = "t_step2_convert",
          icon = icon("exchange-alt")
        ),
        menuItem(
          "Step 3: QAQC",
          tabName = "t_step3_qaqc",
          icon = icon("check-circle")
        ),
        menuItem(
          "Step 4: Save Data",
          tabName = "t_step4_save",
          icon = icon("save")
        ),
        menuItem(
          "Step 5: Summarize",
          tabName = "t_step5_summarize",
          icon = icon("chart-bar")
        )
      )
    )
  )
}
