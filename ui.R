# ui.R
# Waterlogged Shiny Application

# ------------------------------------------------
# Setup UI function for Shiny app
ui <- dashboardPage(
  title = "Waterlogged: Water Data Processing",
  skin = "blue",
  header = dashboardHeader(
    title = "Waterlogged",
    titleWidth = 250
  ),

  # Sidebar page menu
  sidebar = page_sidebar_tab_ui("sidebar_navigation"),

  # Main body panel
  body = dashboardBody(
    # Import custom css files to html head tag
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_style.css")
    ),

    # Import shinyjs
    shinyjs::useShinyjs(),

    # Individual pages - right side tab content
    tabItems(
      tabItem(
        "t_welcome",
        module_welcome_ui("welcome")
      ),
      tabItem(
        "t_upload_data",
        module_upload_data_ui("upload_data")
      ),
      # Step 1 subtabs
      tabItem(
        "t_step1_baro",
        module_step1_baro_ui("step1_baro")
      ),
      tabItem(
        "t_step1_wl",
        module_step1_wl_ui("step1_wl")
      ),
      tabItem(
        "t_step1_do",
        module_step1_do_ui("step1_do")
      ),
      tabItem(
        "t_step1_cond",
        module_step1_cond_ui("step1_cond")
      ),
      tabItem(
        "t_step1_overview",
        module_step1_overview_ui("step1_overview")
      ),
      # Remaining steps
      tabItem(
        "t_step2_convert",
        module_step2_convert_ui("step2_convert")
      ),
      tabItem(
        "t_step3_qaqc",
        module_step3_qaqc_ui("step3_qaqc")
      ),
      tabItem(
        "t_step4_save",
        module_step4_save_ui("step4_save")
      ),
      tabItem(
        "t_step5_summarize",
        module_step5_summarize_ui("step5_summarize")
      )
    )
  )
)
