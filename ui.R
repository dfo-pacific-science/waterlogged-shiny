# ui.R

# ------------------------------------------------
# Setup UI function for Shiny app
ui <- dashboardPage(
  title = "Farm Development Leases (FDL) Toolbox",
  skin = "blue",
  header = dashboardHeader(
    title = "Farm Development Leases (FDL) Toolbox",
    titleWidth = 450
  ),

  # Sidebar page menu
  sidebar = page_sidebar_tab_ui("sidebar_navigation"),
  # Main body panel
  body = dashboardBody( # Boxes need to be put in a row (or column)

    # Import custom css files to html head tag
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_style.css")
    ),

    # Import custom js files - note these are used with DT
    #shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),

    # Individual pages - right side tab content
    tabItems(
      tabItem(
        "t_overview",
        module_overview_ui("overview")
      ),
      tabItem(
        "t_excel_upload",
        module_excel_upload_ui("excel_upload")
      ),
      tabItem(
        "t_average_yields",
        module_average_yields_ui("average_yields")
      ),
      tabItem(
        "t_price_fractions",
        module_price_fractions_ui("price_fractions")
      ),
      tabItem(
        "t_lsrs",
        module_lsrs_ui("lsrs")
      ),
      tabItem(
        "t_gis",
        module_parcel_gis_ui("parcel_gis")
      ),
      tabItem(
        "t_select_crops",
        module_select_crops_ui("select_crops")
      ),
      tabItem(
        "t_ltay",
        module_ltay_ui("ltay")
      ),
      tabItem(
        "t_parcel_rates",
        module_parcel_report_ui("parcel_report")
      ),
      tabItem(
        "t_excel_download",
        module_download_excel_ui("download_excel")
      )
    )
  )
)
