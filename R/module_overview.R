#' Overview
module_overview_ui <- function(id) {
    ns <- NS(id)

    tagList(
        shinydashboard::box(
            width = 12,
            tags$h1("Model Overview and Updates 1", class = "custom-h1"),
            tags$h2("Model Overview and Updates 2", class = "custom-h2"),
            tags$div(
                class = "img-fit container",
                tags$img(src = "./img/logo_small.svg", width = "80%", style = "max-width: 150px; margin: 15px"),
                tags$img(src = "./img/goa_logo.svg", width = "80%", style = "max-width: 150px; margin: 15px")
            ),
            tags$p(
                "The Westslope Cutthroat Trout (WCT) population model is an interactive tool that can be used to evaluate the impacts of changes in the environment might have on WCT population. This memo intends to help users understand the specific details of the WCT population model by providing the processing details, sometimes known as pseudo-code, of the detailed steps required to go from inputs (e.g., parameter values) to outputs (e.g., estimates of population abundance).",
                class = "clean-print"
            ),
            fluidRow(
                column(
                    width = 12,
                    tabBox(
                        width = 12,
                        title = "First tabBox",
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Tab1", "First tab content"),
                        tabPanel("Tab2", "Tab content 2")
                    )
                )
            )
        ),
        fluidRow(
            column(
                width = 12,
                tags$p("Farm Development Leases (FDL) Toolbox | contact mjbayly@mjbayly.com for more information", style = "text-align: center; color: #8A939A; margin-top: -15px; font-size: 12px;")
            )
        ),
    )
}
