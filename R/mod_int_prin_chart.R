#' int_prin_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_int_prin_chart_ui <- function(id){
  ns <- NS(id)
  tagList(

    plotly::plotlyOutput(ns('chart'))

  )
}

#' int_prin_chart Server Functions
#'
#' @noRd
mod_int_prin_chart_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data <- reactive({amort_plan(P = r$P,
                                 r = r$loan_data$r,
                                 n = r$loan_data$n,
                                 month_year = r$loan_data$month_year) %>%
        dplyr::mutate(Month = as.integer(Month),
                      Interest_payment = abs(Interest_payment),
                      Principal_payment = abs(Principal_payment)) %>%
        dplyr::select(Month, Interest_payment, Principal_payment) %>%
        tidyr::pivot_longer(cols = -Month)})


    output$chart <- plotly::renderPlotly({

      plotly::plot_ly(data = data(), x = ~Month, y = ~value, color = ~name, type = "scatter")

    })


  })
}

## To be copied in the UI
# mod_int_prin_chart_ui("int_prin_chart_1")

## To be copied in the server
# mod_int_prin_chart_server("int_prin_chart_1")
