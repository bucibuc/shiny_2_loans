#' amort_plan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_amort_plan_ui <- function(id){
  ns <- NS(id)
  tagList(

    DT::dataTableOutput(outputId = ns("amort_df"))

  )
}

#' amort_plan Server Functions
#'
#' @noRd
mod_amort_plan_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$amort_df <- DT::renderDataTable({

      DT::datatable(data = amort_plan(P = r$P,
                                      r = r$loan_data$r,
                                      n = r$loan_data$n,
                                      month_year = r$loan_data$month_year) %>%
                      dplyr::mutate(Month = as.integer(Month)),
                    rownames = FALSE) %>%
        DT::formatCurrency(., columns = c(2:6), currency = '')

    })

  })
}

## To be copied in the UI
# mod_amort_plan_ui("amort_plan_1")

## To be copied in the server
# mod_amort_plan_server("amort_plan_1")
