#' invest_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_invest_table_ui <- function(id){
  ns <- NS(id)
  tagList(

    DT::dataTableOutput(ns('invest_table'))

  )
}

#' invest_table Server Functions
#'
#' @noRd
mod_invest_table_server <- function(id, r, r1){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    invest_data <- reactive({inv_table(val = r1$invest_val, r = r1$inv_ir, n = r1$loan_data$n, month_year = r1$loan_data$month_year)})

    output$invest_table <- DT::renderDataTable({

      DT::datatable(
        data = invest_data(),
        rownames = FALSE
      ) %>%
        DT::formatCurrency(., columns = c(2:3), currency = '')

    })

  })
}

## To be copied in the UI
# mod_invest_table_ui("invest_table_1")

## To be copied in the server
# mod_invest_table_server("invest_table_1")
