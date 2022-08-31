#' loan_invest_result UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loan_invest_result_ui <- function(id){
  ns <- NS(id)
  tagList(

    DT::dataTableOutput(ns('summary'))

  )
}

#' loan_invest_result Server Functions
#'
#' @noRd
mod_loan_invest_result_server <- function(id, r, r1){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    loan_data <- reactive({

      long_loan <- amort_plan(P = r$P,
                              r = r$loan_data$r,
                              n = r$loan_data$n,
                              month_year = r$loan_data$month_year)

      short_loan <- amort_plan(P = r1$P,
                               r = r1$loan_data$r,
                               n = r1$loan_data$n,
                               month_year = r1$loan_data$month_year)

      long_loan_int <- long_loan %>%
        dplyr::pull(Interest_payment) %>%
        sum(.) %>%
        abs(.)

      remain_prin <- long_loan %>%
        dplyr::filter(Month >= max(short_loan$Month)) %>%
        dplyr::pull(Principal_payment) %>%
        sum(.) %>%
        abs(.)

      short_loan_int <- short_loan%>%
        dplyr::pull(Interest_payment) %>%
        sum(.) %>%
        abs(.)

      invest <- inv_table(val = r1$invest_val, r = r1$inv_ir, n = r1$loan_data$n, month_year = r1$loan_data$month_year) %>%
        dplyr::filter(Month == 'Total') %>%
        dplyr::pull(Return)

      x <- data.frame("Variable" = c("Longer loan interest payment",
                                     "Shorter loan interest payment",
                                     "Return from investment",
                                     "Remaining principal from longer loan"),
                      "Value" = c(long_loan_int,
                                  short_loan_int,
                                  invest,
                                  remain_prin),
                      stringsAsFactors = FALSE)

      return(x)

    })

    output$summary <- DT::renderDataTable({
      DT::datatable(
        data = loan_data()
      ) %>%
        DT::formatCurrency(., columns = c(2), currency = '')
    })

  })
}

## To be copied in the UI
# mod_loan_invest_result_ui("loan_invest_result_1")

## To be copied in the server
# mod_loan_invest_result_server("loan_invest_result_1")
