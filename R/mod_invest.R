#' invest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_invest_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::uiOutput(outputId = ns('ui_inv')),
    shiny::uiOutput(outputId = ns('text')),
    shinyWidgets::autonumericInput(inputId = ns("inv_ir"),
                                   label = "Input annual interest rate:",
                                   value = 2,
                                   currencySymbol = "%",
                                   currencySymbolPlacement = "s",
                                   minimumValue = 0.01)

  )
}

#' invest Server Functions
#'
#' @noRd
mod_invest_server <- function(id, r, r1){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ui_inv <- shiny::renderUI(shinyWidgets::autonumericInput(inputId = ns("add_invest"),
                                                                    label = "Would you like to invest additional funds?",
                                                                    value = 0,
                                                                    currencySymbol = "",
                                                                    currencySymbolPlacement = "s",
                                                                    minimumValue = 0))

    output$text <- shiny::renderUI(paste("This results in",
                                         (abs(loan_payment(P = r$P, r = r$loan_data$r, n = r$loan_data$n, month_year = r$loan_data$month_year) -
                                                loan_payment(P = r1$P, r = r1$loan_data$r, n = r1$loan_data$n, month_year = r1$loan_data$month_year)) +
                                            input$add_invest) %>%
                                           format(., digits = 2, big.mark = ",", decimal.mark = "."),
                                         "of monthy payments."))

    amount <- reactive({
      (abs(loan_payment(P = r$P, r = r$loan_data$r, n = r$loan_data$n, month_year = r$loan_data$month_year) -
             loan_payment(P = r1$P, r = r1$loan_data$r, n = r1$loan_data$n, month_year = r1$loan_data$month_year)) +
         input$add_invest)
    })

    shiny::observeEvent(r$P | r$loan_data$r | r$loan_data$n |
                          r1$loan_data$r | r1$loan_data$n | input$add_invest | input$inv_ir,
                        {r$invest_val <- amount()
                        r$inv_ir <- input$inv_ir},
                        ignoreInit = TRUE)

    shiny::observeEvent(r1$P | r$loan_data$r | r$loan_data$n |
                          r1$loan_data$r | r1$loan_data$n | input$add_invest | input$inv_ir,
                        {r1$invest_val <- amount()
                        r1$inv_ir <- input$inv_ir},
                        ignoreInit = TRUE)



  })
}

## To be copied in the UI
# mod_invest_ui("invest_1")

## To be copied in the server
# mod_invest_server("invest_1")
