#' loan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loan_ui <- function(id, init_ir = 0.01){
  ns <- NS(id)
  tagList(

    shinyWidgets::autonumericInput(inputId = ns("ir"),
                                   label = "Input annual interest rate:",
                                   value = init_ir,
                                   currencySymbol = "%",
                                   currencySymbolPlacement = "s",
                                   minimumValue = 0.01),

    shiny::radioButtons(inputId = ns("month_year"),
                        label = 'Would you like to input the number of months or years:',
                        choices = c('Months', 'Years'),
                        selected = 'Months'),

    shiny::uiOutput(outputId = ns("duration")),

    shiny::uiOutput(outputId = ns("pay_text"))

  )
}

#' loan Server Functions
#'
#' @noRd
mod_loan_server <- function(id, r, init_dur, P){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$duration <- shiny::renderUI({

      shiny::numericInput(inputId = ns("final_mat"),
                          label = 'Set the duration of loan',
                          value = init_dur,
                          min = 0,
                          max = dplyr::case_when(input$month_year == 'Months' ~ 30*12,
                                                 input$month_year == 'Years' ~ 30))

    })

    output$pay_text <- shiny::renderUI(paste("This results in",
                                             loan_payment(P = r$P, r = input$ir, n = input$final_mat, month_year = input$month_year) %>%
                                               format(., digits = 2, big.mark = ",", decimal.mark = "."),
                                             "of monthy payments."))

    shiny::observeEvent(input$loan_val | input$ir | input$final_mat, {
      r$loan_data <- data.frame(r = input$ir,
                                n = input$final_mat,
                                month_year = input$month_year,
                                stringsAsFactors = FALSE)
    }, ignoreInit = TRUE)

  })
}

## To be copied in the UI
# mod_loan_ui("loan_1")

## To be copied in the server
# mod_loan_server("loan_1")
