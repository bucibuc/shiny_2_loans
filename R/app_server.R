#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues()
  observe({r$P <- input$loan_val})
  r1 <- reactiveValues()
  observe({r1$P <- input$loan_val})
  mod_loan_server("loan_1", init_dur = 360, r = r)
  mod_loan_server("loan_2", init_dur = 240, r = r1)
  mod_amort_plan_server("amort_plan_1", r = r)
  mod_amort_plan_server("amort_plan_2", r = r1)
  mod_int_prin_chart_server("int_prin_chart_1", r = r)
  mod_int_prin_chart_server("int_prin_chart_2", r = r1)
  mod_invest_server("invest_1", r = r, r1 = r1)
  mod_invest_table_server("invest_table_1", r = r, r1 = r1)
  mod_loan_invest_result_server("loan_invest_result_1", r = r, r1 = r1)
}
