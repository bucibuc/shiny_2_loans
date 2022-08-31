#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h1("2 loans example"),
          tabsetPanel(type = 'tabs',
                      tabPanel('Loans',
                               shinyWidgets::autonumericInput(inputId = "loan_val",
                                                              label = "Amount of desired loan.",
                                                              value = 300000,
                                                              minimumValue = 1),
                               h3("Set the parameters of the longer loan."),
                               mod_loan_ui("loan_1", init_ir = 2.7),
                               h3("Set the parameters of the shorter loan."),
                               mod_loan_ui("loan_2", init_ir = 2.45)),
                      tabPanel('Investing',
                               mod_invest_ui("invest_1")))
        ),
        mainPanel(
          tabsetPanel(
            type = 'tabs',
            tabPanel("Amortization table",
                     mod_amort_plan_ui("amort_plan_1"),
                     mod_amort_plan_ui("amort_plan_2")),
            tabPanel("Interest and Principal payments chart",
                     mod_int_prin_chart_ui("int_prin_chart_1"),
                     mod_int_prin_chart_ui("int_prin_chart_2")),
            tabPanel('Investing table',
                     mod_invest_table_ui("invest_table_1")),
            tabPanel('Summary',
                     mod_loan_invest_result_ui("loan_invest_result_1"))
          )
        )
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "campaGas"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
