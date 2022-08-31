#' Annuities
#'
#' @description A simple functions that returns monthly loan payments.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

loan_payment <- function(P, r, n, month_year){

  r <- r/1200

  if(month_year == 'Years') n <- n*12

  P*(r*(1+r)^n)/((1+r)^n-1)

}

#' Amortization plan
#'
#' @description A simple function that returns an amortization plan for paying off a loan.
#'
#' @return An amortization plan data.frame
#'
#' @noRd
#'
#' @export

amort_plan <- function(P, r, n, month_year){

  r <- r/1200

  if(month_year == 'Years') n <- n*12

  x <- data.frame(Month = 1:n,
                  Loan_value = as.numeric(NA),
                  Annuity = loan_payment(P = P,
                                         r = r*1200,
                                         n = n,
                                         month_year = 'Months'),
                  Interest_payment = as.numeric(NA),
                  Principal_payment = as.numeric(NA),
                  Remaining_principal = as.numeric(NA))

  for(i in 1:n){

    if(i == 1) x$Loan_value[i] <- P
    if(i > 1) x$Loan_value[i] <- x$Remaining_principal[i-1]
    x$Interest_payment[i] <- -x$Loan_value[i] * r
    x$Principal_payment[i] <- -x$Annuity[i] - x$Interest_payment[i]
    x$Remaining_principal[i] <- x$Loan_value[i] + x$Principal_payment[i]
  }

  return(x)
}

#' Investing table
#'
#' @description A simple functions that returns monthly investing payments and returns.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

inv_table <- function(val, r, n, month_year){

  r <- r/1200

  if(month_year == 'Years') n <- n*12

  x <- data.frame('Month' = 1:n,
                  'Invested_amount' = val) %>%
    dplyr::mutate(Return = Invested_amount * (1 + r) ^ (n - dplyr::row_number())) %>%
    janitor::adorn_totals()


}
