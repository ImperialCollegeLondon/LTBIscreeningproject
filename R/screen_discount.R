
#' screen_discount
#'
#' discount cost and QALYs in decision tree
#' due to delayed start
#'
#' @param cohort
#'
#' @return
#' @export
#'
#' @examples
#'
screen_discount <- function(cohort) {

  prop_screen_year <-
    ceiling(cohort$screen_year) %>%
    prop_table()

  t_limit_screen <- length(prop_screen_year)
  discounts_year <- QALY::discount(t_limit = t_limit_screen)

  res <- prop_screen_year %*% discounts_year

  return(as.numeric(res))
}
