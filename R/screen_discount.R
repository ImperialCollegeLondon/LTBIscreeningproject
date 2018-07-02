
#' screen_discount
#'
#' discount cost and QALYs in decision tree
#' due to delayed start of screening from entry
#'
#' @param cohort individual level data
#' @param discount_rate default 3.5\%
#'
#' @return
#' @export
#'
#' @examples
#'
screen_discount <- function(cohort,
                            discount_rate = 0.035) {

  prop_screen_year <-
    ceiling(cohort$screen_year) %>%
    prop_table() %>%
    matrix()

  t_limit_screen <- length(prop_screen_year)
  discounts_year <- QALY::discount(t_limit = t_limit_screen,
                                   discount_rate = discount_rate)

  res <- discounts_year %*% prop_screen_year

  return(as.numeric(res))
}
