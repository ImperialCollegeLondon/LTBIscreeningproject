
#' screen_discount
#'
#' Average discount cost and QALYs in decision tree
#' due to delayed start of screening from port of entry.
#'
#' @param cohort individual level data
#' @param discount_rate default: 3.5\% per annum
#'
#' @return single numeric proportion between 0 and 1
#' @export
#'
#' @examples
#'
screen_discount <- function(cohort,
                            discount_rate = 0.035) {

    # cohort <-
    #   cohort %>%
    #   assert(not_na, screen_year) %>%
    #   assert(screen_year >= 0)

  prop_year <-
    cohort$screen_year %>%
    ceiling() %>%
    prop_table() %>%
    matrix()

  t_lim_screen <- length(prop_year)
  discounts_year <- QALY::discount(t_limit = t_lim_screen,
                                   discount_rate = discount_rate)

  res <- discounts_year %*% prop_year

  return(as.numeric(res))
}
