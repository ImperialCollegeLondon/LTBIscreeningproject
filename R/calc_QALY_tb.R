
#' Calculate QALYs for active TB cases
#'
#' Calculate the QALYs for each active TB individuals for each of 3 alternatives:
#'  * diseasefree: to all-cause death
#'  * fatality: case-fatality 12 months from notification
#'  * cured: successfully treated
#'
#' Assume that death if it happens is within the first year of active TB.
#' Assume that active TB cases when treated and survive first year are ~~fully cured~~.
#'
#' Consider person-perspective (death) or NHS-perspective (exit uk)
#' by defining the particular time-to-event end point.
#'
#' @param intervals Time intervals for each utility
#' @param utility (list) Utility value of non-diseased individual e.g. 1. Utility value of diseased individual
#' @param age Ages in years
#' @param start_delay What time delay to time origin, to shift discounting to smaller values
#' @param discount_rate default 3.5\%
#' @param ... Additional arguments
#'
#' @return list of diseasefree, death, cured QALYs
#' @export
#'
#' @examples
#'
calc_QALY_tb <- function(intervals = NA,
                         utility,
                         age,
                         start_delay = NA,
                         discount_rate = 0.035,
                         ...){

  utils_pop <- make_utilities_pop_list(utility, n_pop = nrow(intervals))
  intervals_pop <- make_intervals_pop_list(intervals)

  QALY_partial <- partial(calc_QALY_population,
                          age = age,
                          start_delay = start_delay,
                          discount_rate = discount_rate)

  diseasefree <- QALY_partial(utility = utils_pop$diseasefree,
                              intervals = intervals_pop$diseasefree)

  fatality <- QALY_partial(utility = utils_pop$fatality,
                           intervals = intervals_pop$fatality)

  cured <- QALY_partial(utility = utils_pop$cured,
                        intervals = intervals_pop$cured)

  # otherwise cured is better than diseasefree
  cured <- pmax(cured, fatality)

  return(list(diseasefree = diseasefree,
              fatality = fatality,
              cured = cured))
}


make_intervals_pop_list <- function(intervals) {

  list(
    diseasefree = diseasefree_intervals(intervals),
    cured = cured_intervals(intervals),
    fatality = fatality_intervals(intervals))
}

diseasefree_intervals <- function(x) {
  rowSums(x) %>%
    as.list(as.data.frame(t(.)))
}

fatality_intervals <- function(x) {
  cbind(x$symptoms_to_Tx,
        x$Tx_to_cured/2) %>%
    split(., seq(nrow(.)))
}

cured_intervals <- function(x) {
  as.matrix(x) %>%
    split(., seq(nrow(.)))
}


make_utilities_pop_list <- function(utility,
                                    n_pop) {

  list(
    diseasefree =
      replicate(exp = utility$disease_free,
                n = n_pop,
                simplify = FALSE),
    fatality =
      replicate(expr = c(utility$activeTB,
                         utility$TB_Tx),
                n = n_pop,
                simplify = FALSE),
    cured =
      replicate(expr = c(utility$activeTB,
                         utility$TB_Tx,
                         utility$postTx),
                n = n_pop,
                simplify = FALSE))
}
