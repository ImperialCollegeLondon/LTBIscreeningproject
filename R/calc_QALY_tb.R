
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
#' @param timetoevent Time (in years) from TB notification to final event (death)
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
calc_QALY_tb <- function(timetoevent = NA,
                         intervals = NA,
                         utility,
                         age,
                         start_delay = NA,
                         discount_rate = 0.035,
                         ...){

  if (is.list(timetoevent)) {
    timetoevent <-
      timetoevent %>%
      unlist() %>%
      unname()
  }

  QALY_partial <- partial(calc_QALY_population,
                          age = age,
                          start_delay = start_delay,
                          discount_rate = discount_rate, ...)

  diseasefree <- QALY_partial(utility = utility$disease_free,
                              intervals = sum(intervals))

  ##TODO:...
  fatality <- QALY_partial(utility = c(utility$activeTB, utility$TB_Tx),
                           intervals = intervals[1] + intervals[2]/2)
                           # time_horizons = pmin(timetoevent, 0.5)) #ie 6 months

  cured <- QALY_partial(utility = c(utility$activeTB, utility$TB_Tx, utility$postTx),
                        intervals = intervals[1:3])
                        # time_horizons = timetoevent)

  #otherwise cured is better than disease_free
  for (i in seq_along(cured)) {

    cured[i] <-
      if (timetoevent[i] < 1) {
        fatality[i]
      } else {
        cured[i]
      }
  }

  return(list(diseasefree = diseasefree,
              fatality = fatality,
              cured = cured))
}
