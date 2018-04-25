
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
#' @param utility (list) Utility value of non-diseased individual e.g. 1. Utility value of diseased individual
#' @param age Ages in years
#' @param start_delay What time delay to time origin, to shift discounting to smaller values
#' @param ... Additional arguments
#'
#' @return list of diseasefree, death, cured QALYs
#' @export
#'
#' @examples
#'
calc_QALY_tb <- function(timetoevent,
                         utility,
                         age,
                         start_delay = NA,
                         ...){

  if (utility$disease_free < 0 || utility$disease_free > 1)
    stop("Utility of disease free must be between 0 and 1")

<<<<<<< HEAD
  if (utility$activeTB < 0 || utility$activeTB > 1)
=======
  if (utility$case < 0 || utility$case > 1)
>>>>>>> origin/master
    stop("Utility of cases must be between 0 and 1")

  if (utility$postTx < 0 || utility$postTx > 1)
    stop("Utility of post-treatment must be between 0 and 1")

    if (is.list(timetoevent)) {
    timetoevent <-
      timetoevent %>%
      unlist() %>%
      unname()
  }

  timetoevent[timetoevent < 0] <- 0

  QALY_partial <- partial(calc_QALY_population,
                          age = age,
                          start_delay = start_delay)

  diseasefree <- QALY_partial(utility = utility$disease_free,
                              time_horizons = timetoevent)
<<<<<<< HEAD
  fatality <- QALY_partial(utility = utility$activeTB,
                           time_horizons = pmin(timetoevent, 0.5)) #ie 6 months
  cured <- QALY_partial(utility = c(utility$activeTB, utility$postTx),
=======
  fatality <- QALY_partial(utility = utility$case,
                           time_horizons = pmin(timetoevent, 0.5)) #ie 6 months
  cured <- QALY_partial(utility = c(utility$case, utility$postTx),
>>>>>>> origin/master
                        time_horizons = timetoevent)

  #otherwise cured is better than disease_free
  for (i in seq_along(timetoevent)) {

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
