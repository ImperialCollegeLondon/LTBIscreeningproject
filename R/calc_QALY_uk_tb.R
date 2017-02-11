
#' Calculate QALYs for UK Active TB Cases
#'
#' Calculate the QALYs for each active TB individuals for each of 3 alternatives:
#'  * diseasefree: to all-cause death
#'  * fatality: case-fatality 12 months from notification
#'  * cured: successfully treated
#'
#' Assume that death when if happens is within the first year of active TB.
#' Assume that active TB cases when treated and survive first year are fully cured.
#'
#' Consider person-perspective (death) or NHS-perspective (exit uk)
#' by defining the particular time-to-event end point.
#'
#' @param data Data set with TB status, death, exit uk and active TB notification times
#' @param utility.disease_free Utility value of non-diseased individual
#' @param utility.case Utility value of diseased individual
#' @param endpoint Either "death" or "exit uk"
#' @param ... Additional arguments
#'
#' @return list of diseasefree, death, cured QALYs
#' @export
#'
#' @examples
#'
calc_QALY_uk_tb <- function(data,
                            utility.disease_free = 1,
                            utility.case,
                            endpoint = c("death", "exit uk"),
                            ...){


  if(utility.disease_free<0 | utility.disease_free>1) stop("Utility of disease free must be between 0 and 1")
  if(utility.case<0 | utility.case>1) stop("Utility of cases must be between 0 and 1")

  match.arg(arg = endpoint)

  days_in_year <- 365

  # complete years from active TB diagnosis
  if (endpoint=="death"){

    timetoevent <-  with(data,
                         floor((date_death1_issdt - rNotificationDate_issdt)/days_in_year))
  }else if (endpoint=="exit uk"){

    timetoevent <-  with(data,
                         floor((date_exit_uk1_issdt - rNotificationDate_issdt)/days_in_year))
  }

  timetoevent[timetoevent<0] <- 0

  diseasefree <- QALY::calc_QALY_population(utility = utility.disease_free,
                                            time_horizons = timetoevent)#, ...)

  fatality <- QALY::calc_QALY_population(utility = utility.case,
                                         time_horizons = pmin(timetoevent, 1))#, halfend = TRUE)#, ...)

  cured <- QALY::calc_QALY_population(utility = c(utility.case, utility.disease_free),
                                      time_horizons = timetoevent)#, ...)

  return(list(diseasefree = diseasefree,
              fatality = fatality,
              cured = cured))
}
