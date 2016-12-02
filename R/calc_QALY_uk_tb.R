
#' Calculate QALYs for UK Active TB Cases
#'
#' Assume that death when if happens is within the first year of active TB.
#' Assume that active TB cases when treated and survive first year are fully cured.
#'
#' Consider person-perspective or NHS-perspective. by defining the time-to-event.
#'
#' @param data  Data set with tb status, death, exit uk and notification times
#' @param utility.disease_free value
#' @param utility.case value
#' @param endpoint either death or exit uk
#'
#' @return list(diseasefree, death, cured)
#' @export
#'
#' @examples
#'
calc_QALY_uk_tb <- function(data,
                            utility.disease_free,
                            utility.case,
                            endpoint = c("death", "exit uk")){

  match.arg(arg = endpoint)
  stopifnot(utility.disease_free>=0, utility.disease_free<=1)
  stopifnot(utility.case>=0, utility.case<=1)

  uk_tb_only <- subset(data, uk_tb==1)
  days_in_year <- 365

  if (endpoint=="death"){

    timetoevent <-  with(uk_tb_only, floor((date_death1_issdt - rNotificationDate_issdt)/days_in_year))
  }else if (endpoint=="exit uk"){

    timetoevent <-  with(uk_tb_only, floor((date_exit_uk1_issdt - rNotificationDate_issdt)/days_in_year))
  }

  timetoevent[timetoevent<0] <- 0

  diseasefree <- QALY::calc_QALY_population(utility = utility.disease_free,
                                            time_horizons = timetoevent)

  death <- QALY::calc_QALY_population(utility = utility.case,
                                      time_horizons = 1)
  death <- rep(death, length(diseasefree))


  cured <- QALY::calc_QALY_population(utility = c(utility.case, utility.disease_free),
                                      time_horizons = timetoevent)

  return(list(diseasefree = diseasefree,
              death = death,
              cured = cured))
}
