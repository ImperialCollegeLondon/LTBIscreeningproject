
#' calc_QALY_uk_tb
#'
#' person-perspective or than NHS-perspective
#'
#' @param data
#' @param utility.disease_free
#' @param utility.case
#' @param endpoint
#'
#' @return
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
  if (endpoint=="death"){

    timetoevent <-  with(uk_tb_only, floor((date_death1_issdt - rNotificationDate_issdt)/365))
  }else if (endpoint=="exit uk"){

    timetoevent <-  with(uk_tb_only, floor((date_exit_uk1_issdt - rNotificationDate_issdt)/365))
  }

  timetoevent[timetoevent<0] <- 0

  diseasefree <- QALY::calc_QALY_population(utility = utility.disease_free,
                                            time_horizons = timetoevent)

  death <- QALY::calc_QALY_population(utility = utility.case,
                                      time_horizons = 1)
  death <- rep(QALY.uk_tb_death, length(QALY_diseasefree))


  cured <- QALY::calc_QALY_population(utility = c(utility.case, utility.disease_free),
                                      time_horizons = timetoevent)

  return(list(diseasefree = diseasefree,
              death = death,
              cured = cured))
}
