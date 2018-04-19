
#' Calculate total QALYs of a scenario
#'
#' TODO: could generalise like for cost to select EWNI only too
#'
#' @param num_avoided Number avoided for each endpoint
#' @param costeff_cohort Individual data
#' @param endpoint 'death' or 'exit uk'
#'
#' @return list of status-quo and screen life-time QALYs
#' @export
#'
#' @examples
scenario_QALY <- function(num_avoided,
                          costeff_cohort,
                          endpoint = 'death') {

  QALY_statusquo <- costeff_cohort$QALY_statusquo
  QALY_diseasefree <- costeff_cohort$QALY_diseasefree

  screened <- QALY_statusquo

  who_avoid <- rows_first_n_ids(costeff_cohort$id_avoided_tb,
                                num_avoided[endpoint])

  screened[who_avoid] <- QALY_diseasefree[who_avoid]

  statusquo <- sum(QALY_statusquo)
  screened  <- sum(screened)

  return(list(statusquo = statusquo,
              screened = screened))
}



