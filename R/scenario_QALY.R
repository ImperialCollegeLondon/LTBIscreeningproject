
#' Calculate the total QALYs of a scenario
#'
#' TODO: could generalise like for cost to select EWNI only
#'
#' @param avoided Number avoided for each endpoint
#' @param costeff_cohort Individual data
#' @param endpoint 'death' or 'exit uk'
#'
#' @return list of status-quo and screen life-time QALYs
#' @export
#'
#' @examples
scenario_QALY <- function(avoided,
                          costeff_cohort,
                          endpoint = 'death') {

  QALY_statusquo <- costeff_cohort$QALY_statusquo
  QALY_diseasefree <- costeff_cohort$QALY_diseasefree

  screened <- QALY_statusquo

  who_tb_avoided <- rows_first_n_ids(costeff_cohort$id_tb_avoided,
                                     avoided[endpoint])

  screened[who_tb_avoided] <- QALY_diseasefree[who_tb_avoided]

  statusquo <- sum(QALY_statusquo)
  screened  <- sum(screened)

  return(list(statusquo = statusquo,
              screened = screened))
}



