
#' Calculate the total QALYs of a scenario
#'
#' TODO: could generalise like for cost to select EWNI only
#'
#' @param avoided
#' @param costeff_cohort
#'
#' @return
#' @export
#'
#' @examples
scenario_QALY <- function(avoided,
                          costeff_cohort) {

  n_total <- nrow(costeff_cohort)

  QALY_statusquo <- costeff_cohort$QALY_statusquo
  QALY_diseasefree <- costeff_cohort$QALY_diseasefree

  # random sample individuals
  who_all_tb_avoided <- sample(x = seq_len(n_total),
                               size = avoided['all'],
                               replace = FALSE)

  screened <- QALY_statusquo
  screened[who_all_tb_avoided] <- QALY_diseasefree[who_all_tb_avoided]

  statusquo <- sum(QALY_statusquo)
  screened  <- sum(screened)

  return(list(statusquo = statusquo,
              screened = screened))
}



