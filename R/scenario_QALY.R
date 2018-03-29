
#' scenario_QALY
#'
#' @param avoided
#' @param total
#' @param costeff_data
#'
#' @return
#' @export
#'
#' @examples
scenario_QALY <- function(avoided,
                          total,
                          costeff_data) {

  QALY_statusquo <- costeff_data$QALY_statusquo
  QALY_diseasefree <- costeff_data$QALY_diseasefree

  # random sample individuals
  who_all_tb_avoided <- sample(x = seq_num(total),
                               size = avoid_tb['all'],
                               replace = FALSE)

  screened <- QALY_statusquo
  screened[who_all_tb_avoided] <- QALY_diseasefree[who_all_tb_avoided]

  statusquo <- sum(QALY_statusquo)
  screened  <- sum(screened)

  return(list(statusquo = statusquo,
              screened = screened))
}



