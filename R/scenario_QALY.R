
#' scenario_QALY
#'
#' @param avoided
#' @param total
#' @param QALY_statusquo
#' @param QALY_diseasefree
#'
#' @return
#' @export
#'
#' @examples
scenario_QALY <- function(avoided,
                          total,
                          QALY_statusquo,
                          QALY_diseasefree) {

  # random sample individuals
  who_all_tb_avoided <- sample(x = 1:unlist(total),
                               size = unlist(avoided),
                               replace = FALSE)

  screened <- QALY_statusquo
  screened[who_all_tb_avoided] <- QALY_diseasefree[who_all_tb_avoided]

  statusquo <- sum(QALY_statusquo)
  screened  <- sum(screened)

  return(list(statusquo = statusquo,
              screened = screened))
}



