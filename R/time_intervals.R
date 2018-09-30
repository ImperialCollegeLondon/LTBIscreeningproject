
#' split_time_intervals
#'
#' @param cohort
#' @param Tx_interval
#'
#' @return
#' @export
#'
#' @examples
split_time_intervals <- function(cohort,
                                 Tx_interval = 0.5) {

  cohort <-
    cohort %>%
    dplyr::mutate(
      symptoms_to_Tx = symptoms_to_Tx,
      LTBI_to_symptoms = all_tb_issdt - symptoms_to_Tx,
      Tx_to_cured = Tx_interval,
      cured_to_death = all_death_notif - Tx_to_cured)

  any_neg <- function(x) any(x < 0 & !is.na(x))

  if (any_neg(cohort$symptoms_to_Tx))
    stop("Negative time interval not permitted.")

  if (any_neg(cohort$LTBI_to_symptoms))
    stop("Negative time interval not permitted.")

  if (any_neg(cohort$cured_to_death))
    stop("Negative time interval not permitted.")

  return(cohort)
}
