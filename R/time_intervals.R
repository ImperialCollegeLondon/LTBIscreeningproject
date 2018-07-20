
time_intervals <- function(cohort,
                           Tx_interval = 0.5) {

  cohort <-
    cohort %>%
    dplyr::mutate(
      symptoms_to_Tx = symptoms_to_Tx,
      LTBI_to_symptoms = all_tb_issdt - symptoms_to_Tx,
      Tx_to_cured = Tx_interval,
      cured_to_death = all_death_notif - Tx_to_cured)

  if (any(cohort$symptoms_to_Tx < 0 & !is.na(cohort$symptoms_to_Tx)))
    stop("Negative time interval not permitted.")

  if (any(cohort$LTBI_to_symptoms < 0 & !is.na(cohort$LTBI_to_symptoms)))
    stop("Negative time interval not permitted.")

  if (any(cohort$cured_to_death < 0 & !is.na(cohort$cured_to_death)))
    stop("Negative time interval not permitted.")

  return(cohort)
}
