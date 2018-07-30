#' expected_cost_QALY
#'
#' @param cohort
#' @param means
#'
#' @return
#' @export
#'
#' @examples
expected_cost_QALY <- function(cohort,
                               means) {
  cohort %>%
    mutate(E_cost_sec_inf = means$num_sec_inf * means$cost.aTB_TxDx * all_secondary_inf_discounts,
           E_cost_statusquo = (all_notif_discounts * means$cost.aTB_TxDx) + E_cost_sec_inf,
           E_QALY_statusquo = (cfr * QALY_fatality) + ((1 - cfr) * QALY_cured))
}


#
mean_QALYs <- function(cohort,
                       p_contact_tracing) {

  pTB <- p_contact_tracing['aTB_Tx']
  QF <- mean(cohort$QALY_fatality, na.rm = TRUE)
  QD <- mean(cohort$QALY_diseasefree, na.rm = TRUE)
  QC <- mean(cohort$QALY_cured, na.rm = TRUE)
  pF <- 0.012 #15-45

  return(
    list(statusquo = pTB*(pF*QF + (1 - pF)*QC) + (1 - pTB)*QD,
         diseasefree = QD))
}
