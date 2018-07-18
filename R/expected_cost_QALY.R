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
