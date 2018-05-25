
#' Calculate total cost of a scenario
#'
#' @param endpoint 'death' or 'exit uk'
#' @param unit_cost.aTB_TxDx diagnosis and treatment cost distributions
#' @param costeff_cohort nrow total number of tb cases in EWNI and after exit
#' @param prop_avoided p_LTBI_to_cured
#'
#' @return list 'statusquo' and 'screened'
#' @export
#'
#' @examples
scenario_cost <- function(endpoint,
                          unit_cost.aTB_TxDx,
                          costeff_cohort,
                          prop_avoided) {

  assert_that(endpoint %in% c("death", "exit uk"))

  rcost <-
    unit_cost.aTB_TxDx %>%
    treeSimR::sample_distributions() %>%
    sum()

  keep_tb <-
    switch(endpoint,
           "death" = costeff_cohort$all_tb,
           "exit uk" = costeff_cohort$uk_tb)

  num_2nd_inf <- costeff_cohort$num_2nd_inf[keep_tb]
  discounts_1st <- costeff_cohort$all_notif_discounts[keep_tb]
  discounts_2nd <- costeff_cohort$all_secondary_inf_discounts[keep_tb]

  id_avoided_tb <- costeff_cohort$id_avoided_tb[keep_tb]

  notif_statusquo <- cost_tb_notif(num_2nd_inf,
                                   rcost,
                                   discounts_1st,
                                   discounts_2nd)
  notif_screened <- notif_statusquo

  who_avoided <- rows_first_n_ids(id_avoided_tb,
                                  prop_avoided)

  notif_screened[who_avoided] <- 0

  return(list(statusquo = sum(notif_statusquo),
              screened = sum(notif_screened)))
}

