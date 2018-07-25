
#' Calculate total cost of a scenario
#'
#' @param endpoint 'death' or 'exit uk'
#' @param unit_cost diagnosis and treatment cost distributions
#' @param probs_contact Proportions of individuals in subsets
#' @param costeff_cohort nrow total number of tb cases in EWNI and after exit
#' @param prop_avoided p_LTBI_to_cured
#'
#' @return list 'statusquo' and 'screened'
#' @export
#'
#' @examples
scenario_cost <- function(endpoint,
                          unit_cost,
                          probs_contact,
                          costeff_cohort,
                          prop_avoided) {

  assert_that(endpoint %in% c("death", "exit uk"))

  rcost <- rcontact_tracing_costs(unit_cost)

  keep_tb <-
    switch(endpoint,
           "death" = costeff_cohort$all_tb,
           "exit uk" = costeff_cohort$uk_tb)

  num_contacts <- costeff_cohort$num_contacts[keep_tb]
  discounts <- costeff_cohort$all_notif_discounts[keep_tb]
  id_avoided_tb <- costeff_cohort$id_avoided_tb[keep_tb]

  notif_statusquo <-
    notif_cost(rcost,
               probs_contact,
               num_contacts,
               discounts)

  notif_screened <- notif_statusquo

  who_avoided <- rows_first_n_ids(id_avoided_tb,
                                  prop_avoided)

  notif_screened[who_avoided] <- 0

  return(list(statusquo = sum(notif_statusquo),
              screened = sum(notif_screened)))
}

#
notif_cost <- function(cost,
                       probs,
                       num_contacts,
                       discounts) {

  ccontact <-
    total_contact_tracing_cost(num_contacts,
                               cost,
                               probs)

  ctotal <- (cost['index'] + ccontact) * discounts

  return(ctotal)
}

#
total_contact_tracing_cost <- function(num_contacts,
                                       costs,
                                       probs) {

  cnames <- names(probs)
  c_per_contact <- costs[cnames] %*% probs[cnames]

  return(as.vector(c_per_contact) * num_contacts)
}

#
rcontact_tracing_costs <- function(unit_cost) {

  c(
    contact =
      unit_cost$TST %>%
      sample_distributions() %>%
      sum(),
    aTB_Dx =
      unit_cost$aTB_Dx %>%
      sample_distributions() %>%
      sum(),
    aTB_Tx =
      unit_cost$aTB_Tx %>%
      sample_distributions() %>%
      sum(),
    LTBI_DxTx =
      unit_cost$LTBI_DxTx %>%
      sample_distributions() %>%
      sum(),
    index =
      unit_cost$aTB_TxDx %>%
      sample_distributions() %>%
      sum()
  )
}
