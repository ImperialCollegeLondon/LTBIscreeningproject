
#' Calculate total active TB cost of a scenario
#'
#' @param endpoint \code{death} or \code{exit uk} i.e. time horizon
#' @param unit_cost Diagnosis and treatment cost distributions; list
#' @param probs_contact Proportions of individuals in subsets
#' @param cohort individual level dataframe. nrow total number of TB cases in EWNI and after exit
#' @param prop_avoided p_LTBI_to_cured; single numeric
#' @param order default TRUE
#'
#' @return list total cost for \code{statusquo} and \code{screened}; numeric
#' @export
#'
#' @examples
#'
scenario_cost <- function(endpoint,
                          unit_cost,
                          probs_contact,
                          cohort,
                          prop_avoided,
                          order = TRUE) {

  assert_that(endpoint %in% c("death", "exit uk"))

  rcost <- rcontact_tracing_costs(unit_cost)

  keep_tb <-
    switch(endpoint,
           "death" = cohort$all_tb,
           "exit uk" = cohort$uk_tb)

  num_contacts <- cohort$num_contacts[keep_tb]
  discounts <- cohort$all_notif_discounts[keep_tb]
  id_avoided_tb <- cohort$id_avoided_tb[keep_tb]

  notif_statusquo <-
    notif_cost(rcost,
               probs_contact,
               num_contacts,
               discounts)

  notif_screened <- notif_statusquo

  who_avoided <- sample_avoid_lg(id_avoided_tb,
                                 prop_avoided,
                                 ordered)

  notif_screened[who_avoided] <- 0

  return(list(statusquo = sum(notif_statusquo),
              screened = sum(notif_screened)))
}


#' Combined cost for each TB case
#'
#' including secondary infections. with discounting
#'
#' @param cost vector
#' @param probs vector
#' @param num_contacts vector, per case
#' @param discounts at time of notification; vector, per case
#'
#' @return
#' @export
#'
#' @examples
#'
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


#' total_contact_tracing_cost
#'
#' For an index case.
#'
#' @param num_contacts vector, per case
#' @param costs vector
#' @param probs vector
#'
#' @return vector, per case
#' @export
#'
#' @examples
#'
total_contact_tracing_cost <- function(num_contacts,
                                       cost,
                                       probs) {

  cnames <- names(probs)
  c_per_contact <- cost[cnames] %*% probs[cnames]

  return(as.vector(c_per_contact) * num_contacts)
}


#' randomly sample constact tracing costs
#'
#' @param unit_cost named list
#'
#' @return named vector matching \code{p_contact_tracing}:
#' contact, aTB_Dx, aTB_Tx, LTBI_DxTx, index
#'
#' @export
#'
#' @examples
#'
rcontact_tracing_costs <- function(unit_cost) {

  params <- c("IGRA", "aTB_Dx", "aTB_Tx", "LTBI_DxTx", "aTB_TxDx")

  map_dbl(unit_cost[params],
      function(x) sum(sample_distributions(x))) %>%
    set_names("contact", "aTB_Dx", "aTB_Tx", "LTBI_DxTx", "index")

}
