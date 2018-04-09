
#' Calculate the total cost of a scenario
#'
#' @param endpoint 'death' or 'exit uk'
#' @param unit_cost.aTB_TxDx diagnosis and treatment cost distributions
#' @param num_2nd_inf average number of secondary tb infections from a single index case
#' @param costeff_cohort nrow total number of tb cases in EWNI and after exit
#' @param avoid_tb
#'
#' @return
#' @export
#'
#' @examples
scenario_cost <- function(endpoint,
                          unit_cost.aTB_TxDx,
                          num_2nd_inf,
                          costeff_cohort,
                          avoid_tb) {

  assert_that(endpoint %in% c("death", "exit uk"))

  rcost <-
    unit_cost.aTB_TxDx %>%
    treeSimR::sample_distributions() %>%
    sum()

  r2nd_inf <-
    num_2nd_inf %>%
    treeSimR::sample_distributions() %>%
    unlist()

  if (endpoint == "exit uk") {

    discounts_1st <- na.omit(costeff_cohort$uk_notif_discounts)
    discounts_2nd <- na.omit(costeff_cohort$uk_secondary_inf_discounts)
    n_avoid <- avoid_tb['uk']

  } else if (endpoint == "death") {

    discounts_1st <- costeff_cohort$all_notif_discounts
    discounts_2nd <- costeff_cohort$all_secondary_inf_discounts
    n_avoid <- avoid_tb['all']
  }

  notif.statusquo <- cost_tb_notif(r2nd_inf,
                                   rcost,
                                   discounts_2nd,
                                   discounts_1st)
  notif.screened <- notif.statusquo

  who_tb_avoided_cost <- sample(x = seq_along(notif.screened),
                                size = n_avoid,
                                replace = FALSE)

  notif.screened[who_tb_avoided_cost] <- 0

  statusquo <- sum(notif.statusquo)
  screened  <- sum(notif.screened)

  return(list(statusquo = statusquo,
              screened = screened))
}

