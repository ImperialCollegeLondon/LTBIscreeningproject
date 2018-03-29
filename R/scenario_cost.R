
#' scenario_cost
#'
#' @param endpoint
#' @param unit_cost.aTB_TxDx
#' @param NUM_SECONDARY_INF
#' @param costeff_cohort
#' @param total_tb
#' @param avoid_tb
#'
#' @return
#' @export
#'
#' @examples
scenario_cost <- function(endpoint,
                          unit_cost.aTB_TxDx,
                          NUM_SECONDARY_INF,
                          costeff_cohort,
                          total_tb,
                          avoid_tb) {

  uk_1st_discounts <- costeff_cohort$uk_notif_discounts
  uk_2nd_discounts <- costeff_cohort$uk_secondary_inf_discounts

  all_1st_discounts <- costeff_cohort$all_notif_discounts
  all_2nd_discounts <- costeff_cohort$all_secondary_inf_discounts

  # removed randomness
  # unit_cost.aTB_TxDx <- mean_cost.aTB_TxDx

  rcost <-
    unit_cost.aTB_TxDx %>%
    treeSimR::sample_distributions() %>%
    sum()

  # secondary infections
  # in following year
  # removed randomness
  # num_sec_inf <- mean_num_sec_inf

  num_sec_inf <-
    NUM_SECONDARY_INF %>%
    treeSimR::sample_distributions() %>%
    unlist()

  # random sample individuals
  who_all_tb_avoided <- sample(x = seq_len(total_tb),
                               size = avoid_tb['all'],
                               replace = FALSE)

  if (endpoint == "exit uk") {

    notif.statusquo <- cost_tb_notif(num_sec_inf,
                                     rcost,
                                     uk_2nd_discounts,
                                     uk_1st_discounts)
    notif.screened <- notif.statusquo

    ##TODO: remove randomness for testing
    # random sample individuals
    who_tb_avoided_cost <- sample(x = seq_along(notif.screened),
                                  size = avoid_tb['uk'],
                                  replace = FALSE)

    # use this so that more cases avoided is always more QALYs gained
    # creates clumped data tho
    # who_tb_avoided_cost <- seq(1, unlist(num_avoided.uk_tb))

  }else if (endpoint == "death") {

    notif.statusquo <- cost_tb_notif(num_sec_inf,
                                     rcost,
                                     all_2nd_discounts,
                                     all_1st_discounts)
    notif.screened <- notif.statusquo

    who_tb_avoided_cost <- who_all_tb_avoided

    # use this so that more cases avoided is always more QALYs gained
    # who_tb_avoided_cost <- seq(1, unlist(num_avoided.all_tb))
  }

  notif.screened[who_tb_avoided_cost] <- 0

  statusquo <- sum(notif.statusquo)
  screened  <- sum(notif.screened)

  return(list(statusquo = statusquo,
              screened = screened))
}

