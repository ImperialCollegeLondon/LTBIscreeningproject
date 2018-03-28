
#' scenario_cost
#'
#' @param interv
#' @param unit_cost
#' @param NUM_SECONDARY_INF
#' @param uk_secondary_inf_discounts
#' @param num_all_tb_QALY
#' @param uk_notif_discounts
#' @param num_avoided.all_tb
#' @param num_avoided.uk_tb
#'
#' @return
#' @export
#'
#' @examples
scenario_cost <- function(interv,
                          unit_cost,
                          NUM_SECONDARY_INF,
                          uk_secondary_inf_discounts,
                          num_all_tb_QALY,
                          uk_notif_discounts,
                          num_avoided.all_tb,
                          num_avoided.uk_tb) {

  # removed randomness
  # unit_cost.aTB_TxDx <- mean_cost.aTB_TxDx

  unit_cost.aTB_TxDx <-
    unit_cost$aTB_TxDx %>%
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
  who_all_tb_avoided <- sample(x = 1:unlist(num_all_tb_QALY),
                               size = unlist(num_avoided.all_tb),
                               replace = FALSE)

  if (interv$ENDPOINT_cost == "exit uk") {

    notif.statusquo <- cost_tb_notif(num_sec_inf,
                                     unit_cost.aTB_TxDx,
                                     uk_secondary_inf_discounts,
                                     uk_notif_discounts)
    notif.screened <- notif.statusquo

    ##TODO: remove randomness for testing
    # random sample individuals
    who_tb_avoided_cost <- sample(x = seq_along(notif.screened),
                                  size = unlist(num_avoided.uk_tb),
                                  replace = FALSE)

    # use this so that more cases avoided is always more QALYs gained
    # creates clumped data tho
    # who_tb_avoided_cost <- seq(1, unlist(num_avoided.uk_tb))

  }else if (interv$ENDPOINT_cost == "death") {

    notif.statusquo <- cost_tb_notif(num_sec_inf,
                                     unit_cost.aTB_TxDx,
                                     all_secondary_inf_discounts,
                                     all_notif_discounts)
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

