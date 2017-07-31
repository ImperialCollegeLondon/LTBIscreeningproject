#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB
# random sampling


if (cluster) {

    dectree_res <- readRDS(paste0("Q:/R/cluster--LTBI-decision-tree/", cluster_output_filename))

    n.tb_screen <-
      purrr::map(dectree_res, 3) %>%
      purrr::transpose() #previously calc'd scenario-wise

    n.tb_screen.all_tb <- n.tb_screen[[1]]
    n.tb_screen.uk_tb  <- n.tb_screen[[2]]

    n.scenarios <- length(n.tb_screen.all_tb)
    N.mc <- dectree_res[[1]][["mc_cost"]] %>% length()
}

n.diseasefree.all_tb <- map(n.tb_screen.all_tb, function(x) dplyr::filter(x, status == "disease-free"))
n.diseasefree.uk_tb  <- map(n.tb_screen.uk_tb,  function(x) dplyr::filter(x, status == "disease-free"))


aTB_cost.screened <- aTB_QALY.screened <- list()
aTB_cost_incur <- aTB_cost_incur_person <- list()
aTB_QALYgain <- aTB_QALYgain_person <- list()
aTB_ICER <- aTB_INMB <- list()
aTB_p.costEffective <- list()
aTB_QALY.statusquo  <- aTB_cost.statusquo <- list()

E.aTB_cost.screened <- NA
E.aTB_QALY.screened <- NA


# discounts for costs

uk_notif_dates <-
  IMPUTED_sample_year_cohort$rNotificationDate_issdt.years %>%
  keep(function(x) !is.na(x) & x < Inf) %>%
  ceiling()

all_notif_dates <-
  IMPUTED_sample_year_cohort$all_tb_issdt %>%
  keep(function(x) !is.na(x) & x < Inf) %>%
  ceiling()

ydiscounts <- QALY::discount(t_limit = max(all_notif_dates) + 1)

uk_notif_discounts <- ydiscounts[uk_notif_dates]
all_notif_discounts <- ydiscounts[all_notif_dates]

uk_secondary_inf_discounts <- ydiscounts[uk_notif_dates + 1]
all_secondary_inf_discounts <- ydiscounts[all_notif_dates + 1]

cfr <- discard(IMPUTED_sample_year_cohort$cfr, is.na)


for (s in seq_len(n.scenarios)) {

  print(sprintf("[ population model ] scenario: %d", s))

  aTB_cost.screened[[s]] <- aTB_QALY.screened[[s]] <- NA
  aTB_cost_incur[[s]] <- aTB_cost_incur_person[[s]] <- NA   #cost[screen] - cost[statusquo]
  aTB_QALYgain[[s]]  <- aTB_QALYgain_person[[s]]  <- NA   #QALY[screen] - QALY[statusquo]
  aTB_ICER[[s]] <- aTB_INMB[[s]] <- NA
  aTB_p.costEffective[[s]] <- NA
  aTB_QALY.statusquo[[s]]  <- aTB_cost.statusquo[[s]] <- NA


  # QALYs and cost with screening -------------------------------------------

  for (i in seq_len(N.mc)) {

    unit_cost.aTB_TxDx <-
      unit_cost$aTB_TxDx %>%
      sample_distributions() %>%
      sum()

    # secondary infections
    # in following year

    num_sec_inf <-
      NUM_SECONDARY_INF %>%
      sample_distributions() %>%
      unlist()

    num_avoided.all_tb <- n.diseasefree.all_tb[[s]][i, 'n']
    num_avoided.uk_tb  <- n.diseasefree.uk_tb[[s]][i, 'n']

    # TRUE if death due to active TB
    tb_fatality <- runif(length(cfr)) < cfr

    if (ENDPOINT_cost == "exit uk") {

      cost_secondary_inf <- num_sec_inf * unit_cost.aTB_TxDx * uk_secondary_inf_discounts
      cost_notif.statusquo <- (uk_notif_discounts * unit_cost.aTB_TxDx) + cost_secondary_inf
      cost_notif.screened  <- cost_notif.statusquo

      who_tb_avoided  <- sample(x = seq_along(cost_notif.screened),
                                size = unlist(num_avoided.uk_tb), replace = FALSE)
    }else{

      cost_secondary_inf <- num_sec_inf * unit_cost.aTB_TxDx * all_secondary_inf_discounts
      cost_notif.statusquo <- (all_notif_discounts * unit_cost.aTB_TxDx) + cost_secondary_inf
      cost_notif.screened  <- cost_notif.statusquo

      who_tb_avoided <- sample(x = seq_along(cost_notif.screened),
                               size = unlist(num_avoided.all_tb), replace = FALSE)
    }

    cost_notif.screened[who_tb_avoided] <- 0

    aTB_cost.statusquo[[s]][i] <- sum(cost_notif.statusquo)
    aTB_cost.screened[[s]][i]  <- sum(cost_notif.screened)

    who_all_tb_avoided <- sample(x = 1:unlist(num_all_tb_QALY),
                                 size = unlist(num_avoided.all_tb), replace = FALSE)

    # substitute in QALYs for active TB death
    QALY_all_tb_statusquo <- QALY_all_tb$cured
    QALY_all_tb_statusquo[tb_fatality] <- QALY_all_tb$fatality[tb_fatality]

    QALY_all_tb_screened <- QALY_all_tb_statusquo
    QALY_all_tb_screened[who_all_tb_avoided] <- QALY_all_tb$diseasefree[who_all_tb_avoided]

    aTB_QALY.statusquo[[s]][i] <- sum(QALY_all_tb_statusquo)
    aTB_QALY.screened[[s]][i]  <- sum(QALY_all_tb_screened)
  }


  # final cost-effectiveness statistics  ----------------------------------------------------

  # Q1 - Q0: +ve good
  # C1 - C0: +ve bad

  aTB_QALYgain[[s]] <- aTB_QALY.screened[[s]] - aTB_QALY.statusquo[[s]]
  aTB_QALYgain[[s]] <- rm_na(aTB_QALYgain[[s]])

  # per person
  aTB_QALYgain_person[[s]] <- aTB_QALYgain[[s]]/pop_year

  # cost incurred per person for each simulation
  aTB_cost.screened[[s]] <- rm_na(aTB_cost.screened[[s]])

  aTB_cost_incur[[s]] <- aTB_cost.screened[[s]] - aTB_cost.statusquo[[s]]
  aTB_cost_incur_person[[s]] <- aTB_cost_incur[[s]]/pop_year

  # proportion CE at wtp_threshold/QALY
  aTB_p.costEffective[[s]] <- prop.table(table(aTB_INMB[[s]] > 0, useNA = "no"))
}


#  save --------------------------------------------------------------------

aTB_CE_stats <- list(aTB_QALY.statusquo = aTB_QALY.statusquo,
                     aTB_cost.statusquo = aTB_cost.statusquo,
                     aTB_QALY.screened = aTB_QALY.screened,
                     aTB_cost.screened = aTB_cost.screened,
                     aTB_cost_incur = aTB_cost_incur,
                     aTB_QALYgain = aTB_QALYgain,
                     aTB_cost_incur_person = aTB_cost_incur_person,
                     aTB_QALYgain_person = aTB_QALYgain_person,
                     aTB_p.costEffective = aTB_p.costEffective)

save(aTB_CE_stats,
     file = pastef(diroutput, "aTB_CE_stats.RData"))

