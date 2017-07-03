#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB


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
n.diseasefree.uk_tb  <- map(n.tb_screen.uk_tb, function(x) dplyr::filter(x, status == "disease-free"))


aTB_cost.screened <- aTB_QALY.screened <- list()
aTB_cost_incur <- aTB_cost_incur_person <- list()
aTB_QALYgain <- aTB_QALYgain_person <- list()
aTB_ICER <- aTB_INMB <- list()
aTB_p.costEffective <- list()
aTB_QALY.statusquo <- aTB_cost.statusquo <- list()

E.aTB_cost.screened <- NA
E.aTB_QALY.screened <- NA


# discounts for costs

uk_notif_dates <-
  IMPUTED_sample_year_cohort$rNotificationDate_issdt.years %>%
  keep(function(x) !is.na(x) & x < Inf)

ceiling_notif_dates <- ceiling(uk_notif_dates)

ydiscounts <- QALY::discount(t_limit = max(ceiling_notif_dates) + 1)
uk_notif_discounts <- ydiscounts[ceiling_notif_dates]
secondary_inf_discounts <- ydiscounts[ceiling_notif_dates + 1]

cfr <- discard(IMPUTED_sample_year_cohort$cfr, is.na)


for (s in seq_len(n.scenarios)) {

  print(sprintf("scenario: %d", s))

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

    cost_secondary_inf <- num_sec_inf * unit_cost.aTB_TxDx * secondary_inf_discounts

    cost_uk_notif.statusquo <- (uk_notif_discounts * unit_cost.aTB_TxDx) + cost_secondary_inf
    cost_uk_notif.screened  <- cost_uk_notif.statusquo

    num_avoided.all_tb <- n.diseasefree.all_tb[[s]][i, 'n']
    num_avoided.uk_tb  <- n.diseasefree.uk_tb[[s]][i, 'n']

    # TRUE if death due to active TB
    tb_fatality <- runif(length(cfr)) < cfr

    who_uk_tb_avoided <- base::sample(x = seq_along(cost_uk_notif.screened),
                                      size = unlist(num_avoided.uk_tb))

    cost_uk_notif.screened[who_uk_tb_avoided] <- 0

    # substitute in QALYs for active TB death
    QALY_all_tb$cured[tb_fatality] <- QALY_all_tb$fatality[tb_fatality]

    aTB_QALY.statusquo[[s]][i] <- sum(QALY_all_tb$cured)

    aTB_QALY.screened[[s]][i] <-
      screened_cohort_QALYs(unlist(num_avoided.all_tb),
                            QALY_all_tb) %>% sum()

    aTB_cost.statusquo[[s]][i] <- sum(cost_uk_notif.statusquo)
    aTB_cost.screened[[s]][i]  <- sum(cost_uk_notif.screened)

    # reset to status-quo QALYs
    QALY_all_tb$cured <- QALY_tb_cured_original
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

  # expected total aTB screening cost over all simulations in scenario
  E.aTB_cost.screened[s] <-
    aTB_cost.screened[[s]] %>%
    mean(na.rm = TRUE)

  # expected total aTB screening QALYs over all simulations in scenario
  E.aTB_QALY.screened[s] <-
    aTB_QALY.screened[[s]] %>%
    mean(na.rm = TRUE)

  # proportion CE at wtp_threshold/QALY
  aTB_p.costEffective[[s]] <- prop.table(table(aTB_INMB[[s]] > 0, useNA = "no"))
}


#  save --------------------------------------------------------------------

aTB_CE_stats <- list(aTB_QALY.statusquo = aTB_QALY.statusquo,
                     aTB_cost.statusquo = aTB_cost.statusquo,
                     aTB_cost_incur = aTB_cost_incur,
                     aTB_QALYgain = aTB_QALYgain,
                     aTB_cost_incur_person = aTB_cost_incur_person,
                     aTB_QALYgain_person = aTB_QALYgain_person,
                     aTB_p.costEffective = aTB_p.costEffective)

aTB_CE_stats_scenario <- cbind(E.aTB_cost.screened,
                               E.aTB_QALY.screened)

save(aTB_CE_stats,
     file = pastef(diroutput, "aTB_CE_stats.RData"))

save(aTB_CE_stats_scenario,
     file = pastef(diroutput, "aTB_CE_stats_scenario.RData"))

