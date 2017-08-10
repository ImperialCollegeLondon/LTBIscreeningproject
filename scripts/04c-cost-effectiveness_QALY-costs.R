#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB
# random sampling


#interactive
# dectree_res <- readRDS(file.choose())


dectree_res <- readRDS(paste0("Q:/R/cluster--LTBI-decision-tree/", cluster_output_filename))

n.tb_screen <-
  purrr::map(dectree_res, 3) %>%
  purrr::transpose() #previously calc'd scenario-wise

n.tb_screen.all_tb <- n.tb_screen[["n.tb_screen.all_tb"]]
n.tb_screen.uk_tb  <- n.tb_screen[["n.tb_screen.uk_tb"]]

p_complete_Tx <-  purrr::map(dectree_res, "p_complete_Tx") %>% unlist() %>% unname()

n.scenarios <- length(n.tb_screen.all_tb)
N.mc <- dectree_res[[1]][["mc_cost"]] %>% length()

n.diseasefree.all_tb <- map(n.tb_screen.all_tb, function(x) dplyr::filter(x, status == "disease-free"))
n.diseasefree.uk_tb  <- map(n.tb_screen.uk_tb,  function(x) dplyr::filter(x, status == "disease-free"))

#  ------------------------------------------------------------------------

aTB_cost.screened <- aTB_QALY.screened <- list()
aTB_cost_incur <- aTB_cost_incur_person <- list()
aTB_QALYgain <- aTB_QALYgain_person <- list()
aTB_QALY.statusquo  <- aTB_cost.statusquo <- list()

E_cost_notif.screened <- NA
E_QALY_notif.screened <- NA
E_cost_incur <- E_cost_incur_person <- NA
E_QALYgain <- E_QALYgain_person <- NA


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
tb_fatality <- discard(IMPUTED_sample_year_cohort$tb_fatality, is.na)


# expected statistics ------------------------------------------------------
# for reproducability and comparison

mean_cost.aTB_TxDx <- means_distributions(unit_cost$aTB_TxDx) %>% sum()
mean_num_sec_inf <- means_distributions(NUM_SECONDARY_INF) %>% unlist()

E_cost_secondary_inf <- mean_num_sec_inf * mean_cost.aTB_TxDx * all_secondary_inf_discounts
E_cost_notif.statusquo <- (all_notif_discounts * mean_cost.aTB_TxDx) + E_cost_secondary_inf

E_QALY_notif.statusquo <- (cfr * QALY_all_tb$fatality) + ((1 - cfr) * QALY_all_tb$cured)


for (s in seq_len(n.scenarios)) {

  print(sprintf("[ population model ] scenario: %d", s))

  aTB_cost.screened[[s]] <- aTB_QALY.screened[[s]] <- NA
  aTB_cost_incur[[s]] <- aTB_cost_incur_person[[s]] <- NA   #cost[screen] - cost[statusquo]
  aTB_QALYgain[[s]]  <- aTB_QALYgain_person[[s]]  <- NA   #QALY[screen] - QALY[statusquo]
  aTB_QALY.statusquo[[s]]  <- aTB_cost.statusquo[[s]] <- NA


  # QALYs and cost with screening -------------------------------------------

  for (i in seq_len(N.mc)) {

    ##TODO: removed randomness
    # unit_cost.aTB_TxDx <- mean_cost.aTB_TxDx
    unit_cost.aTB_TxDx <-
      unit_cost$aTB_TxDx %>%
      sample_distributions() %>%
      sum()

    # secondary infections
    # in following year
    ##TODO: removed randomness
    # num_sec_inf <- mean_num_sec_inf
    num_sec_inf <-
      NUM_SECONDARY_INF %>%
      sample_distributions() %>%
      unlist()

    num_avoided.all_tb <- n.diseasefree.all_tb[[s]][i, 'n']
    num_avoided.uk_tb  <- n.diseasefree.uk_tb[[s]][i, 'n']

    if (ENDPOINT_cost == "exit uk") {

      cost_secondary_inf <- num_sec_inf * unit_cost.aTB_TxDx * uk_secondary_inf_discounts
      cost_notif.statusquo <- (uk_notif_discounts * unit_cost.aTB_TxDx) + cost_secondary_inf
      cost_notif.screened  <- cost_notif.statusquo

      ##TODO: remove randomness
      # # random sample individuals
      # who_tb_avoided  <- sample(x = seq_along(cost_notif.screened),
      #                           size = unlist(num_avoided.uk_tb), replace = FALSE)

      # use this so that more cases avoided is always more QALYs gained
      who_tb_avoided <- seq(1, unlist(num_avoided.uk_tb))

    }else{

      cost_secondary_inf <- num_sec_inf * unit_cost.aTB_TxDx * all_secondary_inf_discounts
      cost_notif.statusquo <- (all_notif_discounts * unit_cost.aTB_TxDx) + cost_secondary_inf
      cost_notif.screened  <- cost_notif.statusquo

      # # random sample individuals
      # who_tb_avoided <- sample(x = seq_along(cost_notif.screened),
      #                          size = unlist(num_avoided.all_tb), replace = FALSE)

      # use this so that more cases avoided is always more QALYs gained
      who_tb_avoided <- seq(1, unlist(num_avoided.all_tb))
    }

    cost_notif.screened[who_tb_avoided] <- 0

    aTB_cost.statusquo[[s]][i] <- sum(cost_notif.statusquo)
    aTB_cost.screened[[s]][i]  <- sum(cost_notif.screened)

    ##TODO:
    # # random sample individuals
    # who_all_tb_avoided <- sample(x = 1:unlist(num_all_tb_QALY),
    #                              size = unlist(num_avoided.all_tb), replace = FALSE)

    # use this so that more cases avoided is always more QALYs gained
    who_all_tb_avoided <- seq(1, unlist(num_avoided.all_tb))

    # substitute in QALYs for active TB death
    QALY_all_tb_statusquo <- QALY_all_tb$cured
    QALY_all_tb_statusquo[tb_fatality] <- QALY_all_tb$fatality[tb_fatality]

    QALY_all_tb_screened <- QALY_all_tb_statusquo
    QALY_all_tb_screened[who_all_tb_avoided] <- QALY_all_tb$diseasefree[who_all_tb_avoided]

    aTB_QALY.statusquo[[s]][i] <- sum(QALY_all_tb_statusquo)
    aTB_QALY.screened[[s]][i]  <- sum(QALY_all_tb_screened)

  }

  E_QALY_notif.screened[s] <- sum(p_complete_Tx[s] * QALY_all_tb$diseasefree + (1 - p_complete_Tx[s]) * E_QALY_notif.statusquo)

  E_cost_notif.screened[s] <- (1 - p_complete_Tx[s]) * sum(E_cost_notif.statusquo)


  # final cost-effectiveness statistics  ----------------------------------------------------

  # Q1 - Q0: +ve good
  # C1 - C0: +ve bad

  aTB_QALYgain[[s]] <- aTB_QALY.screened[[s]] - aTB_QALY.statusquo[[s]]

  # per person
  aTB_QALYgain_person[[s]] <- aTB_QALYgain[[s]]/pop_year

  # cost incurred per person for each simulation
  aTB_cost.screened[[s]] <- rm_na(aTB_cost.screened[[s]])

  aTB_cost_incur[[s]] <- aTB_cost.screened[[s]] - aTB_cost.statusquo[[s]]
  aTB_cost_incur_person[[s]] <- aTB_cost_incur[[s]]/pop_year

  E_cost_incur[s] <- E_cost_notif.screened[s] - sum(E_cost_notif.statusquo)
  E_cost_incur_person[s] <- E_cost_incur[s]/pop_year

  E_QALYgain[s] <- E_QALY_notif.screened[s] - sum(E_QALY_notif.statusquo)
  E_QALYgain_person[s] <- E_QALYgain[s]/pop_year
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
                     E_cost_incur = E_cost_incur,
                     E_cost_incur_person = E_cost_incur_person,
                     E_QALYgain = E_QALYgain,
                     E_QALYgain_person = E_QALYgain_person)

save(aTB_CE_stats,
     file = pastef(diroutput, "aTB_CE_stats.RData"))

