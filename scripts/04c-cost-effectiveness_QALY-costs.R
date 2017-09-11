# ***************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB
# random sampling


## interactive
# dectree_res <- readRDS(file.choose())


dectree_res <- readRDS(paste0("Q:/R/cluster--LTBI-decision-tree/", cluster_output_filename))


# data format prep --------------------------------------------------------

# convert from scenario-wise to remain-exit format
n.tb_screen <-
  purrr::map(dectree_res, 3) %>%
  purrr::transpose()

n.tb_screen.all_tb <- n.tb_screen[["n.tb_screen.all_tb"]]
n.tb_screen.uk_tb  <- n.tb_screen[["n.tb_screen.uk_tb"]]

p_complete_Tx <-  purrr::map(dectree_res, "p_complete_Tx") %>% unlist() %>% unname()

n.scenarios <- length(n.tb_screen.all_tb)
N.mc <- dectree_res[[1]][["mc_cost"]] %>% length()

n.diseasefree.all_tb <- map(n.tb_screen.all_tb, function(x) dplyr::filter(x, status == "disease-free"))
n.diseasefree.uk_tb  <- map(n.tb_screen.uk_tb,  function(x) dplyr::filter(x, status == "disease-free"))


#  ------------------------------------------------------------------------

cost.screened <- QALY.screened <- list()
cost.screened_person <- QALY.screened_person <- list()
cost_incur <- cost_incur_person <- list()

cost.statusquo <- QALY.statusquo <- list()
cost.statusquo_person <- QALY.statusquo_person <- list()
QALYgain <- QALYgain_person <- list()

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

  cost.screened[[s]] <- QALY.screened[[s]] <- NA
  cost_incur[[s]] <- cost_incur_person[[s]] <- NA   #cost[screen] - cost[statusquo]
  QALYgain[[s]] <- QALYgain_person[[s]]  <- NA   #QALY[screen] - QALY[statusquo]
  QALY.statusquo[[s]] <- cost.statusquo[[s]] <- NA
  QALY.statusquo_person[[s]] <- cost.statusquo_person[[s]] <- NA
  QALY.screened_person[[s]] <- cost.screened_person[[s]] <- NA


  # QALYs and cost with screening -------------------------------------------

  for (i in seq_len(N.mc)) {

    # removed randomness
    # unit_cost.aTB_TxDx - mean_cost.aTB_TxDx

    unit_cost.aTB_TxDx <-
      unit_cost$aTB_TxDx %>%
      sample_distributions() %>%
      sum()

    # secondary infections
    # in following year
    # removed randomness
    # num_sec_inf <- mean_num_sec_inf

    num_sec_inf <-
      NUM_SECONDARY_INF %>%
      sample_distributions() %>%
      unlist()

    num_avoided.all_tb <- n.diseasefree.all_tb[[s]][i, 'n']
    num_avoided.uk_tb  <- n.diseasefree.uk_tb[[s]][i, 'n']

    # random sample individuals
    who_all_tb_avoided <- sample(x = 1:unlist(num_all_tb_QALY),
                                 size = unlist(num_avoided.all_tb), replace = FALSE)

    if (ENDPOINT_cost == "exit uk") {

      cost_secondary_inf <- num_sec_inf * unit_cost.aTB_TxDx * uk_secondary_inf_discounts
      cost_notif.statusquo <- (uk_notif_discounts * unit_cost.aTB_TxDx) + cost_secondary_inf
      cost_notif.screened  <- cost_notif.statusquo

      ##TODO: remove randomness
      # random sample individuals
      who_tb_avoided_cost <- sample(x = seq_along(cost_notif.screened),
                                    size = unlist(num_avoided.uk_tb), replace = FALSE)

      # use this so that more cases avoided is always more QALYs gained
      # creates clumped data tho
      # who_tb_avoided_cost <- seq(1, unlist(num_avoided.uk_tb))

    }else{

      cost_secondary_inf <- num_sec_inf * unit_cost.aTB_TxDx * all_secondary_inf_discounts
      cost_notif.statusquo <- (all_notif_discounts * unit_cost.aTB_TxDx) + cost_secondary_inf
      cost_notif.screened  <- cost_notif.statusquo

      who_tb_avoided_cost <- who_all_tb_avoided

      # use this so that more cases avoided is always more QALYs gained
      # who_tb_avoided_cost <- seq(1, unlist(num_avoided.all_tb))
    }

    cost_notif.screened[who_tb_avoided_cost] <- 0

    cost.statusquo[[s]][i] <- sum(cost_notif.statusquo)
    cost.screened[[s]][i]  <- sum(cost_notif.screened)

    # use this so that more cases avoided is always more QALYs gained
    # who_all_tb_avoided <- seq(1, unlist(num_avoided.all_tb))

    # substitute in QALYs for active TB death
    QALY_all_tb_statusquo <- QALY_all_tb$cured
    QALY_all_tb_statusquo[tb_fatality] <- QALY_all_tb$fatality[tb_fatality]

    QALY_all_tb_screened <- QALY_all_tb_statusquo
    QALY_all_tb_screened[who_all_tb_avoided] <- QALY_all_tb$diseasefree[who_all_tb_avoided]

    QALY.statusquo[[s]][i] <- sum(QALY_all_tb_statusquo)
    QALY.screened[[s]][i]  <- sum(QALY_all_tb_screened)

  }

  E_QALY_notif.screened[s] <- sum(p_complete_Tx[s] * QALY_all_tb$diseasefree + (1 - p_complete_Tx[s]) * E_QALY_notif.statusquo)

  E_cost_notif.screened[s] <- (1 - p_complete_Tx[s]) * sum(E_cost_notif.statusquo)


  # final cost-effectiveness statistics  ----------------------------------------------------

  QALY.screened_person[[s]]  <- QALY.screened[[s]]/pop_year
  QALY.statusquo_person[[s]] <- QALY.statusquo[[s]]/pop_year

  # Q1 - Q0: +ve good
  # C1 - C0: +ve bad

  QALYgain[[s]] <- QALY.screened[[s]] - QALY.statusquo[[s]]

  # per person
  QALYgain_person[[s]] <- QALYgain[[s]]/pop_year

  # cost incurred per person for each simulation
  cost.screened[[s]] <- rm_na(cost.screened[[s]])

  cost.screened_person[[s]]  <- cost.screened[[s]]/pop_year
  cost.statusquo_person[[s]] <- cost.statusquo[[s]]/pop_year

  cost_incur[[s]] <- cost.screened[[s]] - cost.statusquo[[s]]
  cost_incur_person[[s]] <- cost_incur[[s]]/pop_year

  E_cost_incur[s] <- E_cost_notif.screened[s] - sum(E_cost_notif.statusquo)
  E_cost_incur_person[s] <- E_cost_incur[s]/pop_year

  E_QALYgain[s] <- E_QALY_notif.screened[s] - sum(E_QALY_notif.statusquo)
  E_QALYgain_person[s] <- E_QALYgain[s]/pop_year
}


#  save --------------------------------------------------------------------

aTB_CE_stats <- list(QALY.statusquo = QALY.statusquo,
                     cost.statusquo = cost.statusquo,
                     QALY.screened = QALY.screened,
                     cost.screened = cost.screened,
                     QALY.screened_person = QALY.screened_person,
                     QALY.statusquo_person = QALY.statusquo_person,
                     cost.screened_person = cost.screened_person,
                     cost.statusquo_person = cost.statusquo_person,
                     cost_incur = cost_incur,
                     QALYgain = QALYgain,
                     cost_incur_person = cost_incur_person,
                     QALYgain_person = QALYgain_person,
                     E_cost_incur = E_cost_incur,
                     E_cost_incur_person = E_cost_incur_person,
                     E_QALYgain = E_QALYgain,
                     E_QALYgain_person = E_QALYgain_person)

save(aTB_CE_stats,
     file = pastef(diroutput, "aTB_CE_stats.RData"))

