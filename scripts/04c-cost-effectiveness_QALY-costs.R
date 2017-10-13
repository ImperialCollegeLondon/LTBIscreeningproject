# ***************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB
# random sampling individuals


## interactive
# dectree_res <- readRDS(file.choose())

if (!exists("dectree_res")) {
  dectree_res <- readRDS(paste0("Q:/R/cluster--LTBI-decision-tree/", cluster_output_filename))
}


# data format prep --------------------------------------------------------

# convert from scenario-wise to remain-exit format
n.tb_screen <-
  purrr::map(dectree_res, "mc_n.tb_screen") %>%
  purrr::transpose()

n.tb_screen.all_tb <- n.tb_screen[["n.tb_screen.all_tb"]]
n.tb_screen.uk_tb  <- n.tb_screen[["n.tb_screen.uk_tb"]]

p_LTBI_to_effectiveTx <-  purrr::map(dectree_res, "p_LTBI_to_effectiveTx") %>% unlist() %>% unname()

n.scenarios <- length(n.tb_screen.all_tb)

n.diseasefree.all_tb <- purrr::map(n.tb_screen.all_tb, function(x) dplyr::filter(x, status == "disease-free"))
n.diseasefree.uk_tb  <- purrr::map(n.tb_screen.uk_tb,  function(x) dplyr::filter(x, status == "disease-free"))

n_all_tb <- subset(dectree_res[[1]]$mc_n.tb_screen$n.tb_screen.all_tb, sim == 1)$n %>% sum()
n_uk_tb  <- subset(dectree_res[[1]]$mc_n.tb_screen$n.tb_screen.uk_tb, sim == 1)$n %>% sum()


# declare variables

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


# extract tb cases only data

cfr <- purrr::discard(IMPUTED_sample_year_cohort$cfr, is.na)

QALY_statusquo <- purrr::discard(IMPUTED_sample_year_cohort$QALY_statusquo, is.na)
QALY_diseasefree <- purrr::discard(IMPUTED_sample_year_cohort$QALY_diseasefree, is.na)
QALY_cured <- purrr::discard(IMPUTED_sample_year_cohort$QALY_cured, is.na)
QALY_fatality <- purrr::discard(IMPUTED_sample_year_cohort$QALY_fatality, is.na)

uk_notif_discounts <- purrr::discard(IMPUTED_sample_year_cohort$uk_notif_discounts, is.na)
all_notif_discounts <- purrr::discard(IMPUTED_sample_year_cohort$all_notif_discounts, is.na)
uk_secondary_inf_discounts <- purrr::discard(IMPUTED_sample_year_cohort$uk_secondary_inf_discounts, is.na)
all_secondary_inf_discounts <- purrr::discard(IMPUTED_sample_year_cohort$all_secondary_inf_discounts, is.na)


# expected statistics ------------------------------------------------------
# for reproducability and comparison

mean_cost.aTB_TxDx <- means_distributions(unit_cost$aTB_TxDx) %>% sum()
mean_num_sec_inf <- means_distributions(NUM_SECONDARY_INF) %>% unlist()

E_cost_secondary_inf <- mean_num_sec_inf * mean_cost.aTB_TxDx * all_secondary_inf_discounts
E_cost_notif.statusquo <- (all_notif_discounts * mean_cost.aTB_TxDx) + E_cost_secondary_inf

E_QALY_notif.statusquo <- (cfr * QALY_fatality) + ((1 - cfr) * QALY_cured)


for (s in seq_len(n.scenarios)) {

  print(sprintf("[ population model ] scenario: %d", s))

  cost.screened[[s]] <- QALY.screened[[s]] <- NA
  cost_incur[[s]] <- cost_incur_person[[s]] <- NA   #cost[screen] - cost[statusquo]
  QALYgain[[s]] <- QALYgain_person[[s]]  <- NA   #QALY[screen] - QALY[statusquo]
  QALY.statusquo[[s]] <- cost.statusquo[[s]] <- NA
  QALY.statusquo_person[[s]] <- cost.statusquo_person[[s]] <- NA
  QALY.screened_person[[s]] <- cost.screened_person[[s]] <- NA

  set.seed(12345)

  # QALYs and cost with screening -------------------------------------------

  for (i in seq_len(N.mc)) {

    # removed randomness
    # unit_cost.aTB_TxDx <- mean_cost.aTB_TxDx

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
                                 size = unlist(num_avoided.all_tb),
                                 replace = FALSE)

    if (ENDPOINT_cost == "exit uk") {

      cost_notif.statusquo <- cost_tb_notif(num_sec_inf,
                                            unit_cost.aTB_TxDx,
                                            uk_secondary_inf_discounts,
                                            uk_notif_discounts)
      cost_notif.screened <- cost_notif.statusquo

      ##TODO: remove randomness for testing
      # random sample individuals
      who_tb_avoided_cost <- sample(x = seq_along(cost_notif.screened),
                                    size = unlist(num_avoided.uk_tb),
                                    replace = FALSE)

      # use this so that more cases avoided is always more QALYs gained
      # creates clumped data tho
      # who_tb_avoided_cost <- seq(1, unlist(num_avoided.uk_tb))

    }else if (ENDPOINT_cost == "death") {

      cost_notif.statusquo <- cost_tb_notif(num_sec_inf,
                                            unit_cost.aTB_TxDx,
                                            all_secondary_inf_discounts,
                                            all_notif_discounts)
      cost_notif.screened <- cost_notif.statusquo

      who_tb_avoided_cost <- who_all_tb_avoided

      # use this so that more cases avoided is always more QALYs gained
      # who_tb_avoided_cost <- seq(1, unlist(num_avoided.all_tb))
    }

    cost_notif.screened[who_tb_avoided_cost] <- 0

    cost.statusquo[[s]][i] <- sum(cost_notif.statusquo)
    cost.screened[[s]][i]  <- sum(cost_notif.screened)

    # use this so that more cases avoided is always more QALYs gained
    # who_all_tb_avoided <- seq(1, unlist(num_avoided.all_tb))

    QALY_screened <- QALY_statusquo
    QALY_screened[who_all_tb_avoided] <- QALY_diseasefree[who_all_tb_avoided]

    QALY.statusquo[[s]][i] <- sum(QALY_statusquo)
    QALY.screened[[s]][i]  <- sum(QALY_screened)
  }

  E_QALY_notif.screened[s] <- sum(p_LTBI_to_effectiveTx[s] * QALY_diseasefree + (1 - p_LTBI_to_effectiveTx[s]) * E_QALY_notif.statusquo)

  E_cost_notif.screened[s] <- (1 - p_LTBI_to_effectiveTx[s]) * sum(E_cost_notif.statusquo)


  # final cost-effectiveness statistics  ---------------------------------------

  QALY.screened_person[[s]]  <- QALY.screened[[s]]/pop_year
  QALY.statusquo_person[[s]] <- QALY.statusquo[[s]]/pop_year

  # Q1 - Q0: +ve good
  # C1 - C0: +ve bad

  QALYgain[[s]] <- QALY.screened[[s]] - QALY.statusquo[[s]]

  # per person
  QALYgain_person[[s]] <- QALYgain[[s]]/pop_year

  # cost incurred per person for each simulation
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
                     E_QALYgain_person = E_QALYgain_person,
                     pop_year = pop_year)

save(aTB_CE_stats,
     file = pastef(diroutput, "aTB_CE_stats.RData"))

