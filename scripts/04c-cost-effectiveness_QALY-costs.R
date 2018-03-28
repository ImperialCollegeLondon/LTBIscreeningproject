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
scenario_res <-
  dectree_res %>%
  purrr::transpose()

n.scenarios <- length(dectree_res)

n.diseasefree.all_tb <-
  scenario_res$n_tb_screen_all %>%
  purrr::map(function(x) dplyr::filter(x, status == "disease-free")) %>%
  map(function(x) x$n)

n.diseasefree.uk_tb  <-
  scenario_res$n_tb_screen_uk %>%
  purrr::map(function(x) dplyr::filter(x, status == "disease-free")) %>%
  map(function(x) x$n)

n_uk_tb <- dectree_res$`1`$call$n.uk_tb
n_exit_tb <- dectree_res$`1`$call$n.exit_tb
n_all_tb <- n.exit_tb + n_uk_tb


# declare variables

interv_cost <- interv_cost_vs <- vector(length = n.scenarios, mode = "list")
interv_QALY <- interv_QALY_vs <- vector(length = n.scenarios, mode = "list")
cost.screened_person <- QALY.screened_person <- list()
cost_incur <- cost_incur_person <- list()

cost.statusquo_person <- QALY.statusquo_person <- list()
QALYgain <- QALYgain_person <- list()

E_cost_notif.screened <- NA
E_QALY_notif.screened <- NA
E_cost_incur <- E_cost_incur_person <- NA
E_QALYgain <- E_QALYgain_person <- NA


# extract tb cases only data

cfr <- purrr::discard(cohort$cfr, is.na)

QALY_statusquo <- purrr::discard(cohort$QALY_statusquo, is.na)
QALY_diseasefree <- purrr::discard(cohort$QALY_diseasefree, is.na)
QALY_cured <- purrr::discard(cohort$QALY_cured, is.na)
QALY_fatality <- purrr::discard(cohort$QALY_fatality, is.na)

uk_notif_discounts <- purrr::discard(cohort$uk_notif_discounts, is.na)
all_notif_discounts <- purrr::discard(cohort$all_notif_discounts, is.na)
uk_secondary_inf_discounts <- purrr::discard(cohort$uk_secondary_inf_discounts, is.na)
all_secondary_inf_discounts <- purrr::discard(cohort$all_secondary_inf_discounts, is.na)


# expected statistics ------------------------------------------------------
# for reproducability and comparison

mean_cost.aTB_TxDx <- means_distributions(unit_cost$aTB_TxDx) %>% sum()
mean_num_sec_inf <- means_distributions(NUM_SECONDARY_INF) %>% unlist()

E_cost_secondary_inf <- mean_num_sec_inf * mean_cost.aTB_TxDx * all_secondary_inf_discounts
E_cost_notif.statusquo <- (all_notif_discounts * mean_cost.aTB_TxDx) + E_cost_secondary_inf

E_QALY_notif.statusquo <- (cfr * QALY_fatality) + ((1 - cfr) * QALY_cured)


for (s in seq_len(n.scenarios)) {

  print(sprintf("[ population model ] scenario: %d", s))

  cost_incur[[s]] <- cost_incur_person[[s]] <- NA   #cost[screen] - cost[statusquo]
  QALYgain[[s]] <- QALYgain_person[[s]]  <- NA   #QALY[screen] - QALY[statusquo]
  QALY.statusquo_person[[s]] <- cost.statusquo_person[[s]] <- NA
  QALY.screened_person[[s]] <- cost.screened_person[[s]] <- NA

  set.seed(12345)
  N.mc <- dectree_res$`1`$call$N.mc

  # QALYs and cost with screening -------------------------------------------

  for (i in seq_len(N.mc)) {

    interv_cost[[s]][[i]] <- scenario_cost(interv,
                                           unit_cost,
                                           NUM_SECONDARY_INF,
                                           uk_secondary_inf_discounts,
                                           num_all_tb_QALY,
                                           uk_notif_discounts,
                                           n.diseasefree.all_tb[[s]][i],
                                           n.diseasefree.uk_tb[[s]][i])

    interv_QALY[[s]][[i]] <- scenario_QALY(avoided = n.diseasefree.all_tb[[s]][i],
                                           total = n_all_tb,
                                           QALY_statusquo,
                                           QALY_diseasefree)
  }

  interv_cost_vs[[s]] <-
    interv_cost[[s]] %>%
    purrr::transpose() %>%
    simplify_all()

  interv_QALY_vs[[s]] <-
    interv_QALY[[s]] %>%
    purrr::transpose() %>%
    simplify_all()

  E_QALY_notif.screened[s] <- sum(scenario_res$p_LTBI_to_effectiveTx[[s]] * QALY_diseasefree +
                                  (1 - scenario_res$p_LTBI_to_effectiveTx[[s]]) * E_QALY_notif.statusquo)

  E_cost_notif.screened[s] <- (1 - scenario_res$p_LTBI_to_effectiveTx[[s]]) * sum(E_cost_notif.statusquo)

  # final cost-effectiveness statistics  ---------------------------------------

  QALY.screened_person[[s]]  <- interv_QALY_vs[[s]]$screened/pop_year
  QALY.statusquo_person[[s]] <- interv_QALY_vs[[s]]$statusquo/pop_year

  # Q1 - Q0: +ve good
  # C1 - C0: +ve bad

  QALYgain[[s]] <- interv_QALY_vs[[s]]$screened - interv_QALY_vs[[s]]$statusquo

  # per person
  QALYgain_person[[s]] <- QALYgain[[s]]/pop_year

  # cost incurred per person for each simulation
  cost.screened_person[[s]]  <- interv_cost_vs[[s]]$screened/pop_year
  cost.statusquo_person[[s]] <- interv_cost_vs[[s]]$statusquo/pop_year

  cost_incur[[s]] <- interv_cost_vs[[s]]$screened - interv_cost_vs[[s]]$statusquo
  cost_incur_person[[s]] <- cost_incur[[s]]/pop_year

  E_cost_incur[s] <- E_cost_notif.screened[s] - sum(E_cost_notif.statusquo)
  E_cost_incur_person[s] <- E_cost_incur[s]/pop_year

  E_QALYgain[s] <- E_QALY_notif.screened[s] - sum(E_QALY_notif.statusquo)
  E_QALYgain_person[s] <- E_QALYgain[s]/pop_year
}


#  save --------------------------------------------------------------------

aTB_CE_stats <- list(QALY.statusquo = map(interv_QALY_vs, "statusquo"),
                     cost.statusquo = map(interv_cost_vs, "statusquo"),
                     QALY.screened = map(interv_QALY_vs, "screened"),
                     cost.screened = map(interv_cost_vs, "screened"),
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

