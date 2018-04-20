# ***************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB
# random sampling individuals


# data format prep --------------------------------------------------------

# convert from scenario-wise to remain-exit format
scenario_res <-
  dectree_res %>%
  purrr::transpose()

n.scenarios <- length(dectree_res)

n_uk_tb <- unlist(unname(n.uk_tb))
n_exit_tb <- unlist(unname(n.exit_tb))
n_all_tb <- n_exit_tb + n_uk_tb

# Numbers of individuals who avoid getting TB due to screening
avoid_all_tb <-
  scenario_res$n_tb_screen_all %>%
  purrr::map(function(x) dplyr::filter(x, status == "disease-free")) %>%
  map(function(x) x$n)

avoid_uk_tb  <-
  scenario_res$n_tb_screen_uk %>%
  purrr::map(function(x) dplyr::filter(x, status == "disease-free")) %>%
  map(function(x) x$n)

avoid_tb <-
  mapply(function(x,y) cbind(x, y),
         avoid_all_tb,
         avoid_uk_tb,
         SIMPLIFY = FALSE) %>%
  map(`colnames<-`, c('death','exit uk'))

rm(avoid_all_tb,
   avoid_uk_tb)

n.scenarios <- length(dectree_res)
N.mc <- interv$N.mc

interv_cost <- vector(length = N.mc, mode = "list")
interv_QALY <- vector(length = N.mc, mode = "list")
stats_scenario <- vector(length = n.scenarios, mode = "list")

# expected statistics for reproducability/comparison with randomly generated values

mean_cost.aTB_TxDx <-
  unit_cost$aTB_TxDx %>%
  means_distributions() %>%
  sum()

mean_num_sec_inf <-
  NUM_SECONDARY_INF %>%
  means_distributions() %>%
  unlist()

# extract cost-effectiveness variables

costeff_cohort <-
  cohort %>%
  dplyr::filter(all_tb) %>%
  select(cfr,
         uk_tb,
         all_tb,
         QALY_statusquo,
         QALY_diseasefree,
         QALY_cured,
         QALY_fatality,
         uk_notif_discounts,
         all_notif_discounts,
         uk_secondary_inf_discounts,
         all_secondary_inf_discounts,
         id_avoided_tb) %>%
  mutate(E_cost_sec_inf = mean_num_sec_inf * mean_cost.aTB_TxDx * all_secondary_inf_discounts,
         E_cost_statusquo = (all_notif_discounts * mean_cost.aTB_TxDx) + E_cost_sec_inf,
         E_QALY_statusquo = (cfr * QALY_fatality) + ((1 - cfr) * QALY_cured))


########
# main #
########

interv_scenario_cost <- partial(scenario_cost,
                                endpoint = interv$ENDPOINT_cost,
                                unit_cost.aTB_TxDx = unit_cost$aTB_TxDx,
                                num_2nd_inf = NUM_SECONDARY_INF,
                                costeff_cohort = costeff_cohort)

interv_scenario_QALY <- partial(scenario_QALY,
                                endpoint = interv$ENDPOINT_QALY,
                                costeff_cohort = costeff_cohort)

for (s in seq_len(n.scenarios)) {

  message(sprintf("[ population model ] scenario: %s", green(s)))

  for (i in seq_len(N.mc)) {

    # set.seed(12345)

    num_avoided <- avoid_tb[[s]][i, ]

    interv_cost[[i]] <- interv_scenario_cost(num_avoided)
    interv_QALY[[i]] <- interv_scenario_QALY(num_avoided)
  }

  stats_scenario[[s]] <- costeff_stats(scenario_dat = dectree_res[[s]],
                                       interv_QALY = interv_QALY,
                                       interv_cost = interv_cost,
                                       pop_year = pop_year)
}

aTB_CE_stats <-
  stats_scenario %>%
  purrr::transpose()

save(aTB_CE_stats,
     file = pastef(diroutput, "aTB_CE_stats.RData"))

