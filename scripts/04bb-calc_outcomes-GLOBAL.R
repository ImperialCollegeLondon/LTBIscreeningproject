#***************************************************************
# project: LTBI screening
# N Green
# May 2017
#
# calculate outcomes
#   - numbers of active TB cases
#   - QALYs per person for each outcome (death, avoided, cured)


n.exit_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(exituk_tb) %>%
  dplyr::count()

n.uk_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(uk_tb) %>%
  dplyr::count()


num_all_tb_cost <-
  if (ENDPOINT_cost == "exit uk") {
    n.uk_tb
  } else if (ENDPOINT_cost == "death") {
    n.uk_tb + n.exit_tb}

num_all_tb_QALY <-
  if (ENDPOINT_QALY == "exit uk") {
    n.uk_tb
  } else if (ENDPOINT_QALY == "death") {
    n.uk_tb + n.exit_tb}


# calculate QALYs for all tb cases for all situations
# so can sample later
QALY_all_tb <-
  IMPUTED_sample_year_cohort %>%
  subset(all_tb == TRUE) %$%
  calc_QALY_tb(timetoevent = all_death_rNotificationDate,
               utility.disease_free = utility$disease_free,
               utility.case = utility$activeTB,
               age = age_all_notification)

E_fatalities <- with(IMPUTED_sample_year_cohort,
                     cfr[!is.na(cfr)])

E_total_fatalities <- sum(E_fatalities)

E_fatality_QALYloss <- QALY_all_tb$diseasefree * E_fatalities

E_total_fatality_QALYloss <- sum(E_fatality_QALYloss)


# adjusted_life_years type object equivalent calc
# useful for plotting...

QALY_diseasefree <- list()

QALY_diseasefree <-
  IMPUTED_sample_year_cohort %>%
  subset(all_tb == TRUE) %$%
       map2(.x = age_all_notification,
            .y = all_death_rNotificationDate,
            .f = QALY::adjusted_life_years,
            start_year = 0,
            end_year = NA,
            utility = utility$disease_free,
            discount_rate = 0.035) %>%
  map(QALY::total_QALYs)

