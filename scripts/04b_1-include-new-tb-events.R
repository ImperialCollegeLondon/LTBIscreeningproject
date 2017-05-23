#
# project: LTBI screening
# N Green
# May 2017
#
# incorporate the imputated events into analysis


num_all_tb_cost <-
  if (ENDPOINT_cost == "exit uk") {
    n.uktb_year
  } else if (ENDPOINT_cost == "death") {
    n.uktb_year + n.exit_tb}

num_all_tb_QALY <-
  if (ENDPOINT_QALY == "exit uk") {
    n.uktb_year
  } else if (ENDPOINT_QALY == "death") {
    n.uktb_year + n.exit_tb}


# combine exit uk and uk tb data ------------------------------------------

##TODO: what about extrapolated in uk???
# uk_tb and uk_tb_orig??

# active TB case fatality rate age groups

IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  mutate(all_tb = uk_tb | exituk_tb,
         all_tb_issdt = ifelse(uk_tb,
                               rNotificationDate_issdt.years,
                               exituk_tb.years),
         uk_death_rNotificationDate = (date_death1_issdt.years - rNotificationDate_issdt.years),
         all_death_rNotificationDate = (date_death1_issdt.years - all_tb_issdt),
         age_exituk_notification = age_at_entry + exituk_tb.years,
         age_all_notification = ifelse(uk_tb,
                                       age_uk_notification,
                                       age_exituk_notification),
         agegroup_all_notification = cut(age_all_notification,
                                         breaks = cfr_age_breaks,
                                         right = FALSE))

# case fatality rate for each active TB case
IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  left_join(cfr_age_lookup,
            by = c("agegroup_all_notification" = "age")) %>%
  select(-distn, -a, -b)


QALY_all_tb <-
  IMPUTED_sample_year_cohort %>%
  subset(all_tb == TRUE) %>%
  with(.,
       calc_QALY_tb(timetoevent = all_death_rNotificationDate,
                    utility.disease_free = utility$disease_free,
                    utility.case = utility$activeTB,
                    age = age_all_notification))

QALY_tb_cured_original <- QALY_all_tb$cured


E_fatality_QALYloss <-
  QALY_all_tb$diseasefree * with(IMPUTED_sample_year_cohort,
                                 cfr[!is.na(cfr)])

QALY_diseasefree <- list()

QALY_diseasefree <-
  IMPUTED_sample_year_cohort %>%
  subset(all_tb == TRUE) %>%
  with(.,
       map2(.x = age_all_notification,
            .y = all_death_rNotificationDate,
            .f = adjusted_life_years,
            start_year = 0,
            end_year = NA,
            utility = utility$disease_free,
            discount_rate = 0.035)) %>%
  map(total_QALYs)

# map_dbl(QALY_diseasefree, 1)

