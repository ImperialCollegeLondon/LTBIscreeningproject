#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# subset complete data set
# calculate associated stats


##TODO: ??
# IMPUTED_sample %>% dplyr::count(who_prev_cat_Pareek2011) %>% mutate(cumsum = rev(cumsum(rev(n))))


# single year cohort only
IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample,
                                            issdt_year == interv$year_cohort)

# uk stay long enough
IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample_year_cohort,
                                            date_exit_uk1_issdt.years >= interv$min_screen_length_of_stay)

if (interv$screen_with_delay) {
  IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample_year_cohort,
                                              screen == 1)}

if (interv$no_students) {
  IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample_year_cohort,
                                              visatype2 != "Students")}

# remove individuals from 'lower' incidence countries
IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample_year_cohort,
                                            who_prev_cat_Pareek2011 %in% interv$incidence_grps_screen)


# discount cost and QALYs in decision tree  ---------------------------------
## due to delayed start

prop_screen_year <- ceiling(IMPUTED_sample_year_cohort$screen_year) %>% miscUtilities::prop_table()
screen_discount  <- prop_screen_year %*% QALY::discount(t_limit = length(prop_screen_year)) %>% c()

# year cohort size potentially screened
pop_year <- nrow(IMPUTED_sample_year_cohort)

num_screen_year <- table(ceiling(IMPUTED_sample_year_cohort$screen_year))


# count numbers of tb cases -----------------------------------------------

n.exit_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(exituk_tb) %>%
  dplyr::count()

n.uk_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(uk_tb) %>%
  dplyr::count()


num_all_tb_cost <-
  if (interv$ENDPOINT_cost == "exit uk") {
    n.uk_tb
  } else if (interv$ENDPOINT_cost == "death") {
    n.uk_tb + n.exit_tb}

num_all_tb_QALY <-
  if (interv$ENDPOINT_QALY == "exit uk") {
    n.uk_tb
  } else if (interv$ENDPOINT_QALY == "death") {
    n.uk_tb + n.exit_tb}

