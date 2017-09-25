#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# subset complete data set
# calculate associated stats


##TODO:
# IMPUTED_sample %>% dplyr::count(who_prev_cat_Pareek2011) %>% mutate(cumsum = rev(cumsum(rev(n))))


# single year cohort only -------------------------------------------------

IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample,
                                            issdt_year == year_cohort)

# coverage: keep only if indiv screened  ------------------------------------

IMPUTED_sample_year_cohort %<>%
  dplyr::mutate(screened_before_exit = date_exit_uk1_issdt.years >= screen_year,
                screened_before_tb = (rNotificationDate_issdt.years >= screen_year) | is.na(rNotificationDate_issdt.years),
                screened_before_death = date_death1_issdt.years >= screen_year,
                stay_longer_than_min = date_exit_uk1_issdt.years >= min_screen_length_of_stay,
                screen = ifelse(screened_before_death &
                                  screened_before_exit &
                                  screened_before_tb &
                                  stay_longer_than_min,
                                yes = 1, no = 0))

if (screen_with_delay) {
  IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample_year_cohort,
                                              screen == 1)}

if (no_students) {
  IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample_year_cohort,
                                              visatype2 != "Students")}


# remove individuals from 'lower' incidence countries
IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample_year_cohort,
                                            who_prev_cat_Pareek2011 %in% incidence_grps_screen)


# discount cost and QALYs in decision tree  ---------------------------------
## due to delayed start

prop_screen_year <- ceiling(IMPUTED_sample_year_cohort$screen_year) %>% table %>% prop.table
screen_discount  <- prop_screen_year %*% QALY::discount(t_limit = length(prop_screen_year)) %>% c()


# summary statistics ------------------------------------------------------

# year cohort size potentially screened
pop_year <- nrow(IMPUTED_sample_year_cohort)

# number of active TB cases _without_ screening i.e. status-quo
n.uktb_orig <- sum(IMPUTED_sample_year_cohort$uk_tb)

# probability cohort in each WHO TB category
p.who_year <-
  table(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011) %>%
  prop.table()


