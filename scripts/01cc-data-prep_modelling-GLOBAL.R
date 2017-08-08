#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# subset complete data set
# calculate associated stats


# single year cohort only -------------------------------------------------

IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample,
                                            issdt_year == year_cohort)

# coverage: keep only if indiv screened  ------------------------------------

IMPUTED_sample_year_cohort$screen_year <- runif(n = nrow(IMPUTED_sample_year_cohort))*MAX_SCREEN_DELAY

IMPUTED_sample_year_cohort %<>%
  mutate(screen = ifelse(date_death1_issdt.years >= screen_year &
                           date_exit_uk1_issdt.years >= pmax(screen_year, min_screen_length_of_stay) &
                           (rNotificationDate_issdt.years >= screen_year | is.na(rNotificationDate_issdt.years)),
                         1, 0))

if (screen_with_delay) {

  IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample_year_cohort,
                                              screen == 1)
}

rm(rNotificationDate_issdt.years)


# remove individuals from 'low' incidence countries
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

# probability cohort in each who active TB category
p.who_year <-
  table(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011) %>%
  prop.table()


# calc yearly counts for cohort year  -------------------------------------
# active tb, exit uk, death sub-pops

attach(IMPUTED_sample_year_cohort)

strat_pop_year <-
  list(tb = rNotificationDate_issdt.years,
       exit_uk = date_exit_uk1_issdt.years,
       death = date_death1_issdt.years) %>%
  count_comprsk_events()

detach(IMPUTED_sample_year_cohort)

# include a year 0 baseline
strat_pop_year <- cbind(c(0, 0, 0, 0, pop_year),
                        strat_pop_year)

