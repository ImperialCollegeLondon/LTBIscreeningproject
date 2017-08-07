#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#


# coverage: keep only if indiv screened  ------------------------------------

IMPUTED_sample$screen_year <- runif(n = nrow(IMPUTED_sample))*MAX_SCREEN_DELAY

IMPUTED_sample %<>%
  mutate(screen = ifelse(date_death1_issdt.years >= screen_year &
                           date_exit_uk1_issdt.years >= pmax(screen_year, min_screen_length_of_stay) &
                           (rNotificationDate_issdt.years >= screen_year | is.na(rNotificationDate_issdt.years)),
                         1, 0))

if (screen_with_delay) {

  IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                  screen == 1)
}

rm(rNotificationDate_issdt.years)


# remove individuals from 'low' incidence countries
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                who_prev_cat_Pareek2011 %in% incidence_grps_screen)




# single year cohort only -------------------------------------------------

IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample,
                                            issdt_year == year_cohort)


# calc yearly counts for cohort year  -------------------------------------
# active tb, exit uk, death sub-pops

attach(IMPUTED_sample_year_cohort)

strat_pop_year <-
  list(tb = rNotificationDate_issdt.years,
       exit_uk = date_exit_uk1_issdt.years,
       death = date_death1_issdt.years) %>%
  count_comprsk_events()

detach(IMPUTED_sample_year_cohort)


# discount cost and QALYs in decision tree  ---------------------------------
## due to delayed start

prop_screen_year <- ceiling(IMPUTED_sample_year_cohort$screen_year) %>% table %>% prop.table
screen_discount  <- prop_screen_year %*% QALY::discount(t_limit = length(prop_screen_year)) %>% c()


# mdr ---------------------------------------------------------------------

# MDR_burden <- readr::read_csv("C:/Users/ngreen1/Dropbox/TB/LTBI/data/WHO/MDR_RR_TB_burden_estimates_2017-05-30.csv")
#
# sample_mdr <- left_join(IMPUTED_sample_year_cohort[,c("iso_a3_nat","iso_a3_country")],
#                 MDR_burden[,c("iso3","e_rr_pct_new")],
#                 by = c("iso_a3_country" = "iso3"))
#
# mdr_pct <- mean(sample_mdr$e_rr_pct_new)


# summary statistics ------------------------------------------------------

# total sample size
n.pop_screen <- nrow(IMPUTED_sample)

# total sample sizes for each yearly cohort
n.popyear_screen <-
  aggregate(x = rep(1, n.pop_screen),
            by = list(IMPUTED_sample$issdt_year),
            sum) %>%
  set_names(c("year", "pop"))

# year cohort size
pop_year <-
  n.popyear_screen %>%
  dplyr::filter(year == year_cohort) %>%
  dplyr::select(pop) %>%
  as.integer()

# number of active TB cases _without_ screening i.e. status-quo
# n.tb_total <- sum(IMPUTED_sample$uk_tb)
n.uktb_orig <- sum(IMPUTED_sample_year_cohort$uk_tb)

# probability in each who active TB category
p.who_year <-
  table(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011) %>%
  prop.table()

who_levels <- names(p.who_year)

# probability LTBI for each WHO category for year cohort
pLatentTB.who_year <-
  IMPUTED_sample_year_cohort %>%
  dplyr::group_by(who_prev_cat_Pareek2011) %>%
  dplyr::summarise(LTBI = mean(pLTBI)) %>%
  complete(who_prev_cat_Pareek2011,
           fill = list(LTBI = 0))

