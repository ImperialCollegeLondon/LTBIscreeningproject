#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# subset complete data set
# calculate associated stats


##TODO: ??
# IMPUTED_sample %>%
# dplyr::count(who_prev_cat_Pareek2011) %>%
#   mutate(cumsum = rev(cumsum(rev(n))))


# single year cohort only
cohort <- dplyr::filter(IMPUTED_sample,
                        issdt_year == interv$year_cohort)

# uk stay long enough
cohort <- dplyr::filter(cohort,
                        date_exit_uk1_issdt.years >= interv$min_screen_length_of_stay)

if (interv$screen_with_delay) {
  cohort <- dplyr::filter(cohort,
                          screen == 1)}

if (interv$no_students) {
  cohort <- dplyr::filter(cohort,
                          visatype2 != "Students")}

# remove individuals from 'lower' incidence countries
cohort <- dplyr::filter(cohort,
                        who_prev_cat_Pareek2011 %in% interv$incidence_grps_screen)


cohort$id_avoided_tb[cohort$all_tb] <- {set.seed(111); sample.int(n.all_tb, replace = FALSE)}


save(cohort, file = "data/cohort.RData")


# discount cost and QALYs in decision tree  ---------------------------------
## due to delayed start

prop_screen_year <- ceiling(cohort$screen_year) %>% miscUtilities::prop_table()
screen_discount  <- prop_screen_year %*% QALY::discount(t_limit = length(prop_screen_year)) %>% c()

# year cohort size potentially screened
pop_year <- nrow(cohort)

num_screen_year <- table(ceiling(cohort$screen_year))


# count numbers of tb cases -----------------------------------------------

n.exit_tb <-
  cohort %>%
  dplyr::filter(exituk_tb) %>%
  dplyr::count()

n.uk_tb <-
  cohort %>%
  dplyr::filter(uk_tb) %>%
  dplyr::count()

n.all_tb <- n.uk_tb + n.exit_tb

