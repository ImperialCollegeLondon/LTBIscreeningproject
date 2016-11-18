#
# project: LTBI screening
# N Green
# Oct 2016
#
# pre-process Rob's imputed dataset
# impute LTBI status given country of origin
# define entry cohort in terms of LTBI prevalence and size by year


library(dplyr)



##TODO##
# why are there missing issdt uk entry dates?
# for now just remove them...
IMPUTED_sample <- dplyr::filter(IMPUTED_sample, !is.na(issdt))

# eligible screening age range only
IMPUTED_sample <- dplyr::filter(IMPUTED_sample, age_at_entry%in%screen_age_range)


# match active TB prevalence groups in dataset to paper
IMPUTED_sample$who_prev_cat_Pareek2011 <- cut(IMPUTED_sample$who_prevalence,
                                              breaks = c(0, 50, 150, 250, 350, 100000))



# create time-to-events in days --------------------------
# from uk entry to event dates

IMPUTED_sample$issdt <- as.Date(IMPUTED_sample$issdt, '%Y-%m-%d')

# days to arrival in uk from time origin
issdt.asnumeric <- IMPUTED_sample$issdt - as.Date("1960-01-01")


# days from uk entry to active tb
rNotificationDate.asnumeric <- as.Date(IMPUTED_sample$rNotificationDate) - as.Date("1960-01-01")
rNotificationDate_issdt <- rNotificationDate.asnumeric - issdt.asnumeric
rNotificationDate_issdt.years <- as.numeric(IMPUTED_sample$rNotificationDate_issdt)/365


IMPUTED_sample <- data.frame(IMPUTED_sample,
                             rNotificationDate_issdt,
                             rNotificationDate_issdt.years)


rm(issdt.asnumeric,
   rNotificationDate.asnumeric,
   rNotificationDate_issdt)



# yearly entry cohort size by age and prevalence ---------------------------

# total sample size
n.pop <- nrow(IMPUTED_sample)

# keep pre-screened status
IMPUTED_sample$uk_tb_orig <- IMPUTED_sample$uk_tb

# active TB case fatality rate age groups
IMPUTED_sample$cfr_age_groups <- cut(IMPUTED_sample$age_at_entry + rNotificationDate_issdt.years,
                                     breaks = c(15, 45, 65, 200),
                                     right = FALSE)

# extract year only
IMPUTED_sample$issdt_year <- format(IMPUTED_sample$issdt, '%Y')

# single year sample
IMPUTED_sample_year_cohort <- filter(IMPUTED_sample, issdt_year==year_cohort)

# total sample sizes for each yearly cohort
entryCohort_poptotal <- aggregate(rep(1, n.pop),
                                  by = list(IMPUTED_sample$issdt_year), sum)
names(entryCohort_poptotal) <- c("year", "pop")


# logical active TB status of original data
uk_tb_TRUE <- IMPUTED_sample$uk_tb==1


# summary statistics ------------------------------------------------------

# cohort size at arrival to uk
pop_year <- with(entryCohort_poptotal, pop[year==year_cohort])

# number of active TB cases _before_ screening
n.tb <- sum(IMPUTED_sample$uk_tb)
n.tb_year <- sum(IMPUTED_sample_year_cohort$uk_tb)

p.who_year <- prop.table(table(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011))

who_levels <- names(p.who_year)



