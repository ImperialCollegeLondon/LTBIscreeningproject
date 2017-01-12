#
# project: LTBI screening
# N Green
# Oct 2016
#
# pre-process R Aldridge, Lancet (2016) imputed dataset


library(dplyr)
library(tidyr)
library(purrr)


IMPUTED_sample <- IMPUTED_IOM_ETS_WHO_merged_15_2_9
rm(IMPUTED_IOM_ETS_WHO_merged_15_2_9)


##TODO##
# why are there missing issdt uk entry dates?
# for now just remove them...
IMPUTED_sample <- dplyr::filter(IMPUTED_sample, !is.na(issdt))

# eligible screening age range only
IMPUTED_sample <- dplyr::filter(IMPUTED_sample, age_at_entry%in%screen_age_range)


# match active TB prevalence groups in dataset to Pareek (2011)
IMPUTED_sample$who_prev_cat_Pareek2011 <- cut(IMPUTED_sample$who_prevalence,
                                              breaks = c(0, 50, 150, 250, 350, 100000))



# LTBI probabilities by who active TB prevalence group --------------------

# ref. Pareek M, Watson JP, Ormerod LP, Kon OM, Woltmann G, White PJ, et al. Lancet Infect Dis. Elsevier Ltd; 2011;11(6)
# ages 18-35 pooled
pLatentTB.who <- c(0.03, 0.13, 0.2, 0.3, 0.3)


### adjust 36-45 by scaling factor ###

# # age-dependent prob of LTBI
# pLatentTB.who_18to35 <- matrix(data = pLatentTB.who,
#                                ncol = 18,
#                                nrow = length(pLatentTB.who))
#
# # ref. Lancet, tuberculosis infection in rural China: baseline results of a population-based, multicentre, prospective cohort study
# # 36_to_45/20_to_35 years old => 16%/10%
#
# ##TODO## check this value!!
#
# pLatentTB.who_36to45 <- matrix(data = pLatentTB.who*1.65,
#                                ncol = 10,
#                                nrow = length(pLatentTB.who))
#
# pLatentTB.who_age <- data.frame(levels(IMPUTED_sample$who_prev_cat_Pareek2011),
#                                 pLatentTB.who_18to35,
#                                 pLatentTB.who_36to45)

### or
### assume >35 == 35 year olds ###
# i.e. age independent

pLatentTB.who_18to45 <- matrix(data = pLatentTB.who,
                               ncol = 28,
                               nrow = length(pLatentTB.who))

pLatentTB.who_age <- data.frame(levels(IMPUTED_sample$who_prev_cat_Pareek2011),
                                pLatentTB.who_18to45)


colnames(pLatentTB.who_age) <- c("who_prev_cat_Pareek2011", as.character(18:45))

rm(pLatentTB.who_18to35, pLatentTB.who_36to45, pLatentTB.who_18to45)


# join with main data set

if("reshape"%in%loadedNamespaces()) detach(package:reshape)

pLatentTB.who_age.long <- reshape2:::melt(data = pLatentTB.who_age,
                                          id.vars = "who_prev_cat_Pareek2011",
                                          value.name = "pLTBI",
                                          variable.name = "age_at_entry")

IMPUTED_sample <- merge(x = IMPUTED_sample,
                        y = pLatentTB.who_age.long,
                        by = c("age_at_entry", "who_prev_cat_Pareek2011"))



# create time-to-events in days --------------------------
# from uk entry to event dates

IMPUTED_sample$issdt <- as.Date(IMPUTED_sample$issdt, '%Y-%m-%d')

# days to arrival in uk from time origin
issdt.asnumeric <- IMPUTED_sample$issdt - as.Date("1960-01-01")


# days from uk entry to active tb
rNotificationDate.asnumeric <- as.Date(IMPUTED_sample$rNotificationDate) - as.Date("1960-01-01")
rNotificationDate_issdt <- rNotificationDate.asnumeric - issdt.asnumeric
rNotificationDate_issdt.years <- as.numeric(rNotificationDate_issdt)/365


# days from uk entry to all-cause death
date_death1.asnumeric <- as.Date(IMPUTED_sample$date_death1) - as.Date("1960-01-01")
date_death1_issdt <- date_death1.asnumeric - issdt.asnumeric
date_death1_issdt.years <- as.numeric(date_death1_issdt)/365


# days from uk entry to all-cause death
date_exit_uk1.asnumeric <- as.Date(IMPUTED_sample$date_exit_uk1) - as.Date("1960-01-01")
date_exit_uk1_issdt <- date_exit_uk1.asnumeric - issdt.asnumeric
date_exit_uk1_issdt.years <- as.numeric(date_exit_uk1_issdt)/365


IMPUTED_sample <- data.frame(IMPUTED_sample,
                             rNotificationDate_issdt,
                             rNotificationDate_issdt.years,
                             date_death1_issdt,
                             date_death1_issdt.years,
                             date_exit_uk1_issdt,
                             date_exit_uk1_issdt.years)

rm(issdt.asnumeric,
   rNotificationDate.asnumeric,
   rNotificationDate_issdt,
   rNotificationDate_issdt.years,
   date_death1_issdt,
   date_death1.asnumeric,
   date_death1_issdt.years,
   date_exit_uk1_issdt,
   date_exit_uk1.asnumeric,
   date_exit_uk1_issdt.years)


# yearly entry cohort size by age and prevalence ---------------------------

# total sample size
n.pop <- nrow(IMPUTED_sample)

# keep status-quo TB status
IMPUTED_sample$uk_tb_orig <- IMPUTED_sample$uk_tb

# active TB case fatality rate age groups
age_at_Notification <- IMPUTED_sample$age_at_entry + rNotificationDate_issdt.years
IMPUTED_sample$cfr_age_groups <- cut(age_at_Notification,
                                     breaks = cfr_age_breaks,
                                     right = FALSE)
rm(age_at_Notification)

# extract uk entry year only
IMPUTED_sample$issdt_year <- format(IMPUTED_sample$issdt, '%Y')

# single year cohort only
IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample, issdt_year==year_cohort)

# is record in chosen cohort logical
whoin_year_cohort <- IMPUTED_sample$issdt_year==year_cohort

# total sample sizes for each yearly cohort
entryCohort_poptotal <- aggregate(x = rep(1, n.pop),
                                  by = list(IMPUTED_sample$issdt_year), sum) %>%
                        set_names(c("year", "pop"))


# logical active TB status of original data
uk_tb_TRUE <- IMPUTED_sample$uk_tb==1


# summary statistics ------------------------------------------------------

# cohort size at arrival to uk
pop_year <- with(entryCohort_poptotal, pop[year==year_cohort])

# number of active TB cases _before_ screening i.e. status-quo
n.tb <- sum(IMPUTED_sample$uk_tb)
n.tb_year <- sum(IMPUTED_sample_year_cohort$uk_tb)

# probability in each who active TB category
p.who_year <- prop.table(table(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011))

who_levels <- names(p.who_year)

# probability LTBI for each who category for year cohort
pLatentTB.who_year <- IMPUTED_sample_year_cohort %>%
                        dplyr::group_by(who_prev_cat_Pareek2011) %>%
                        summarise(LTBI = mean(pLTBI)) %>%
                        complete(who_prev_cat_Pareek2011, fill = list(LTBI = 0))

