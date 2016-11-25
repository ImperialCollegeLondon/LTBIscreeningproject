l#
# project: LTBI screening
# N Green
# Oct 2016
#
# pre-process Rob's imputed dataset
# impute LTBI status given country of origin
# define entry cohort in terms of LTBI prevalence and size by year


library(dplyr)
library(tidyr)


##TODO##
# why are there missing issdt uk entry dates?
# for now just remove them...
IMPUTED_sample <- dplyr::filter(IMPUTED_sample, !is.na(issdt))

# eligible screening age range only
IMPUTED_sample <- dplyr::filter(IMPUTED_sample, age_at_entry%in%screen_age_range)


# match active TB prevalence groups in dataset to paper
IMPUTED_sample$who_prev_cat_Pareek2011 <- cut(IMPUTED_sample$who_prevalence,
                                              breaks = c(0, 50, 150, 250, 350, 100000))



# LTBI probabilities by who active TB prevalence group --------------------

# ref. Pareek M, Watson JP, Ormerod LP, Kon OM, Woltmann G, White PJ, et al. Lancet Infect Dis. Elsevier Ltd; 2011;11(6)
# ages 18-35 pooled
pLatentTB.who <- c(0.03, 0.13, 0.2, 0.3, 0.3)

# age-dependent prob of LTBI
pLatentTB.who_18to35 <- matrix(pLatentTB.who, ncol = 18, nrow = length(pLatentTB.who))

# ref. Lancet, tuberculosis infection in rural China: baseline results of a population-based, multicentre, prospective cohort study
# 20_to_35/36_to_45 years old => 16%/10%
pLatentTB.who_36to45 <- matrix(pLatentTB.who*1.65, ncol = 10, nrow = length(pLatentTB.who))

pLatentTB.who_age <- data.frame(levels(IMPUTED_sample$who_prev_cat_Pareek2011),
                                pLatentTB.who_18to35,
                                pLatentTB.who_36to45)
colnames(pLatentTB.who_age) <- c("who_prev_cat_Pareek2011", as.character(18:45))

detach(package:reshape)


pLatentTB.who_age.long <- reshape2:::melt(pLatentTB.who_age, id.vars = "who_prev_cat_Pareek2011", value.name = "pLTBI", variable.name = "age_at_entry")
IMPUTED_sample <- merge(IMPUTED_sample, pLatentTB.who_age.long, by = c("age_at_entry", "who_prev_cat_Pareek2011"))


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

# probability in each who active TB category
p.who_year <- prop.table(table(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011))

who_levels <- names(p.who_year)

# probability LTBI for each who category for year cohort
IMPUTED_sample_year_cohort %>%
  group_by(who_prev_cat_Pareek2011) %>%
  summarise(LTBI = mean(pLTBI)) %>%
  complete(who_prev_cat_Pareek2011, fill = list(LTBI = 0))


