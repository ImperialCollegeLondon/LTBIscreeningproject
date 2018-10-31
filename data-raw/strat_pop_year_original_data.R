
#' ---
#' title: "LTBI screening model:
#'  generate event count table for original data set"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


# Campbell et al (2017) PLOS one optimise to fit model parameter values, including the probability of LTBI in their cohort.
# We take this for different WHO incidence categories from Pareek ()
# However, we can check that these values are appropriate for our data by doing the following.


# Using the original cleaned data we can create a table of event counts and year of occurrence.

library(LTBIscreeningproject)
library(dplyr)
library(reshape2)

load(file = here::here("data", "sample_cleaned.RData"))

event_times <- list(tb = IMPUTED_sample$notif_issdt.years,
                    fup = IMPUTED_sample$fup_issdt,
                    exit_uk = IMPUTED_sample$date_exit_uk1_issdt.years,
                    death = IMPUTED_sample$date_death1_issdt.years)

strat_pop_year <- count_comprsk_events(event_times)


# Furthermore, we can repeat this but stratify by country of origin.
# If we assume that the progression probability from LTBI to active TB between groups is the same
# then the difference between the different group is attributable to the difference in LTBI prevalence
# in the groups

# imputed proportions from Pareek () are
prop.table(table(IMPUTED_sample$who_inc_Pareek2011,
                 IMPUTED_sample$pLTBI))


# Create the tables

event_times <- list(tb = IMPUTED_sample$notif_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(150,250]"],
                    fup = IMPUTED_sample$fup_issdt[IMPUTED_sample$who_inc_Pareek2011 == "(150,250]"],
                    exit_uk = IMPUTED_sample$date_exit_uk1_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(150,250]"],
                    death = IMPUTED_sample$date_death1_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(150,250]"])

strat_pop_year150 <- count_comprsk_events(event_times)


event_times <- list(tb = IMPUTED_sample$notif_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(250,350]"],
                    fup = IMPUTED_sample$fup_issdt[IMPUTED_sample$who_inc_Pareek2011 == "(250,350]"],
                    exit_uk = IMPUTED_sample$date_exit_uk1_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(250,350]"],
                    death = IMPUTED_sample$date_death1_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(250,350]"])

strat_pop_year250 <- count_comprsk_events(event_times)


event_times <- list(tb = IMPUTED_sample$notif_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(350,1e+05]"],
                    fup = IMPUTED_sample$fup_issdt[IMPUTED_sample$who_inc_Pareek2011 == "(350,1e+05]"],
                    exit_uk = IMPUTED_sample$date_exit_uk1_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(350,1e+05]"],
                    death = IMPUTED_sample$date_death1_issdt.years[IMPUTED_sample$who_inc_Pareek2011 == "(350,1e+05]"])

strat_pop_year350 <- count_comprsk_events(event_times)


# We would expect the cumulative incidence plots to have a constant proportional difference over time

plot(cumsum(strat_pop_year150$incid_rate), type = "o", ylim = c(0,2000),
     xlab = "year", ylab = "cumulative incidence")
lines(cumsum(strat_pop_year250$incid_rate), type = "o")
lines(cumsum(strat_pop_year350$incid_rate), type = "o")


# Recall that the Pareek () values are

unique(IMPUTED_sample$pLTBI)

# So we would expect the 3rd group to be 50% larger than the 2nd group
# i.e. from 20% to 30%

# One way to check this is to fit a linear regression to the two curves and compare the gradients

lm150 <- lm(y ~ x - 1,
   data.frame(x = 1:nrow(strat_pop_year150),
              y = cumsum(strat_pop_year150$incid_rate)))


lm250 <- lm(y ~ x - 1,
   data.frame(x = 1:nrow(strat_pop_year250),
              y = cumsum(strat_pop_year250$incid_rate)))

# We find that the estimate probability of LTBI is close to 20%
round(lm150$coefficients/lm250$coefficients * 0.3, 2)

# Similarly, we can take the total incidence instead.
# We see that this is also consistent with the Pareek () estimates used in our model.

round((cumsum(strat_pop_year150$incid_rate)/cumsum(strat_pop_year250$incid_rate))[8] * 0.3, 2)
