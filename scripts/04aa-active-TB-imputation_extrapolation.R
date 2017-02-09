#
# project: LTBI screening
# N Green
# Oct 2016
#
# LTBI to active TB progression including individuals who leave the UK
# estimate (impute or proportionally) number of active TB cases outside of
# UK by year


# generate at-risk population
# each year for outside UK

LTBI_status <- sample_uk_tb(prob = 1 - IMPUTED_sample$pLTBI)

# sensitivity analysis:
# assume _everyone_ has LTBI
# LTBI_status <- rep(1, pop_year)



##########################
# yearly sub-populations #
##########################


attach(IMPUTED_sample_year_cohort)

strat_pop_year <- list(tb = rNotificationDate_issdt.years,
                       exit_uk = date_exit_uk1_issdt.years,
                       death = date_death1_issdt.years) %>%
  count_comprsk_events()

detach(IMPUTED_sample_year_cohort)


# par(mfrow=c(2,2))
#
# plot(unlist(strat_pop_year["tb", ]), type = "s", ylim = c(0,500), xlim=c(0,20), ylab="active TB")
# plot(unlist(strat_pop_year["death", ]), type = "s", xlim = c(0,20), ylab = "all-cause death")
# plot(unlist(strat_pop_year["exit_uk", ]), type = "s", xlim = c(0,20), ylab = "exit EWNI")
# plot(unlist(strat_pop_year["remainder", ]), type = "s", xlim = c(0,20), ylab = "remain in EWNI")
#
# knitr::kable(t(strat_pop_year))



########################################
## create (outside uk) times to event ##
########################################

cmprsk <- data.frame(dis = 1,
                     ftime = fup_times,
                     status = event) # without age

# replace exit uk with censored times
cmprsk$status[cmprsk$status==2] <- 0


# not all individuals are in the risk set of progressing to active TB
# we asssume that those without LTBI will never do so
# remove non-LTBI individuals

cmprsk <- cmprsk[LTBI_status==1 | IMPUTED_sample$uk_tb_orig==1, ]


# competing risks of active TB and all-cause death
# cumulative probability
# use total cohort

attach(cmprsk)
fit = CumIncidence(ftime, status, dis, cencode = 0,
                   t = seq(0, 3650, 365), #discrete yearly values
                   xlab = "Days since arrival to UK", col = 2:4)

legend("topleft", legend = c("Active TB", "Death"),
       col = 2:3, lty = 1, bg = "white")
detach(cmprsk)


# include year 0
cum_incidence.activetb <- c(0, fit$est[1, ])
cum_incidence.death <- c(0, fit$est[2, ])

names(cum_incidence.activetb) <- as.character(1:length(cum_incidence.activetb) - 1)
names(cum_incidence.death) <- as.character(1:length(cum_incidence.death) - 1)



# extrapolate with exponential decay --------------------------------------

year_prob.activetb <- prob_from_cum_incidence(cum_incidence_event = cum_incidence.activetb,
                                              cum_incidence_comprisks = list(cum_incidence.death)) %>%
                        na.omit()

max_years_obs <- length(year_prob.activetb)

# constant prob at older ages
# asymptote to non-zero
offset <- 0.001

# only use decreasing section of curve
year_prob.activetb.log <- model.frame(formula = logy ~ year,
                                      data = data.frame(logy = log(year_prob.activetb - offset)[5:max_years_obs],
                                                        year = 5:max_years_obs))
fit.lm <- lm(year_prob.activetb.log)
years <- (max_years_obs + 1):50
year_prob.activetb_estimated <- exp(years*fit.lm$coefficients["year"] + fit.lm$coefficients["(Intercept)"])

# append
year_prob.activetb <- c(year_prob.activetb,
                        year_prob.activetb_estimated + offset)


plot(year_prob.activetb, type = "o")
lines(year_prob.activetb[1:max_years_obs], type = "o", col = 2) #observed
# sum(year_prob.activetb)


# sensitivity analysis:
## constant yearly hazard
#
# year_prob.activetb <- rep(0.001, 100)



##################################################
## calculate absolute number of cases each year ##
##################################################

## annual populations leaving

##TODO: not as good as individual level cos fixed prob LTBI
issdt_exit_year.tab <- (strat_pop_year["exit_uk", ] * 0.3) %>%
                          diff()

issdt_exit_year.tab <- c(strat_pop_year["exit_uk", 1] * 0.3, issdt_exit_year.tab)
issdt_exit_year.tab <- issdt_exit_year.tab[!duplicated(issdt_exit_year.tab)]


exit_max_year <- 10
followup_max_year <- 100

activetb.exituk <- matrix(data = 0, nrow = exit_max_year, ncol = followup_max_year*2)

for (i in seq_len(exit_max_year)){

  pop_exit_in_year_i <- issdt_exit_year.tab[as.character(i)]

  activetb.exituk[i, i] <- pop_exit_in_year_i * year_prob.activetb[i]

  for (j in 1:(followup_max_year-1)){

    activetb.exituk[i, i+j] <- (pop_exit_in_year_i - sum(activetb.exituk[i, ])) * year_prob.activetb[i+j]
  }

}


# sum all curves for each year
exituk_tb_year <- colSums(activetb.exituk)[1:followup_max_year]

n.exit_tb <- sum(exituk_tb_year)



# sample of exit individuals who progress to active TB --------------------

##TODO: test and use in cost-effectivness-QALY-costs.R

IMPUTED_sample_exit_tb <- NULL

for (yeari in 1:exit_max_year){

  LTBI_exit_yeari <- IMPUTED_sample[LTBI_status==1 & whoin_year_cohort, ][issdt_exit_year==yeari, ]

  exit_cohorts_activetb_pop <- rowSums(activetb.exituk[, 1:followup_max_year], na.rm = T)

  who_activetb <- sample(x = 1:nrow(LTBI_exit_yeari),
                         size = exit_cohorts_activetb_pop[yeari],
                         replace = FALSE)

  IMPUTED_sample_exit_tb <- rbind(IMPUTED_sample_exit_tb,
                                  LTBI_exit_yeari[who_activetb, ])
}



# extrapolate within uk exponential decay -----------------------------------


##check are these for non-duplicate active tb cases only n.tb_year=368??

notifDate_issdt.years <- subset(IMPUTED_sample_year_cohort,
                                subset = (uk_tb==1 &
                                          rNotificationDate_issdt.years>=0),
                                select = rNotificationDate_issdt.years) %>%
                          ceiling()

activeTBcases <- table(notifDate_issdt.years)

#
#
#
#
notifDate_issdt.years <- strat_pop_year["tb"][!duplicated(strat_pop_year["tb"])]
##ToDO## test
#####
#
#
#
#
#
#


max_years_obs_uk <- length(activeTBcases)

activeTBcases.log <- model.frame(formula = logy.Freq ~ year,
                                 data = data.frame(logy = log(activeTBcases)[4:max_years_obs_uk], year = 4:max_years_obs_uk))

fit <- lm(activeTBcases.log)

years <- 1:followup_max_year

uktb_estimated <- exp(years*fit$coefficients["year"] + fit$coefficients["(Intercept)"])

names(uktb_estimated) <- as.character(years)

uktb_estimated <- c(rep(0, max_years_obs_uk),
                    uktb_estimated[-(1:max_years_obs_uk)])


# sensitivity analysis:
# constant number of cases after followup
#
# uktb_estimated <- c(rep(0, max_years_obs_uk),
#                     rep(activeTBcases[length(activeTBcases)], followup_max_year))


##TODO: test
##TODO: make dependent on different LTBI probs

# yearly in uk LTBI population
LTBI_pop_year <- strat_pop_year["remainder", ] * pLTBI

num_activeTB_extrap <- NULL

# removing death and active tb from risk set
for (i in seq_along(LTBI_pop_year)){

  num_activeTB_extrap[i] <- year_prob.activetb[i] * LTBI_pop_year[i]
  LTBI_pop_year[i+1] <- LTBI_pop_year[i+1] - num_activeTB_extrap[i]
}






# check populations sizes ---------------------------------------------------
# against expected
#
# # 'life time' risk in UK tb
# n.tb_year/(pop_year*0.3)
#
# # 'life time' risk in UK tb of those 'detectable'
# n.tb_year/(pop_year*0.3*case_detection_rate)
#
# # life time risk in UK and exit tb
# (n.tb_year + n.exit_tb)/(pop_year*0.3)
#
# # life time risk in UK and exit tb of those 'detectable'
# (n.tb_year + n.exit_tb)/(pop_year*0.3*case_detection_rate)
#
# # predicted number of active tb cases notified
# pop_year*0.3*0.1*case_detection_rate
#
