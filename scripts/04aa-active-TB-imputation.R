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
# assume everyone has LTBI
# LTBI_status <- rep(1, pop_year)



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
names(cum_incidence.activetb) <- as.character(1:length(cum_incidence.activetb) - 1)


# extrapolate with exponential decay

year_prob.activetb <- prob_from_cum_incidence(cum_incidence.activetb) %>%
                        na.omit()

year_prob.activetb.log <- model.frame(formula = logy ~ year,
                                      data = data.frame(logy = log(year_prob.activetb)[6:9], year = 6:9))
fit.lm <- lm(year_prob.activetb.log)
years <- 10:50
year_prob.activetb_estimated <- exp(years*fit.lm$coefficients["year"] + fit.lm$coefficients["(Intercept)"])

year_prob.activetb <- c(year_prob.activetb, year_prob.activetb_estimated)


# plot(year_prob.activetb, type="o")
# sum(year_prob.activetb)


# sensitivity analysis:
## constant yearly hazard
# year_prob.activetb <- rep(0.001, 100)


##################################################
## calculate absolute number of cases each year ##
##################################################

## annual populations leaving

# extract year only
issdt_exit_year <- ceiling(IMPUTED_sample$date_exit_uk1_issdt/365)[LTBI_status==1 & whoin_year_cohort]

issdt_exit_year.tab <- table(issdt_exit_year)

# 101 is 'never leave UK'; remove
issdt_exit_year.tab <- issdt_exit_year.tab[as.numeric(names(issdt_exit_year.tab))<100]


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



##TODO: test and use in cost-effectivness-QALY-costs.R

# sample of exit individuals who progress to active TB

IMPUTED_sample_exit_tb <- NULL

for (yeari in 1:exit_max_year){

  LTBI_exit_yeari <- IMPUTED_sample[LTBI_status==1 & whoin_year_cohort, ][issdt_exit_year==1, ]

  exit_cohorts_activetb_pop <- rowSums(activetb.exituk[, 1:followup_max_year], na.rm = T)

  who_activetb <- sample(x = 1:nrow(LTBI_exit_yeari),
                         size = exit_cohorts_activetb_pop[yeari],
                         replace = FALSE)

  IMPUTED_sample_exit_tb <- rbind(IMPUTED_sample_exit_tb, LTBI_exit_yeari[who_activetb, ])
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
