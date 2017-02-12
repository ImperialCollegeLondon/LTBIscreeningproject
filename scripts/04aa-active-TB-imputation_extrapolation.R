#
# project: LTBI screening
# N Green
# Oct 2016
#
# LTBI to active TB progression including individuals who leave the UK
# estimate (impute or proportionally) number of active TB cases outside of
# UK by year

##TODO: add error to CIF plots and abs numbers


# generate at-risk (LTBI) population

LTBI_status <- sample_uk_tb(prob = 1 - IMPUTED_sample$pLTBI)
IMPUTED_sample_year_cohort$LTBI <- sample_uk_tb(prob = 1 - IMPUTED_sample_year_cohort$pLTBI)

# sensitivity analysis:
# assume _everyone_ has LTBI
# LTBI_status <- rep(1, pop_year)



#  ------------------------------------------------------------------------


data_etm <- data.frame(id = seq_len(n.pop),
                       from = 9,
                       to = event,
                       time = ceiling(fup_times/365))

data_etm$to[data_etm$to==2] <- 0
data_etm <- data_etm[LTBI_status==1 | IMPUTED_sample$uk_tb_orig==1, ]
data_etm <- data_etm[data_etm$time>0, ]

TRA <- mstate::trans.comprisk(K = 2, names = c(1, 3)) %>%
          is.na() %>% not()

res_etm <- etm::etm(data = data_etm,
                    state.names = c(9, 1, 3),
                    cens.name = 0,
                    tra = TRA,
                    s = 0)

year_prob.activetb <- diff(c(0, 0, res_etm$est["9","1",]))

plot(year_prob.activetb, ylim = c(0,0.006), xlim = c(0,50), type = "o")





# fit CIF to complete dataset --------------------------------------

# cmprsk <- data.frame(dis = 1,
#                      ftime = fup_times,
#                      status = event) # without age
#
# # replace exit uk with censored times
# cmprsk$status[cmprsk$status==2] <- 0
#
#
# # not all individuals are in the risk set of progressing to active TB
# # we asssume that those without LTBI will never do so
# # remove non-LTBI individuals
#
# cmprsk <- cmprsk[LTBI_status==1 | IMPUTED_sample$uk_tb_orig==1, ]

# competing risks of active TB and all-cause death
# cumulative probability
# use total cohort

# attach(cmprsk)
# fit = CumIncidence(ftime, status, dis, cencode = 0,
#                    t = seq(0, 3650, 365), #discrete yearly values
#                    xlab = "Days since arrival to UK", col = 2:4)
#
# legend("topleft", legend = c("Active TB", "Death"),
#        col = 2:3, lty = 1, bg = "white")
# detach(cmprsk)
#
#
# # include year 0
# cum_incidence.activetb <- c(0, fit$est[1, ])
# cum_incidence.death <- c(0, fit$est[2, ])
#
# names(cum_incidence.activetb) <- as.character(1:length(cum_incidence.activetb) - 1)
# names(cum_incidence.death) <- as.character(1:length(cum_incidence.death) - 1)
#
#
#
# year_prob.activetb <- prob_from_cum_incidence(cum_incidence_event = cum_incidence.activetb,
#                                               cum_incidence_comprisks = list(cum_incidence.death)) %>%
#                         na.omit()




# fit exponential distn to trans probs & extrapolate  ----------------------------

fup_max_year <- 100

max_years_obs <- length(year_prob.activetb)

# constant prob at older ages
# asymptote to non-zero
offset <- 0.001

# only use decreasing section of curve
year_prob.activetb.log <- model.frame(formula = logy ~ year,
                                      data = data.frame(logy = log(year_prob.activetb - offset)[5:max_years_obs],
                                                        year = 5:max_years_obs))
fit.lm <- lm(year_prob.activetb.log)
years <- (max_years_obs + 1):fup_max_year
year_prob.activetb_estimated <- exp(years*fit.lm$coefficients["year"] + fit.lm$coefficients["(Intercept)"])

# append
year_prob.activetb <- c(year_prob.activetb,
                        year_prob.activetb_estimated + offset)


plot(year_prob.activetb, type = "o", xlab = "Year", ylab = "Probability")
lines(year_prob.activetb[1:max_years_obs], type = "o", col = 2) #observed
abline(h = 0.001, col = "blue")
segments(max_years_obs, year_prob.activetb[max_years_obs], fup_max_year, year_prob.activetb[max_years_obs], col = "green")


# 1 - exp(-sum(year_prob.activetb))


# sensitivity analysis:
## constant yearly hazard
#
# year_prob.activetb <- rep(0.001, 100)




# calc number of LTBI each year in exit uk pop  ---------------------------

# each year sub-populations leaving

##TODO: not quite as good as individual level calc cos fixed prob LTBI

LTBI_ukexit_year_pop <- (strat_pop_year["exit_uk", ] * 0.3) %>%
                          diff()

LTBI_ukexit_year_pop <- c(strat_pop_year["exit_uk", 1] * 0.3, LTBI_ukexit_year_pop)
LTBI_ukexit_year_pop <- LTBI_ukexit_year_pop[!duplicated(LTBI_ukexit_year_pop)]

# include year 0 so consistent with year_prob.activetb
LTBI_ukexit_year_pop <- c(0, LTBI_ukexit_year_pop)

exit_max_year <- 10

activetb.exit <- matrix(data = 0,
                        nrow = exit_max_year,
                        ncol = fup_max_year*2)



# simulate active tb progression times for exit uk individuals ----------------------


sample_tb <- function(p) sample(c("tb", "disease-free"), size = 1, prob = c(p, 1-p))

for (i in seq_len(pop_year)){

  # LTBI free
  if(IMPUTED_sample_year_cohort$LTBI[i]==0){

    IMPUTED_sample_year_cohort$exituk_tb_year[i] <- Inf
  }else{

    # sample if active TB each year
    tb_year <- sapply(year_prob.activetb, sample_tb) %>%
                equals("tb") %>%
                which()

    # remove time if progress before exit uk
    tb_year[tb_year<IMPUTED_sample_year_cohort$date_exit_uk1_issdt.years[i]] <- NA

    # if multiple take first occurence
    IMPUTED_sample_year_cohort$exituk_tb_year[i] <- min(tb_year, na.rm = TRUE) %>%
                                                      suppressWarnings()
  }
}



# count number of deaths, tb cases in each exit uk year subgroup ---------------------


strat_exit_year <- list()

for (yeari in seq_len(exit_max_year)){

  # single year cohort
  # exit in yeari and exit first event (before death, active tb, followup censoring)
  cohort_subset <- IMPUTED_sample_year_cohort %>%
                    dplyr::filter((yeari-1)<date_exit_uk1_issdt.years,
                                  date_exit_uk1_issdt.years<yeari,
                                  exit_uk1==TRUE)

  strat_exit_year[[yeari]] <- list(tb = cohort_subset$exituk_tb_year,
                                   death = cohort_subset$date_death1_issdt.years) %>%
                                count_comprsk_events()
}



# # sum across all curves for each year
# exituk_tb_year <- colSums(activetb.exit)[1:fup_max_year]
#
# n.exit_tb <- sum(exituk_tb_year, na.rm = TRUE)


# plot(exituk_tb_year, type = "o")



# fit exp distn to _number_ of active TB & extrapolate tb_uk  -------------------------

## depricated by using CIF instead ##

notifDate_issdt.years <- strat_pop_year["tb", ][!duplicated(strat_pop_year["tb", ])]

activeTBcases <- diff(c(0, notifDate_issdt.years))

#
# max_years_obs_uk <- length(activeTBcases)
#
# activeTBcases.log <- model.frame(formula = logy ~ year,
#                                  data = data.frame(logy = log(activeTBcases)[4:max_years_obs_uk], year = 4:max_years_obs_uk))
#
# fit <- lm(activeTBcases.log)
#
# years <- 1:followup_max_year
#
# uktb_estimated <- exp(years*fit$coefficients["year"] + fit$coefficients["(Intercept)"])
#
# names(uktb_estimated) <- as.character(years)
#
# uktb_estimated <- c(rep(0, max_years_obs_uk),
#                     uktb_estimated[-(1:max_years_obs_uk)])


# sensitivity analysis:
# constant number of cases after followup
#
# uktb_estimated <- c(rep(0, max_years_obs_uk),
#                     rep(activeTBcases[length(activeTBcases)], followup_max_year))



# calc number tb_uk (extrapolated) using CIF ----------------------------------

##TODO: make dependent on different LTBI probs

# yearly in uk LTBI population
LTBI_pop_year <- strat_pop_year["remainder", ] * 0.3

num_activeTB_extrap <- NULL

for (i in seq_along(LTBI_pop_year)){

  num_activeTB_extrap[i] <- year_prob.activetb[i+1] * LTBI_pop_year[i]

  # removing active tb from risk set
  LTBI_pop_year[i+1] <- LTBI_pop_year[i+1] - num_activeTB_extrap[i]
}


# plot(num_activeTB_extrap[1:20], type = "o", ylim = c(0,85),
#      xlab = "Year", ylab = "Number of active TB cases")
# lines(activeTBcases, type="o", col="red")


# use estimated active tb numbers rather than observed sample
activeTBcases <- num_activeTB_extrap



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
