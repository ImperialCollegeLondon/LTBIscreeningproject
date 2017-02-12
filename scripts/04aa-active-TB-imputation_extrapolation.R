#
# project: LTBI screening
# N Green
# Oct 2016
#
# LTBI to active TB progression including individuals who leave the UK
# estimate (impute or proportionally) number of active TB cases outside of
# UK by year

##TODO: add error to survival estimates


# generate at-risk (LTBI) population

LTBI_status <- sample_uk_tb(prob = 1 - IMPUTED_sample$pLTBI)
IMPUTED_sample_year_cohort$LTBI <- sample_uk_tb(prob = 1 - IMPUTED_sample_year_cohort$pLTBI)

# sensitivity analysis:
# assume _everyone_ has LTBI
# LTBI_status <- rep(1, pop_year)




# estimate active TB transition probabilities -----------------------------

data_etm <- data.frame(id = seq_len(n.pop),
                       from = 9,
                       to = event,
                       time = ceiling(fup_times/365))

data_etm$to[data_etm$to==2] <- 0
data_etm <- data_etm[LTBI_status==1 | IMPUTED_sample$uk_tb_orig==1, ]
data_etm <- data_etm[data_etm$time>0, ]

trans_mat <- mstate::trans.comprisk(K = 2, names = c(1, 3)) %>%
              is.na() %>% not()

res_etm <- etm::etm(data = data_etm,
                    state.names = c(9, 1, 3),
                    cens.name = 0,
                    tra = trans_mat,
                    s = 0)

year_prob.activetb <- diff(c(0, 0, res_etm$est["9","1",]))

# plot(year_prob.activetb,
#      ylim = c(0,0.006), xlim = c(0,50), type = "o")




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



# count number of deaths & active TB cases in each exit uk year subgroup ---------------------

strat_exit_year <- list()

for (yeari in seq_len(exit_max_year)){

  # single year cohort
  # exit in yeari and exit first event (before death, active tb, followup censoring)
  cohort_subset <- IMPUTED_sample_year_cohort %>%
                    dplyr::filter((yeari-1) < date_exit_uk1_issdt.years,
                                  date_exit_uk1_issdt.years < yeari,
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




notifDate_issdt.years <- strat_pop_year["tb", ][!duplicated(strat_pop_year["tb", ])]

activeTBcases <- diff(c(0, notifDate_issdt.years))



# calc number tb_uk (extrapolated) using trans probs  -----------------------------

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
