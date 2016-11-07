#
# project: LTBI screening
# N Green
# Oct 2016
#
# fit competing risk models to imputed active TB data
# with screening


##TODO##
# extrapolate TB activation past ETS follow-up


library(survival)
library(mstate)
library(cmprsk) # http://www.stat.unipg.it/luca/R


p.complete_treat_scenarios <- read.csv(file = "ext-data/prob_complete_Tx_given_LTBI_by_who.csv", header = FALSE)
names(p.complete_treat_scenarios) <- names(p.complete_Tx_given_LTBI_by_who)

n.pop <- nrow(IMPUTED_sample)


######################################
## simple approach: other events as ##
##       censored (active TB) times ##
######################################


# resample active TB status _after_ screening
# create multiple samples of screened cohort

n.uk_tbX <- 2 #n.mc
uk_tbX_names <- paste("uk_tb", seq_len(n.uk_tbX), sep = "")

uk_tb_scenarios <- as.data.frame(matrix(IMPUTED_sample$uk_tb,
                                        nrow = n.pop,
                                        ncol = n.uk_tbX, byrow = FALSE))

# 3-D output array recycled values
uk_tb_scenarios <- array(uk_tb_scenarios,
                         dimnames = c("uk_tb", "sim", "scenario"),
                         dim = c(n.pop, n.uk_tbX, N.scenarios))


names(uk_tb_scenarios) <- uk_tbX_names
uk_tb_TRUE <- IMPUTED_sample$uk_tb==1

N.scenarios <- nrow(p.complete_treat_scenarios)

# sample updated active TB status for each active TB case
# after screening
uk_tb_after_screen <- function(is.tb, prob){

  n.tb <- sum(is.tb)
  as.numeric(prob[is.tb] < runif(n.tb))
}


# all deterministic decision tree scenarios
for (scenario in seq_len(N.scenarios)){

  # from decision tree get probability of successfully completing LTBI treatment
  # for each cohort individual
  p.complete_treat_sample <- p.complete_treat_given_LTBI_by_who[scenario, IMPUTED_sample$who_prev_cat_Pareek2011]

  # sample new tb status for n.uk_tbX samples
  for (nm in uk_tbX_names){
    uk_tb_scenarios[uk_tb_TRUE, nm, scenario] <- uk_tb_after_screen(uk_tb_TRUE,
                                                                    p.complete_treat_sample)
  }

  # IMPUTED_sample <- data.frame(IMPUTED_sample, scenario)

  # number of active TB cases _after_ screening
  # for each simulated sample
  n.tb_screen[scenario] <- apply(uk_tb_scenarios, 2, table)
}



# fit Kaplan-Meier to _screened_ data -------------------------------------

# redo year split with updated tb events
IMPUTED_sample_splityear_screen <- split(IMPUTED_sample, IMPUTED_sample$issdt_year)
IMPUTED_sample_year_cohort_screen <- IMPUTED_sample_splityear_screen[[year_cohort]]


# screened sample formulas
imputed.form_uk_tb1 <- as.formula(Surv(fup3_issdt, uk_tb1) ~ 1)
imputed_age.form_uk_tb1 <- as.formula(Surv(fup3_issdt, uk_tb1) ~ age_at_entry)


# fit K-M to screened _full_ data
KM_screen_full <- survfit(formula = imputed.form_uk_tb1,
                          data = IMPUTED_sample)
#   stratified by age
KM_screen_full_age <- survfit(formula = imputed_age.form_uk_tb1,
                              data = IMPUTED_sample)

# fit K-M to screened _year_ data
KM_screen_year <- survfit(formula = imputed.form_uk_tb1,
                          data = IMPUTED_sample_year_cohort_screen)
#   stratified by age
KM_screen_year_age <- survfit(formula = imputed_age.form_uk_tb1,
                              data = IMPUTED_sample_year_cohort_screen)



# # alternatively...
# # fit Cox proportional hazards model to _original_ data
# # then predict with _screened_ data
# cens_coxph1_predict_screen <- survfit(formula = cens_coxph1,
#                                         newdata = IMPUTED_sample_year_cohort_screen)



# combine all TB status realisations adding id column
uk_tb_melt <- data.frame(reshape2::melt(x), fup1_issdt = IMPUTED_sample$fup1_issdt)

# survival fit as a function of each active TB set realisation
KM_screen_full_uk_tbX <- survfit(Surv(fup1_issdt, value) ~ variable, data = uk_tb_melt)




######################
## multistate model ##
######################

event_screen <- rep(0, n.pop) #event-free i.e. censored event time
event_screen[IMPUTED_sample$death1] <- 3
event_screen[IMPUTED_sample$exit_uk1] <- 2
event_screen[IMPUTED_sample$uk_tb=="1"] <- 1

dat_screen <- data.frame(times)
dat_screen$age_at_entry <- IMPUTED_sample$age_at_entry

dat_screen$event3 <- as.numeric(event_screen == 3) #death
dat_screen$event2 <- as.numeric(event_screen == 2) #exit_uk
dat_screen$event1 <- as.numeric(event_screen == 1) #uk_tb

# transform to mstate format array
mslong_screen <- msprep(time = c(NA, "times", "times", "times"),
                        status = c(NA, "event1", "event2", "event3"),
                        data = dat_screen, keep = "age_at_entry", trans = tmat)

events(mslong_screen)

# append age to mstate format array
mslong_screen <- expand.covs(mslong_screen, "age_at_entry")

cx_screen <- coxph(Surv(Tstart, Tstop, status) ~ age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
                   data = mslong_screen, method = "breslow")
# cx_screen



cmprsk_age_screen <- data.frame(dis = dat_screen$age_at_entry,
                                ftime = times,
                                status = event_screen)
cmprsk_screen <- data.frame(dis = 1,
                            ftime = times,
                            status = event_screen) # without age


