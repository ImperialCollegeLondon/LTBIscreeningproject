#
# project: LTBI screening
# N Green
# Oct 2016
#
# fit competing risk models to imputed complete dataset with ETS active TB cases
# without screening


##TODO##
# extrapolate TB activation past ETS follow-up


library(survival)
library(mstate)
library(cmprsk) # http://www.stat.unipg.it/luca/R



######################################
## simple approach: other events as ##
##       censored (active TB) times ##
######################################

imputed.form <- as.formula(Surv(fup3_issdt, uk_tb) ~ 1)
imputed_age.form <- as.formula(Surv(fup3_issdt, uk_tb) ~ age_at_entry)


# Cox proportion hazard fits
# to different death and exit uk imputations
# total sample
cens_coxph1 <- coxph(Surv(fup1_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
cens_coxph2 <- coxph(Surv(fup2_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
cens_coxph3 <- coxph(imputed_age.form, data = IMPUTED_sample)

# single year
cens_coxph1_year <- coxph(imputed_age.form, data = IMPUTED_sample_year_cohort)


# fit K-M to original _full_ data
KM_original_full <- survfit(formula = imputed.form,
                            data = IMPUTED_sample)
#   stratified by age
KM_original_full_age <- survfit(formula = imputed_age.form,
                                data = IMPUTED_sample)

# fit K-M to original _year_ data
KM_original_year <- survfit(formula = imputed.form,
                            data = IMPUTED_sample_year_cohort)
#   stratified by age
KM_original_year_age <- survfit(formula = imputed_age.form,
                                data = IMPUTED_sample_year_cohort)



######################
## multistate model ##
######################

# create final state vectors full sample
event <- rep(0, n.pop) #event-free i.e. censored event time
event[IMPUTED_sample$death1] <- 3
event[IMPUTED_sample$exit_uk1] <- 2
event[IMPUTED_sample$uk_tb_orig=="1"] <- 1

times <- IMPUTED_sample$fup1_issdt

# transition matrix
tmat <- trans.comprisk(3, c("event-free", "active_TB", "exit_uk", "dead"))

dat <- data.frame(times)
dat$age_at_entry <- IMPUTED_sample$age_at_entry

dat$event3 <- as.numeric(event == 3) #death
dat$event2 <- as.numeric(event == 2) #exit_uk
dat$event1 <- as.numeric(event == 1) #uk_tb

# transform to mstate format array
mslong <- msprep(time = c(NA, "times", "times", "times"),
                 status = c(NA, "event1", "event2", "event3"),
                 data = dat, keep = "age_at_entry", trans = tmat)
##TODO## could include other (latent) times (not just first) cos we unusually do know these?

# check frequencies
events(mslong)

# append age to mstate format array
mslong <- expand.covs(mslong, "age_at_entry")

cx <- coxph(Surv(Tstart, Tstop, status) ~ age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
            data = mslong, method = "breslow")
cx


# create data structures for cmprsk::
cmprsk_age <- data.frame(dis = dat$age_at_entry,
                         ftime = times,
                         status = event)
cmprsk <- data.frame(dis = 1,
                     ftime = times,
                     status = event) # without age

