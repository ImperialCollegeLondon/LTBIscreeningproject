#
# project: LTBI screening
# N Green
# Oct 2016
#
# fit competing risk models to imputed active TB data

##TODO##
# extrapolate TB activation past ETS follow-up


library(survival)
library(mstate)


# single year sample
IMPUTED_sample_year_cohort <- IMPUTED_sample_splityear[[year_cohort]]

# cohort size at arrival to uk
pop <- sum(entryCohort_poptotal$pop)
pop_year <- with(entryCohort_poptotal, pop[year==year_cohort])

# number of active TB cases _before_ screening
n.tb <- sum(IMPUTED_sample$uk_tb)
n.tb_year <- sum(IMPUTED_sample_year_cohort$uk_tb)

# keep pre-screened status
IMPUTED_sample$uk_tb_orig <- IMPUTED_sample$uk_tb


######################################
## simple approach: other events as ##
##       censored (active TB) times ##
######################################

#  without screening  ------------------------------------------------

imputed.form <- as.formula(Surv(fup3_issdt, uk_tb) ~ 1)
imputed_age.form <- as.formula(Surv(fup3_issdt, uk_tb) ~ age_at_entry)


# Cox proportion hazard fits
cens_coxph1 <- coxph(Surv(fup1_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
cens_coxph2 <- coxph(Surv(fup2_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
cens_coxph3 <- coxph(Surv(fup3_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)

cens_coxph1_year <- coxph(Surv(fup1_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample_year_cohort)


# scaled F=1-S
# cum_activeTB <- pop * (1 - exp(-survfit(formula = cens_coxph3)$cumhaz))


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



# after screening ---------------------------------------------------------

# from decision tree create probability of successfully completing LTBI treatment
IMPUTED_sample$p.complete_treat_given_LTBI_by_who <- p.complete_treat_given_LTBI_by_who[IMPUTED_sample$who_prev_cat_Pareek2011]

# resample tb status after screening
IMPUTED_sample$uk_tb[IMPUTED_sample$uk_tb==1] <- as.numeric(IMPUTED_sample$p.complete_treat_given_LTBI_by_who[IMPUTED_sample$uk_tb==1] < runif(n.tb))

# number of active TB cases _after_ screening
n.tb_screen <- sum(IMPUTED_sample$uk_tb)



# fit Kaplan-Meier to _screened_ data directly

# redo year split with updated tb events
IMPUTED_sample_splityear_screen <- split(IMPUTED_sample, IMPUTED_sample$issdt_year)
IMPUTED_sample_year_cohort_screen <- IMPUTED_sample_splityear_screen[[year_cohort]]

# fit K-M to screened _full_ data
KM_screened_full <- survfit(formula = imputed.form,
                            data = IMPUTED_sample)
#   stratified by age
KM_screened_full_age <- survfit(formula = imputed_age.form,
                                data = IMPUTED_sample)

# fit K-M to screened _year_ data
KM_screened_year <- survfit(formula = imputed.form,
                            data = IMPUTED_sample_year_cohort_screen)
#   stratified by age
KM_screened_year_age <- survfit(formula = imputed_age.form,
                                data = IMPUTED_sample_year_cohort_screen)



# # alternatively...
# # fit Cox proportional hazards model to _original_ data
# # then predict with _screened_ data
# cens_coxph1_predict_screened <- survfit(formula = cens_coxph1,
#                                         newdata = IMPUTED_sample_year_cohort_screen)



######################
## multistate model ##
######################

#  without screening -------------------------------------------------------

# create final state vectors full sample
event <- rep(0, nrow(IMPUTED_sample)) #event-free i.e. censored event time
event[IMPUTED_sample$death1] <- 3
event[IMPUTED_sample$exit_uk1] <- 2
event[IMPUTED_sample$uk_tb_orig=="1"] <- 1

times <- IMPUTED_sample$fup1_issdt

#warning: slow for large sample
# ci <- mstate::Cuminc(time = times, status = event)
# ci.age <- mstate::Cuminc(time = times, status = event, group = IMPUTED_sample$age_at_entry)


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
                 data = dat[1:1000,], keep = "age_at_entry", trans = tmat)
##TODO## could include other (latent) times (not just first) cos we unusually do know these?

# check frequencies
events(mslong)

# append age to mstate formatted array
mslong <- expand.covs(mslong, "age_at_entry")

cx <- coxph(Surv(Tstart, Tstop, status) ~ age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
            data = mslong, method = "breslow")
cx


# screened ----------------------------------------------------------------

event_screen <- rep(0, nrow(IMPUTED_sample)) #event-free i.e. censored event time
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
##TODO## could include other (latent) times (not just first) cos we unusually do know these?

# check frequencies
events(mslong_screen)

# append age to mstate formatted array
mslong_screen <- expand.covs(mslong, "age_at_entry")

cx_screen <- coxph(Surv(Tstart, Tstop, status) ~ age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
                   data = mslong_screen, method = "breslow")
cx_screen





##TODO##
# # prediction
# HvH <- msfit(cx, newdata=mslong, trans=tmat)
# pt <- probtrans(HvH,predt=0)
# pt[[1]] # predictions from state 1



