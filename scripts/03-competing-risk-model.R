#
# project: LTBI screening
# N Green
# Oct 2016
#
# fit a competing risk model to imputed active TB data


library(survival)
library(mstate)


####################
## COMPETING RISK ##
####################

# simple approach: other events as censored (active TB) times ----------

cens_coxph1 <- coxph(Surv(X_1_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
cens_coxph2 <- coxph(Surv(X_2_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
cens_coxph3 <- coxph(Surv(X_3_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)


# cohort size at arrival to uk
pop <- with(entryCohort_poptotal, pop[year==year_cohort])

# scaled F=1-S
cum_activeTB <- pop * (1 - exp(-survfit(formula = cens_coxph3)$cumhaz))

# fit stratified by age
cens_survfit_byage <- survfit(Surv(X_3_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)


# after screening ---------------------------------------------------------

# resample_tb_status_after_screening
n.tb <- sum(IMPUTED_sample$uk_tb)
IMPUTED_sample$uk_tb_orig <- IMPUTED_sample$uk_tb
IMPUTED_sample$uk_tb[IMPUTED_sample$uk_tb==1] <- as.numeric(!(p.LTBI_to_nonLTBI > runif(n.tb)))


# fit Kaplan-Meier to screened data
IMPUTED_sample_splityear <- split(IMPUTED_sample, IMPUTED_sample$issdt_year)
KM_screened <- survfit(Surv(X_9_fup_issdt, uk_tb) ~ age_at_entry,
                       data = IMPUTED_sample_splityear[[year_cohort]])


# fit Cox proportional hazards model to original data
# then predict with screened data
cens_coxph1_predict_screened <- survfit(cens_coxph1,
                                        newdata = IMPUTED_sample_splityear[[year_cohort]])


# multistate model --------------------------------------------------------

##TODO##
# need leave_uk and death times from STATA model...

IMPUTED_LTBI <- transform(IMPUTED_LTBI,
                          leave_uk = length_uk_stay <= `X_9_fup_issdt` & uk_tb==0)

# create final state vector
event <- rep(0, nrow(IMPUTED_LTBI)) #event-free i.e. censored event time
# event <- rep(1, nrow(IMPUTED_LTBI)) #death
event[IMPUTED_LTBI$leave_uk] <- 2
event[IMPUTED_LTBI$uk_tb=="1"] <- 3

times <- IMPUTED_LTBI$`X_9_fup_issdt`
times[event==2] <- IMPUTED_LTBI$length_uk_stay[event==2]
times[event==3] <- IMPUTED_LTBI$time_screen_case[event==3]


ci <- mstate::Cuminc(time = times, status = event)
ci.age <- mstate::Cuminc(time = times, status = event, group = IMPUTED_LTBI$age_at_entry)


# transition matrix
tmat <- trans.comprisk(3, c("event-free", "emmigrate", "dead", "active_TB"))

dat <- data.frame(times)
dat$age_at_entry <- IMPUTED_LTBI$age_at_entry

dat$event1 <- as.numeric(event == 1) #death
dat$event2 <- as.numeric(event == 2) #leave_uk
dat$event3 <- as.numeric(event == 3) #uk_tb

# transform to mstate format array
mslong <- msprep(time = c(NA, "times", "times", "times"),
                 status = c(NA, "event1", "event2", "event3"),
                 data = dat, keep = "age_at_entry", trans = tmat)
## could include other (latent) times (not just first) cos we unusually do know these. so more status =1??

# check frequencies
events(mslong)

# append age to mstate formatted array
mslong <- expand.covs(mslong, "age_at_entry")

cx <- coxph(Surv(Tstart, Tstop, status) ~ age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
            data = mslong, method = "breslow")
cx


##TODO##
# HvH <- msfit(cx, newdate=, trans=tmat)
# pt <- probtrans(HvH,predt=0)
# pt[[1]] # predictions from state 1



