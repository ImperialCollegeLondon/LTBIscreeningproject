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
library(cmprsk) # http://www.stat.unipg.it/luca/R



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
cens_coxph3 <- coxph(imputed_age.form, data = IMPUTED_sample)

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



# after screening ---------------------------------------------------------

# from decision tree get probability of successfully completing LTBI treatment
# for each cohort individual
p.complete_treat <- p.complete_treat_given_LTBI_by_who[IMPUTED_sample$who_prev_cat_Pareek2011]

# resample active TB status _after_ screening
# create multiple samples of screened cohort
n.uk_tbX <- 2
uk_tbX_names <- paste("uk_tb", seq_len(n.uk_tbX), sep="")

x <- as.data.frame(matrix(IMPUTED_sample$uk_tb,
                          nrow = nrow(IMPUTED_sample),
                          ncol = n.uk_tbX, byrow = FALSE))

names(x) <- uk_tbX_names

uk_tb_TRUE <- IMPUTED_sample$uk_tb==1


# sample updated active TB status for each active TB case
# after screening

uk_tb_after_screen <- function(uk_tb_TRUE,
                               p.complete_treat){
  n.tb <- sum(uk_tb_TRUE)
  as.numeric(p.complete_treat[uk_tb_TRUE] < runif(n.tb))
}

for (nm in names(x)){
  x[uk_tb_TRUE, nm] <- uk_tb_after_screen(uk_tb_TRUE, p.complete_treat)
}

IMPUTED_sample <- data.frame(IMPUTED_sample, x)

# number of active TB cases _after_ screening
n.tb_screen <- apply(x, 2, table)


# individuals who have changed from tb to non-tb
# are now censored times
IMPUTED_sample <- transform(IMPUTED_sample,
                            cens1_screen  = cens1  | (uk_tb==0 & uk_tb_orig==1),
                            cens2_screen  = cens2  | (uk_tb==0 & uk_tb_orig==1),
                            cens3_screen  = cens3  | (uk_tb==0 & uk_tb_orig==1),
                            cens4_screen  = cens4  | (uk_tb==0 & uk_tb_orig==1),
                            cens5_screen  = cens5  | (uk_tb==0 & uk_tb_orig==1),
                            cens6_screen  = cens6  | (uk_tb==0 & uk_tb_orig==1),
                            cens7_screen  = cens7  | (uk_tb==0 & uk_tb_orig==1),
                            cens8_screen  = cens8  | (uk_tb==0 & uk_tb_orig==1),
                            cens9_screen  = cens9  | (uk_tb==0 & uk_tb_orig==1),
                            cens10_screen = cens10 | (uk_tb==0 & uk_tb_orig==1))
##TODO##
# use followup and imputed death, leave uk times



# fit Kaplan-Meier to _screened_ data directly

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
xx <- data.frame(reshape2::melt(x), fup1_issdt = IMPUTED_sample$fup1_issdt)

# survival fit as a function of each active TB set realisation
KM_screen_full_uk_tbX <- survfit(Surv(fup1_issdt, value) ~ variable, data = xx)




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
                 data = dat, keep = "age_at_entry", trans = tmat)
##TODO## could include other (latent) times (not just first) cos we unusually do know these?

# check frequencies
events(mslong)

# append age to mstate format array
mslong <- expand.covs(mslong, "age_at_entry")

cx <- coxph(Surv(Tstart, Tstop, status) ~ age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
            data = mslong, method = "breslow")
cx



cmprsk_age <- data.frame(dis = dat$age_at_entry,
                         ftime = times,
                         status = event)
cmprsk <- data.frame(dis = 1,
                     ftime = times,
                     status = event) # without age



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

events(mslong_screen)

# append age to mstate format array
mslong_screen <- expand.covs(mslong_screen, "age_at_entry")

cx_screen <- coxph(Surv(Tstart, Tstop, status) ~ age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
                   data = mslong_screen, method = "breslow")
cx_screen



cmprsk_age_screen <- data.frame(dis = dat_screen$age_at_entry,
                                ftime = times,
                                status = event_screen)
cmprsk_screen <- data.frame(dis = 1,
                            ftime = times,
                            status = event_screen) # without age




# calculate cost-effectiveness active TB values ---------------------------

# 12 month case fatality rate
cfr_age_lookup <- data.frame(age = c("[15,45)", "[45,65)", "[65,200)"),
                             cfr = c(0.0018, 0.0476, 0.1755),
                             a = c(1, 125, 413), #beta distn
                             b = c(564, 2500, 1940))
rownames(cfr_age_lookup) <- c("[15,45)", "[45,65)", "[65,200)")

QALYloss_TB_death <- 19.96
##TODO##
# could use expected death to calculate each QALY loss
# difference between time of death from active TB and all-cause time of death
# adjusted for utility of active TB

lifetime_QALYsloss <- 0.054

# treatment:
aTB_Tx_cost <- 5329
# gamma(8.333, 639.435)

# adverse effects?
# test: cost?

# create matching age bins in data
cfr_age <- cut(IMPUTED_sample$age_at_entry,
               breaks = c(15, 45, 65, 200), right = FALSE)

for (i in uk_tbX_names){

  whos_aTB <- IMPUTED_sample[ ,i]==1

  # expected QALY loss due to active TB in total cohort
  E.lifetime_QALYsloss_cohort <- mean(lifetime_QALYsloss * whos_aTB)

  # expected QALY loss due to early death due to active TB
  E.QALYloss_death_aTB <- cfr_age_lookup[cfr_age, "cfr"] * QALYloss_TB_death

  # expected QALY loss due to early death over total cohort
  E.QALYloss_death_cohort <- mean(E.QALYloss_death_aTB * whos_aTB)

  # expected treatment cost over total cohort
  E.aTB_Tx_cost_cohort <- mean(aTB_Tx_cost * whos_aTB)

  aTB_QALYloss <- c(aTB_QALYloss, E.lifetime_QALYsloss_cohort + E.QALYloss_death_cohort)
  aTB_cost <- c(aTB_cost, E.aTB_Tx_cost_cohort)
}


