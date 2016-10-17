#
# project: LTBI screening
# N Green
# Oct 2016
#
# fit a competing risk model to imputed active TB data


## variable used in competing risk calculation:
# age on entry - age_at_entry
# country of origin - iso_a3_country or iso_n3_country
# who prevalence in that country - who_prev_cat or who_prevalence
# date of arrival in UK - issdt (we assumed they travel on day they are issued the certificate,
#   which is a bit unrealistic, but we didn't have anything better to make an estimate on)
# date of death or time to death from arrival - We don't actually save the date of death as a variable as we never need to,
#   but you can easily create one by editing the code slightly in do file 7 and taking the date of death from the variable date_death within the macro there.
#   For them to actually die within the cohort date_death would have to be less than: 1) end of the cohort (variable is cohort_end); or 2)
#   before they left the uk on their visa (variable is date_exit_uk)
# data of active TB or time to active TB from arrival - rNotificationDate
# date of leaving country or time to leaving country - again we don't save this variable but it is also within do file 7 under the variable date_exit_uk.
#   For this to be valid it would have to be less than: 1) end of the cohort (variable is cohort_end); or 2) date of death date_death


library(survival)
library(mstate)
source("scripts/ggsurv.R")


data("IMPUTED_sample")


# cr.colnames <- c("date_exit_uk", "date_death", "rNotificationDate", "issdt", "age_at_entry")
cr.colnames <- c("_9_fup", "length_uk_stay", "time_screen_case", "uk_tb", "rNotificationDate", "issdt", "age_at_entry")
IMPUTED_LTBI <- IMPUTED_sample[IMPUTED_sample$LTBI, cr.colnames]


# convert follow-up columns to date format ---------------------------------

cols_fup <- grepl(pattern = "_fup", x = names(IMPUTED_sample))
# initialise
fup_asDate <- data.frame(matrix(vector(), ncol = sum(cols_fup), nrow = nrow(IMPUTED_sample)))


##TODO##
# why does it not want to stay as date format??
for (j in seq_len(length.out = sum(cols_fup))){
  for (i in seq_len(length.out = nrow(IMPUTED_sample))){

    fup_asDate[i,j] <-  as.Date(IMPUTED_sample[,cols_fup][i,j], origin="1960-01-01")
  }
}


# _fup has lots of individuals at 19723 = 31st Dec 2013 (last ETS date)
# as.Date("2013/12/31") - 19723
# [1] "1960-01-01"
# is this a censoring time? should we include event=0?

# date of arrival
issdt.asnumeric <- as.Date(IMPUTED_sample$issdt) - as.Date("1960-01-01")

x <- apply(IMPUTED_sample[ ,cols_fup], 2, FUN = function(x) x - issdt.asnumeric)
colnames(x) <- paste(colnames(x), "_issdt", sep="")

IMPUTED_sample <- data.frame(IMPUTED_sample, x)

# time from uk entry to active tb
rNotificationDate.asnumeric <- as.Date(IMPUTED_sample$rNotificationDate) - as.Date("1960-01-01")


# other events as censored (active TB) times -------------------------------

res_coxph1 <- coxph(Surv(X_1_fup_issdt, uk_tb) ~ age_at_entry, data=IMPUTED_sample)
res_coxph2 <- coxph(Surv(X_2_fup_issdt, uk_tb) ~ age_at_entry, data=IMPUTED_sample)
res_coxph3 <- coxph(Surv(X_3_fup_issdt, uk_tb) ~ age_at_entry, data=IMPUTED_sample)

plot(survfit(res_coxph1), xlab="days from uk entry", ylab="Proportion non-active TB",
     xlim=c(0, max(IMPUTED_sample$X_1_fup_issdt, na.rm = T)))
plot(survfit(res_coxph2), xlab="days from uk entry", ylab="Proportion non-active TB",
     xlim=c(0, max(IMPUTED_sample$X_2_fup_issdt, na.rm = T)))
plot(survfit(res_coxph3), xlab="days from uk entry", ylab="Proportion non-active TB",
     xlim=c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)))


# cumulative incidence
pop <- 1000
plot(survfit(res_coxph3)$time, pop*(1-exp(-survfit(res_coxph3)$cumhaz)),
     xlab = "days from uk entry", ylab="Number active TB",
     xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), ylim = c(0,1*pop), type="l")

# stratified by age
res_survfit <- survfit(Surv(X_1_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
ggsurv(res_survfit) + ylim(0,1)


#  competing risk -----------------------------------------------------

event <- rep(0, nrow(IMPUTED_LTBI)) #event-free i.e. censored event time
event <- rep(1, nrow(IMPUTED_LTBI)) #death
event[IMPUTED_LTBI$length_uk_stay<=IMPUTED_LTBI$`X_9_fup`] <- 2  #emmigrate
event[IMPUTED_LTBI$uk_tb=="1"] <- 3

times <- IMPUTED_LTBI$`X_9_fup`
times[event==2] <- IMPUTED_LTBI$length_uk_stay[event==2]
times[event==3] <- IMPUTED_LTBI$time_screen_case[event==3]



#  regression models ---------------------------------------------------

# cmprsk::
# z <- cmprsk::crr(times, event, IMPUTED_LTBI$age_group2)
# z.p <- predict(z, 2)
# plot(z.p)

# naive censoring (bias?)
c_tb <- coxph(Surv(times, event == 3) ~ IMPUTED_LTBI$ager) #tb
c_emmig <- coxph(Surv(times, event == 2) ~ IMPUTED_LTBI$ager) #emmigrate

s_tb <- survfit(c_tb)
s_emmig <- survfit(c_emmig)

plot(s_emmig, col=2, xlab="time", ylab="survival", xlim=c(0,5000), main="")
lines(s_tb, col=3)

# mstate::
# https://cran.r-project.org/web/packages/mstate/vignettes/Tutorial.pdf
# http://ac.els-cdn.com/S0169260710000027/1-s2.0-S0169260710000027-main.pdf?_tid=7f7ba4f8-897d-11e6-9905-00000aacb361&acdnat=1475508458_40cc7e1da75d2650bd432f8337eb448a
ci <- mstate::Cuminc(time = times, status = event, group = IMPUTED_LTBI$age_at_entry)
plot(ci, xlab="time", ylab="cumulative incidence probability", xlim=c(0,5000), col=2:10)


tmat <- trans.comprisk(3, c("event-free", "emmigrate", "dead", "active_TB"))

dat <- data.frame(times)
dat$event1 <- as.numeric(event == 1)
dat$event2 <- as.numeric(event == 2)
dat$event3 <- as.numeric(event == 3)
dat$age_at_entry <- IMPUTED_LTBI$age_at_entry

mslong <- msprep(time = c(NA, "times", "times", "times"),
                 status = c(NA, "event1", "event2", "event3"),
                 data = dat, keep = "age_at_entry", trans = tmat)
## could include other (latent) times (not just first) cos we unusually do know these. so more status =1??

# check frequencies
events(mslong)

mslong <- expand.covs(mslong, "age_at_entry")

cx <- coxph(Surv(Tstart, Tstop,status)~age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
            data = mslong, method = "breslow")

# HvH <- msfit(cx, newdate=, trans=tmat)
# pt <- probtrans(HvH,predt=0)
# pt[[1]] # predictions from state 1

