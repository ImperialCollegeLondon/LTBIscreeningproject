#
# project: LTBI screening
# N Green
# Oct 2016
#
# fit a competing risk model to imputed active TB data


library(survival)
library(mstate)
source("scripts/ggsurv.R")



###############
## DATA PREP ##
###############

##TODO##
# use these times from modified STATA code...
# cr.colnames <- c("date_exit_uk", "date_death", "rNotificationDate", "issdt", "age_at_entry")

cr.colnames <- c("X_9_fup", "length_uk_stay", "time_screen_case", "uk_tb", "rNotificationDate", "issdt", "age_at_entry")

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




####################
## COMPETING RISK ##
####################

# simple approach: other events as censored (active TB) times ----------

res_coxph1 <- coxph(Surv(X_1_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
res_coxph2 <- coxph(Surv(X_2_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
res_coxph3 <- coxph(Surv(X_3_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)

plot(survfit(res_coxph1), xlab="Days from uk entry", ylab="Proportion non-active TB",
     xlim=c(0, max(IMPUTED_sample$X_1_fup_issdt, na.rm = T)), main="1_fup")

plot(survfit(res_coxph2), xlab="Days from uk entry", ylab="Proportion non-active TB",
     xlim=c(0, max(IMPUTED_sample$X_2_fup_issdt, na.rm = T)), main="2_fup")

plot(survfit(res_coxph3), xlab="Days from uk entry", ylab="Proportion non-active TB",
     xlim=c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), main="3_fup")


##TODO## what is this number?
# cohort size at arrival to uk
pop <- 1000

# scaled F = 1-S
cum_activeTB <- pop * (1 - exp(-survfit(formula = res_coxph3)$cumhaz))

plot(survfit(res_coxph3)$time, cum_activeTB,
     xlab = "Days from uk entry", ylab="Number active TB",
     xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), ylim = c(0,1*pop), type="l",
     main="3_fup")

# stratified by age
res_survfit <- survfit(Surv(X_3_fup_issdt, uk_tb) ~ age_at_entry, data = IMPUTED_sample)
ggsurv(res_survfit) + ylim(0,1)


## cmprsk::
# z <- cmprsk::crr(times, event, IMPUTED_LTBI$age_group2)
# z.p <- predict(z, 2)
# plot(z.p)

## investigate censoring assumption
# c_tb <- coxph(Surv(times, event == 3) ~ age_at_entry, data = IMPUTED_sample) #tb
# c_emmig <- coxph(Surv(times, event == 2) ~ age_at_entry, data = IMPUTED_sample) #leave uk
#
# s_tb <- survfit(c_tb)
# s_emmig <- survfit(c_emmig)
#
# plot(s_emmig, col=2, xlab="Days from uk entry", ylab="survival", xlim=c(0,5000), main="TB activation or leaving Uk as event time")
# lines(s_tb, col=3)



# multistate model --------------------------------------------------------

##TODO##
# need leave_uk and death times from STATA model...


IMPUTED_LTBI <- transform(IMPUTED_LTBI,
                          leave_uk = length_uk_stay <= `X_9_fup` & uk_tb==0)

# create final state vector
event <- rep(0, nrow(IMPUTED_LTBI)) #event-free i.e. censored event time
# event <- rep(1, nrow(IMPUTED_LTBI)) #death
event[IMPUTED_LTBI$leave_uk] <- 2
event[IMPUTED_LTBI$uk_tb=="1"] <- 3

times <- IMPUTED_LTBI$`X_9_fup`
times[event==2] <- IMPUTED_LTBI$length_uk_stay[event==2]
times[event==3] <- IMPUTED_LTBI$time_screen_case[event==3]


# https://cran.r-project.org/web/packages/mstate/vignettes/Tutorial.pdf
# http://ac.els-cdn.com/S0169260710000027/1-s2.0-S0169260710000027-main.pdf?_tid=7f7ba4f8-897d-11e6-9905-00000aacb361&acdnat=1475508458_40cc7e1da75d2650bd432f8337eb448a

ci <- mstate::Cuminc(time = times, status = event)
plot(ci, xlab = "Days from uk entry", ylab="cumulative incidence probability", xlim = c(0, 5000), col = 2:10)

ci <- mstate::Cuminc(time = times, status = event, group = IMPUTED_LTBI$age_at_entry)
plot(ci, xlab = "Days from uk entry", ylab="cumulative incidence probability", xlim = c(0, 5000), col = 2:10, main="split by age")


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

