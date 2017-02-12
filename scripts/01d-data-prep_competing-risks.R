#
# project: LTBI screening
# N Green
# Oct 2016
#
# create competing risk modelling data
# [fit competing risk models to imputed complete dataset]
# _without_ screening



library(survival)
library(mstate)
library(cmprsk) # http://www.stat.unipg.it/luca/R



# create uk_entry to follow-up times ---------------------------------------------

# find all columns with follow-up time imputations
cols_fup <- grepl(pattern = "fup", x = names(IMPUTED_sample))

# find all columns with either exit uk or death event time imputations
cols_eventdate <- grepl(pattern = "date_exit_uk|date_death", x = names(IMPUTED_sample))

# days to arrival in uk from time origin
issdt.asnumeric <- IMPUTED_sample$issdt - as.Date("1960-01-01")

# days from arrival in uk to end of follow-up
fup_issdt <- apply(IMPUTED_sample[ ,cols_fup], 2, FUN = function(x) x - issdt.asnumeric)

colnames(fup_issdt) <- paste0(colnames(fup_issdt), "_issdt")

# days from uk arrival to death & uk exit
event_issdt <- apply(IMPUTED_sample[ ,cols_eventdate], 2,
                     FUN = function(y) as.Date(y, "%Y-%m-%d") - IMPUTED_sample$issdt)

colnames(event_issdt) <- paste0(colnames(event_issdt), "_issdt")

IMPUTED_sample <- data.frame(IMPUTED_sample,
                             event_issdt,
                             fup_issdt)



fup_limit <- 19723  #days from 1960-01-01


# first event indicator (T/F) for each imputation -------------------------------

IMPUTED_sample <- transform(IMPUTED_sample,
                            cens1  = fup1==fup_limit,
                            cens2  = fup2==fup_limit,
                            cens3  = fup3==fup_limit,
                            cens4  = fup4==fup_limit,
                            cens5  = fup5==fup_limit,
                            cens6  = fup6==fup_limit,
                            cens7  = fup7==fup_limit,
                            cens8  = fup8==fup_limit,
                            cens9  = fup9==fup_limit,
                            cens10 = fup10==fup_limit,

                            death1  = (date_death1<=date_exit_uk1 & uk_tb==0 & fup1!=fup_limit), #is.death(1, IMPUTED_sample)
                            death2  = (date_death2<=date_exit_uk2 & uk_tb==0 & fup2!=fup_limit), #is.death(2, IMPUTED_sample)
                            death3  = (date_death3<=date_exit_uk3 & uk_tb==0 & fup3!=fup_limit),
                            death4  = (date_death4<=date_exit_uk4 & uk_tb==0 & fup4!=fup_limit),
                            death5  = (date_death5<=date_exit_uk5 & uk_tb==0 & fup5!=fup_limit),
                            death6  = (date_death6<=date_exit_uk6 & uk_tb==0 & fup6!=fup_limit),
                            death7  = (date_death7<=date_exit_uk7 & uk_tb==0 & fup7!=fup_limit),
                            death8  = (date_death8<=date_exit_uk8 & uk_tb==0 & fup8!=fup_limit),
                            death9  = (date_death9<=date_exit_uk9 & uk_tb==0 & fup9!=fup_limit),
                            death10 = (date_death10<=date_exit_uk10 & uk_tb==0 & fup10!=fup_limit),

                            exit_uk1  = (date_death1>date_exit_uk1 & uk_tb==0 & fup1!=fup_limit),
                            exit_uk2  = (date_death2>date_exit_uk2 & uk_tb==0 & fup2!=fup_limit),
                            exit_uk3  = (date_death3>date_exit_uk3 & uk_tb==0 & fup3!=fup_limit),
                            exit_uk4  = (date_death4>date_exit_uk4 & uk_tb==0 & fup4!=fup_limit),
                            exit_uk5  = (date_death5>date_exit_uk5 & uk_tb==0 & fup5!=fup_limit),
                            exit_uk6  = (date_death6>date_exit_uk6 & uk_tb==0 & fup6!=fup_limit),
                            exit_uk7  = (date_death7>date_exit_uk7 & uk_tb==0 & fup7!=fup_limit),
                            exit_uk8  = (date_death8>date_exit_uk8 & uk_tb==0 & fup8!=fup_limit),
                            exit_uk9  = (date_death9>date_exit_uk9 & uk_tb==0 & fup9!=fup_limit),
                            exit_uk10 = (date_death10>date_exit_uk10 & uk_tb==0 & fup10!=fup_limit))



IMPUTED_sample_year_cohort <- transform(IMPUTED_sample_year_cohort,
                                        cens1  = fup1==fup_limit,
                                        cens2  = fup2==fup_limit,
                                        cens3  = fup3==fup_limit,
                                        cens4  = fup4==fup_limit,
                                        cens5  = fup5==fup_limit,
                                        cens6  = fup6==fup_limit,
                                        cens7  = fup7==fup_limit,
                                        cens8  = fup8==fup_limit,
                                        cens9  = fup9==fup_limit,
                                        cens10 = fup10==fup_limit,

                                        death1  = (date_death1<=date_exit_uk1 & uk_tb==0 & fup1!=fup_limit),
                                        death2  = (date_death2<=date_exit_uk2 & uk_tb==0 & fup2!=fup_limit),
                                        death3  = (date_death3<=date_exit_uk3 & uk_tb==0 & fup3!=fup_limit),
                                        death4  = (date_death4<=date_exit_uk4 & uk_tb==0 & fup4!=fup_limit),
                                        death5  = (date_death5<=date_exit_uk5 & uk_tb==0 & fup5!=fup_limit),
                                        death6  = (date_death6<=date_exit_uk6 & uk_tb==0 & fup6!=fup_limit),
                                        death7  = (date_death7<=date_exit_uk7 & uk_tb==0 & fup7!=fup_limit),
                                        death8  = (date_death8<=date_exit_uk8 & uk_tb==0 & fup8!=fup_limit),
                                        death9  = (date_death9<=date_exit_uk9 & uk_tb==0 & fup9!=fup_limit),
                                        death10 = (date_death10<=date_exit_uk10 & uk_tb==0 & fup10!=fup_limit),

                                        exit_uk1  = (date_death1>date_exit_uk1 & uk_tb==0 & fup1!=fup_limit),
                                        exit_uk2  = (date_death2>date_exit_uk2 & uk_tb==0 & fup2!=fup_limit),
                                        exit_uk3  = (date_death3>date_exit_uk3 & uk_tb==0 & fup3!=fup_limit),
                                        exit_uk4  = (date_death4>date_exit_uk4 & uk_tb==0 & fup4!=fup_limit),
                                        exit_uk5  = (date_death5>date_exit_uk5 & uk_tb==0 & fup5!=fup_limit),
                                        exit_uk6  = (date_death6>date_exit_uk6 & uk_tb==0 & fup6!=fup_limit),
                                        exit_uk7  = (date_death7>date_exit_uk7 & uk_tb==0 & fup7!=fup_limit),
                                        exit_uk8  = (date_death8>date_exit_uk8 & uk_tb==0 & fup8!=fup_limit),
                                        exit_uk9  = (date_death9>date_exit_uk9 & uk_tb==0 & fup9!=fup_limit),
                                        exit_uk10 = (date_death10>date_exit_uk10 & uk_tb==0 & fup10!=fup_limit))


# create final state vectors full sample
event <- rep(0, n.pop) #event-free i.e. censored event time
event[IMPUTED_sample$death1] <- 3
event[IMPUTED_sample$exit_uk1] <- 2
event[IMPUTED_sample$uk_tb_orig=="1"] <- 1


# 'observed' event time
fup_times <- fup_issdt[ ,"fup1_issdt"]

##TODO: use imputed exit uk and death times instead of followup censoring


dat <- data.frame(fup_times)
dat$age_at_entry <- IMPUTED_sample$age_at_entry

dat$event3 <- as.numeric(event == 3) #death
dat$event2 <- as.numeric(event == 2) #exit_uk
dat$event1 <- as.numeric(event == 1) #uk_tb


# multistate model --------------------------------------------------------
#
# # transition matrix
# tmat <- trans.comprisk(3, c("event-free", "active_TB", "exit_uk", "dead"))
#
# # transform to mstate format array
# mslong <- msprep(time = c(NA, "fup_times", "fup_times", "fup_times"),
#                  status = c(NA, "event1", "event2", "event3"),
#                  data = dat,
#                  keep = "age_at_entry",
#                  trans = tmat)
#
# # check frequencies
# events(mslong)
#
# # append age to mstate format array
# mslong <- expand.covs(mslong, "age_at_entry")
#
# cx <- coxph(Surv(Tstart, Tstop, status) ~ age_at_entry.1 + age_at_entry.2 + age_at_entry.3 + strata(trans),
#             data = mslong,
#             method = "breslow")
# cx


