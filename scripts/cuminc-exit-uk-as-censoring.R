#######################
## multistate model: ##
##
## assume that leave uk individuals are censored times
## to active TB progression
##
######################

#  without screening -------------------------------------------------------

# create final state vectors full sample
event <- rep(0, nrow(IMPUTED_sample)) #event-free i.e. censored event time
event[IMPUTED_sample$death1] <- 2
event[IMPUTED_sample$uk_tb_orig=="1"] <- 1

times <- IMPUTED_sample$fup1_issdt
j
# transition matrix
tmat <- trans.comprisk(2, c("event-free", "active_TB", "dead"))

dat <- data.frame(times)
dat$age_at_entry <- IMPUTED_sample$age_at_entry

cmprsk <- data.frame(dis = 1,
                     ftime = times,
                     status = event) # without age

attach(cmprsk)
fit = CumIncidence(ftime, status, dis, cencode=0, xlab="Days since arrival to UK", col=c(2,4))
legend("topleft", legend = c("Active TB","Death"), col=c(2,4), lty = 1, bg = "white")
detach(cmprsk)
