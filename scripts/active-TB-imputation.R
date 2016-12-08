#
# project: LTBI screening
# N Green
# Oct 2016
#

# LTBI to active TB progression including individuals who leave the UK
# estimate (impute or proportionally) number of active TB cases outside of
# UK over time


source("03a-competing-risk-model_statusquo.R")


# create outside uk times to event

cmprsk <- data.frame(dis = 1,
                     ftime = times,
                     status = event) # without age

# replace exit uk with censored times
cmprsk$status[cmprsk$status==2] <- 0

attach(cmprsk)
fit = CumIncidence(ftime, status, dis, cencode=0,
                   t = seq(0, 3650, 365), #course yearly values
                   xlab="Days since arrival to UK", col=2:4)
legend("topleft", legend = c("Active TB", "Death"), col=2:3, lty = 1, bg = "white")
detach(cmprsk)


# cumulative probability of active TB in UK
# use total cohort
# censor at leave UK
# competing risk of all-cause death

cumprob.activetb <- fit$est[1, ]


##TODO##
# do for all 10 imputation samples
# ...





# at risk population each year for outside UK
# extract year only
LTBI_status <- sample_uk_tb(prob = 1 - IMPUTED_sample$pLTBI)

issdt_exit_year <- ceiling((IMPUTED_sample$date_exit_uk1 - IMPUTED_sample$issdt)/365)[LTBI_status==1 & IMPUTED_sample$issdt_year=="2009"]

issdt_exit_year.tab <- table(issdt_exit_year)

# 101 is never leave; remove
issdt_exit_year.tab <- issdt_exit_year.tab[as.numeric(names(issdt_exit_year.tab))<100]

# scaled-up CIF by year population
activetb.exituk <- issdt_exit_year.tab["0"] * cumprob.activetb

for (i in 1:10){

  activetb.exituk <- rbind(activetb.exituk,
                           issdt_exit_year.tab[as.character(i)] * (pmax(0, cumprob.activetb - cumprob.activetb[i], na.rm=TRUE)))
}


# sum all curves for each year
cum_year_totals <- colSums(activetb.exituk)
cum_year_total.diff <- cum_year_totals[-1] - cum_year_totals
plot(0:10, cum_year_total.diff, ylim=c(0, 100), xlab="year", type="h")



# or
# equivalently at an individual level
# for each LTBI individual sample from CIF
# account for when they leave country they have already moved forward in time
# see Aldridge (2016) for method






