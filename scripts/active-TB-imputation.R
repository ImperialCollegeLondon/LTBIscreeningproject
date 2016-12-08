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

cumprob.activetb <- c(0, fit$est[1, ])
names(cumprob.activetb) <- as.character(1:length(cumprob.activetb) - 1)


##TODO##
# do for all 10 imputation samples
# ...





# generate at risk population each year for outside UK
LTBI_status <- sample_uk_tb(prob = 1 - IMPUTED_sample$pLTBI)

# extract year only
issdt_exit_year <- ceiling(IMPUTED_sample$date_exit_uk1_issdt/365)[LTBI_status==1 & whoin_year_cohort]

issdt_exit_year.tab <- table(issdt_exit_year)

# 101 is 'never leave UK'; remove
issdt_exit_year.tab <- issdt_exit_year.tab[as.numeric(names(issdt_exit_year.tab))<100]

# scaled-up CIF by year population
max_year <- 10
activetb.exituk <- NULL

for (i in 1:max_year){

  pop_exit_in_year_i <- issdt_exit_year.tab[as.character(i)]

  cumprob.activetb_starting_year_i <- pmax(0, cumprob.activetb - cumprob.activetb[as.character(i - 1)], na.rm = TRUE)

  activetb.exituk <- rbind(activetb.exituk,
                           pop_exit_in_year_i * cumprob.activetb_starting_year_i)
}
colnames(activetb.exituk) <- names(cumprob.activetb)


# sum all curves for each year
cum_year_totals <- colSums(activetb.exituk)
cum_year_total.diff <- diff(cum_year_totals)
cum_year_total.diff <- cum_year_total.diff[cum_year_total.diff>0]

plot(x = 1:length(cum_year_total.diff),
     y = cum_year_total.diff,
     main = "Histogram of number of\n active TB cases outside of UK",
     ylim = c(0, 100), xlab = "Time from UK entry (years)", type = "h")



# or
# equivalently at an individual level
# for each LTBI individual sample from CIF
# account for when they leave country they have already moved forward in time
# see Aldridge (2016) for method






