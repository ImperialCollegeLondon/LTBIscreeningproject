#
# project: LTBI screening
# N Green
# Oct 2016
#

# LTBI to active TB progression including individuals who leave the UK
# estimate (impute or proportionally) number of active TB cases outside of
# UK by year


source("scripts/03a-competing-risk-model_statusquo.R")


# generate at-risk population
# each year for outside UK

LTBI_status <- LTBIscreeningproject::sample_uk_tb(prob = 1 - IMPUTED_sample$pLTBI)


########################################
## create (outside uk) times to event ##
########################################

cmprsk <- data.frame(dis = 1,
                     ftime = times,
                     status = event) # without age

# replace exit uk with censored times
cmprsk$status[cmprsk$status==2] <- 0


# not all individuals are in the risk set of progressing to active TB
# we asssume that those without LTBI will never do so
# remove non-LTBI individuals

cmprsk <- cmprsk[LTBI_status==1 | IMPUTED_sample$uk_tb_orig==1, ]


# calculate and plot
# competing risk of active TB and all-cause death
# cumulative probability
# use total cohort

attach(cmprsk)
fit = CumIncidence(ftime, status, dis, cencode = 0,
                   t = seq(0, 3650, 365), #discrete yearly values
                   xlab = "Days since arrival to UK", col = 2:4)

legend("topleft", legend = c("Active TB", "Death"),
       col = 2:3, lty = 1, bg = "white")
detach(cmprsk)


# include year 0
cumprob.activetb <- c(0, fit$est[1, ])
names(cumprob.activetb) <- as.character(1:length(cumprob.activetb) - 1)



##################################################
## calculate absolute number of cases each year ##
##################################################

## annual populations

# extract year only
issdt_exit_year <- ceiling(IMPUTED_sample$date_exit_uk1_issdt/365)[LTBI_status==1 & whoin_year_cohort]

issdt_exit_year.tab <- table(issdt_exit_year)

# 101 is 'never leave UK'; remove
issdt_exit_year.tab <- issdt_exit_year.tab[as.numeric(names(issdt_exit_year.tab))<100]


# scaled-up CIF by year populations
max_year <- 10
activetb.exituk <- NULL

for (i in 1:max_year){

  pop_exit_in_year_i <- issdt_exit_year.tab[as.character(i)]

  cumprob.activetb_starting_year_i <- cumprob.activetb - cumprob.activetb[as.character(i - 1)]

  cumprob.activetb_starting_year_i <- pmax(0, cumprob.activetb_starting_year_i, na.rm = TRUE)

  activetb.exituk <- rbind(activetb.exituk,
                           pop_exit_in_year_i * cumprob.activetb_starting_year_i)
}
colnames(activetb.exituk) <- names(cumprob.activetb)


# sum all curves for each year
cum_year_totals <- colSums(activetb.exituk)
cum_year_total.diff <- diff(cum_year_totals)
cum_year_total.diff <- cum_year_total.diff[cum_year_total.diff>0]


###########
## plots ##
###########

plot(diff(cumprob.activetb), type = "h",
     main = "annual probability of active TB progression",
     xlab = "Time from UK entry (years)")


plot(x = 1:length(cum_year_total.diff),
     y = cum_year_total.diff,
     main = "Histogram of number of\n active TB cases outside of UK",
     ylim = c(0, 200), xlab = "Time from UK entry (years)", type = "h")


plot(issdt_exit_year.tab[-1],
     main = "annual number of recent arrivals cohort leaving UK",
     xlab = "Time from UK entry (years)")


# combined plots
windows(rescale = "R")

par(mar = c(5,4,4,5) + .1)
plot(issdt_exit_year.tab[-1][1:9],
     type = "s", col = "red",
     xlab = "", ylab = "Number leaving UK")

par(new = TRUE)
plot(cum_year_total.diff,
     main = "",
     ylim = c(0, 200), lty = 2,
     ylab = "", xlab = "Time from UK entry (years)", type = "s", col = "blue",
     xaxt = "n", yaxt = "n")
axis(4)
mtext("Number of active TB cases of leavers", side = 4, line = 3)
legend("topright", col = c("red","blue"), lty = c(1,2), legend = c("Leave UK","Active TB"))





#  ------------------------------------------------------------------------
# or
# equivalently at an individual level
# for each LTBI individual sample from CIF
# account for when they leave country they have already moved forward in time
# see Aldridge (2016) for method


