#
# project: LTBI screening
# N Green
# Oct 2016
#
# output plots


library(BCEA)
source("scripts/ggsurv.R")


# screening decision tree -----------------------------------------------------------

screen.bcea <- bcea(e = cbind('donothing'=0, 'interv'=mc.health$`expected values`),
                    c = cbind('donothing'=0, 'interv'=mc.cost$`expected values`),
                    ref = 1)

ceplane.plot(screen.bcea)


##TODO##
# have number of active TB cases as health detriment instead?


# active TB competing risks ---------------------------------------------------------

################
## status quo ##
################

plot(ci, xlab = "Days from UK entry", ylab = "Cumulative incidence probability", main = "Multistate model",
     xlim = c(0, 5000), col = 2:10)

plot(ci.age, xlab = "Days from UK entry", ylab = "Cumulative incidence probability", main = "Multistate model: split by age",
     xlim = c(0, 5000), col = 2:10)

# compare different imputations
plot(survfit(cens_coxph1), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_1_fup_issdt, na.rm = T)), main="1_fup")

plot(survfit(cens_coxph2), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_2_fup_issdt, na.rm = T)), main="2_fup")

plot(survfit(cens_coxph3), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), main="3_fup")


plot(survfit(cens_coxph3)$time, cum_activeTB,
     xlab = "Days from UK entry", ylab = "Number active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), ylim = c(0, pop), type = "l",
     main = "3_fup censored for non-active TB events")


ggsurv(cens_survfit_byage) + ylim(0,1) +
  ggtitle("Time to active TB split\n by age, censoring on death and leaving UK")


###############
## screening ##
###############

plot(KM_screened, lty = 2:3,
     ylab = "survival", xlab = "Days since UK entry", main = "Cohort active TB progression Kaplan-Meier using cohort year fit")
legend(100, 0.8, c("", ""), lty = 2:3)

plot(cens_coxph1_predict_screened,
     xlab = "Days since UK entry", ylab = "Survival", main = "Cohort active TB progression prediction using all years fit")


##TODO##

# money saved/cases averted over time
#   take difference of status quo and screening active TB cases

# probability cost-effective for adherence vs uptake, for given costs per test

# case fatality ratio: at the moment dont have this in the model. we stop at TB activation.

