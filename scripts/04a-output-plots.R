#
# project: LTBI screening
# N Green
# Oct 2016
#
# output plots active TB competing risks


source("scripts/ggsurv.R")



################
## status quo ##
################

# plot(ci, xlab = "Days from UK entry", ylab = "Cumulative incidence probability", main = "Multistate model",
#      xlim = c(0, 5000), col = 2:10)
#
# plot(ci.age, xlab = "Days from UK entry", ylab = "Cumulative incidence probability", main = "Multistate model: split by age",
#      xlim = c(0, 5000), col = 2:10)

# plot(survfit(cens_coxph3)$time, cum_activeTB,
#      xlab = "Days from UK entry", ylab = "Number active TB cases",
#      xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), ylim = c(0, pop), type = "l",
#      main = "3_fup censored for non-active TB events")


# compare different imputations
plot(survfit(cens_coxph1), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_1_fup_issdt, na.rm = T)), main="1_fup")

plot(survfit(cens_coxph2), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_2_fup_issdt, na.rm = T)), main="2_fup")

plot(survfit(cens_coxph3), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), main="3_fup")



ggsurv(KM_original_year_age) + ylim(0,1) +
  ggtitle("Time to active TB split\n by age, censoring on death and leaving UK")

# x-axis population scaling
scale <- 100000  #pop_year

# Kaplan-Meier (non-parametric) cumulative incidence

plot(KM_original_full$time, scale * (1-KM_original_full$surv),
     xlim = c(0,800), type = "l")
plot(KM_original_full_age$time, scale * (1-KM_original_full_age$surv),
     xlim = c(0,800), type = "l")
plot(KM_original_year$time, scale * (1-KM_original_year$surv),
     xlim = c(0,800), type = "l")
plot(KM_original_year_age$time, scale * (1-KM_original_year_age$surv),
     xlim = c(0,800), type = "l")


plot(survfit(cx), col=2:4, xlab = "Days", ylab = "Survival probability")
legend("bottomleft", legend = c("Active TB", "Exit UK", "Death"), col=2:4, lty = 1)



###############
## screening ##
###############


plot(KM_screened_year$time, pop_year * (1-KM_screened_year$surv), xlim = c(0,800), type = "l")

plot(KM_screened_full, lty = 2:3,
     ylab = "survival", xlab = "Days since UK entry", main = "Cohort active TB progression Kaplan-Meier\n using cohort year fit")
legend(100, 0.8, c("", ""), lty = 2:3)

plot(cens_coxph1_predict_screened,
     xlab = "Days since UK entry", ylab = "Survival", main = "Cohort active TB progression prediction\n using all years fit")


# Kaplan-Meier (non-parametric) cumulative incidence

plot(KM_screen_full$time, scale * (1-KM_screen_full$surv),
     xlim = c(0,800), type = "l")
plot(KM_screen_full_age$time, scale * (1-KM_screen_full_age$surv),
     xlim = c(0,800), type = "l")
plot(KM_screen_year$time, scale * (1-KM_screen_year$surv),
     xlim = c(0,800), type = "l")
plot(KM_screen_year_age$time, scale * (1-KM_screen_year_age$surv),
     xlim = c(0,800), type = "l")


plot(survfit(cx_screen), col=2:4, xlab = "Days", ylab = "Survival probability")
legend("bottomleft", legend = c("Active TB", "Exit UK", "Death"), col=2:4, lty = 1)



##TODO##

# money saved/cases averted over time
#   take difference of status quo and screening active TB cases

# case fatality ratio: at the moment dont have this in the model. we stop at TB activation.

