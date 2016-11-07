#
# project: LTBI screening
# N Green
# Oct 2016
#
# output plots active TB competing risks


library("ggplot2")
library("GGally")
# source("scripts/ggsurv.R")


# x-axis population scaling
scale <- 100000  #pop_year


################
## status quo ##
################

# compare different imputations
plot(survfit(cens_coxph1), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_1_fup_issdt, na.rm = T)), main="1_fup")

plot(survfit(cens_coxph2), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_2_fup_issdt, na.rm = T)), main="2_fup")

plot(survfit(cens_coxph3), xlab = "Days from UK entry", ylab = "Proportion non-active TB cases",
     xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), main="3_fup")


# Kaplan-Meier (non-parametric)
ggsurv(KM_original_year) + ylim(0.99,1) + xlim(0,1000) +
  ggtitle("Time to active TB split\n, censoring on death and leaving UK")

ggsurv(KM_original_year_age) + ylim(0.99,1) + xlim(0,1000) +
  ggtitle("Time to active TB split\n by age, censoring on death and leaving UK")


# KM cumulative incidence naive

plot(KM_original_full$time, scale * (1-KM_original_full$surv),
     xlab = "Days from UK entry", ylab = "Active TB cases per 100,000",
     xlim = c(0,3000), type = "l")

plot(KM_original_full_age$time, scale * (1-KM_original_full_age$surv),
     xlab = "Days from UK entry", ylab = "Active TB cases per 100,000",
     xlim = c(0,3000), type = "l")

plot(KM_original_year$time, scale * (1-KM_original_year$surv),
     xlab = "Days from UK entry", ylab = "Active TB cases per 100,000",
     xlim = c(0,3000), type = "l")

plot(KM_original_year_age$time, scale * (1-KM_original_year_age$surv),
     xlab = "Days from UK entry", ylab = "Active TB cases per 100,000",
     xlim = c(0,3000), type = "l")


# multistate models
plot(survfit(cx), col=2:4, xlab = "Days", ylab = "Survival probability")
legend("bottomleft", legend = c("Active TB", "Exit UK", "Death"), col=2:4, lty = 1)

attach(cmprsk)
fit = CumIncidence(ftime, status, dis, cencode=0, xlab="Days since arrival to UK", col=2:4)
legend("topleft", legend = c("Active TB", "Exit UK", "Death"), col=2:4, lty = 1, bg = "white")
detach(cmprsk)

attach(cmprsk_age)
fit = CumIncidence(ftime, status, dis, cencode=0, xlab="Days since arrival to UK")
# legend("topleft", legend = c("Active TB", "Exit UK", "Death"), col=2:4, lty = 1, bg = "white")
detach(cmprsk_age)



###############
## screening ##
###############


plot(cens_coxph1_predict_screened,
     xlab = "Days since UK entry", ylab = "Survival", main = "Cohort active TB progression prediction\n using all years fit")

# Kaplan-Meier (non-parametric)

ggsurv(KM_screen_year) + ylim(0.99,1) + xlim(0,1000) +
  ggtitle("Time to active TB split\n, censoring on death and leaving UK")

ggsurv(KM_screen_year_age) + ylim(0.99,1) + xlim(0,1000) +
  ggtitle("Time to active TB split\n by age, censoring on death and leaving UK")

# KM cumulative incidence naive

plot(KM_screen_full$time, scale * (1-KM_screen_full$surv),
     xlim = c(0,800), type = "l",
     ylab = "Survival", xlab = "Days since UK entry", main = "Cohort active TB progression Kaplan-Meier\n using cohort year fit")

plot(KM_screen_full_age$time, scale * (1-KM_screen_full_age$surv),
     xlim = c(0,800), type = "l",
     ylab = "Survival", xlab = "Days since UK entry", main = "Cohort active TB progression Kaplan-Meier\n using cohort year fit")

plot(KM_screen_year$time, scale * (1-KM_screen_year$surv),
     xlim = c(0,800), type = "l",
     ylab = "Survival", xlab = "Days since UK entry", main = "Cohort active TB progression Kaplan-Meier\n using cohort year fit")

plot(KM_screen_year_age$time, scale * (1-KM_screen_year_age$surv),
     xlim = c(0,800), type = "l",
     ylab = "Survival", xlab = "Days since UK entry", main = "Cohort active TB progression Kaplan-Meier\n using cohort year fit")


# plot all realisations of active TB cases after screening survival curves
ggsurv(KM_screen_full_uk_tbX)


######################
## multistate model ##
######################

plot(survfit(cx_screen), col=2:4, xlab = "Days", ylab = "Survival probability")
legend("bottomleft", legend = c("Active TB", "Exit UK", "Death"), col=2:4, lty = 1)

attach(cmprsk_screen)
fit = CumIncidence(ftime, status, dis, cencode=0, xlab="Days since arrival to UK", col=2:4)
legend("topleft", legend = c("Active TB", "Exit UK", "Death"), col=2:4, lty = 1, bg = "white")
detach(cmprsk_screen)

attach(cmprsk_age_screen)
fit = CumIncidence(ftime, status, dis, cencode=0, xlab="Days since arrival to UK", col=2:4)
legend("topleft", legend = c("Active TB", "Exit UK", "Death"), col=2:4, lty = 1, bg = "white")
detach(cmprsk_age_screen)



# raw data number of TB cases over time

CDF <- ecdf(IMPUTED_sample$fup1_issdt[IMPUTED_sample$uk_tb==1])
plot(CDF, yaxt='n', xlab="Days", main="Raw cumulative counts of active TB cases")
axis(side = 2, at = 1, tck = 0.01, labels = sum(IMPUTED_sample$uk_tb), las="2")




# barplots ----------------------------------------------------------------

# see Robs Lancet (2016)
counts <- table(floor(IMPUTED_sample$fup2_issdt[IMPUTED_sample$uk_tb_orig==1]/365))
barplot(counts, ylim=c(0,600), xlab=("Years since migration"), ylab="Number of cases")




##TODO##

# money saved/cases averted over time
#   take difference of status quo and screening active TB cases

# case fatality ratio: at the moment dont have this in the model. we stop at TB activation.

