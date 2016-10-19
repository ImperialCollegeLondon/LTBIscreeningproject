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


# active TB competing risks ---------------------------------------------------------

plot(ci, xlab = "Days from Uk entry", ylab = "Cumulative incidence probability", main = "Multistate model",
     xlim = c(0, 5000), col = 2:10)

plot(ci.age, xlab = "Days from UK entry", ylab = "Cumulative incidence probability", main = "Multistate model: split by age",
     xlim = c(0, 5000), col = 2:10)


plot(survfit(cens_coxph1), xlab = "Days from UK entry", ylab = "Proportion non-active TB",
     xlim = c(0, max(IMPUTED_sample$X_1_fup_issdt, na.rm = T)), main="1_fup")

plot(survfit(cens_coxph2), xlab = "Days from UK entry", ylab = "Proportion non-active TB",
     xlim = c(0, max(IMPUTED_sample$X_2_fup_issdt, na.rm = T)), main="2_fup")

plot(survfit(cens_coxph3), xlab = "Days from UK entry", ylab = "Proportion non-active TB",
     xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), main="3_fup")


plot(survfit(cens_coxph3)$time, cum_activeTB,
     xlab = "Days from UK entry", ylab = "Number active TB",
     xlim = c(0, max(IMPUTED_sample$X_3_fup_issdt, na.rm = T)), ylim = c(0, 1*pop), type = "l",
     main = "3_fup censored non-active TB events")


ggsurv(cens_survfit_byage) + ylim(0,1) +
  ggtitle("Time to active TB split\n by age, censoring on death and leaving UK")


