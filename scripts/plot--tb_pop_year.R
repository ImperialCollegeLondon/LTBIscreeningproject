
cohort1 <-
  IMPUTED_sample %>%
  subset(IMPUTED_sample$who_inc_Pareek2011 %in% "(150,250]")

cohort2 <-
  IMPUTED_sample %>%
  subset(IMPUTED_sample$who_inc_Pareek2011 %in% "(250,350]")

cohort3 <-
  IMPUTED_sample %>%
  subset(IMPUTED_sample$who_inc_Pareek2011 %in% "(350,1e+05]")



# histograms with different bin sizes -------------------------------------

hist(cohort1$rNotificationDate_issdt.years[!is.na(cohort$rNotificationDate_issdt.years)],
     breaks = c(0,1,2,3,4,5,6,7,9,15,50,100),
     col = "black",
     prob = TRUE,
     main = "",
     xlab = "year")
hist(cohort2$rNotificationDate_issdt.years[!is.na(cohort$rNotificationDate_issdt.years)],
     breaks = c(0,1,2,3,4,5,6,7,9,15,50,100), prob = TRUE,
     add = T,
     col = "blue",
     border = "blue")
hist(cohort3$rNotificationDate_issdt.years[!is.na(cohort$rNotificationDate_issdt.years)],
     breaks = c(0,1,2,3,4,5,6,7,9,15,50,100), prob = TRUE,
     add = T,
     col = "white",
     border = "red")


# density plots -----------------------------------------------------------

plot(density(cohort1$rNotificationDate_issdt.years, from = 0, bw = 1, na.rm = TRUE),
     lwd = 2,
     ylim = c(0, 0.002),
     main = "")
lines(density(cohort2$rNotificationDate_issdt.years, from = 0, bw = 1, na.rm = TRUE),
      lwd = 2, col = "blue")
lines(density(cohort3$rNotificationDate_issdt.years, from = 0, bw = 1, na.rm = TRUE),
      lwd = 2, col = "red")



# survival plots ----------------------------------------------------------

library(survminer)
library(survival)

fit_tb <- survfit(Surv(rNotificationDate_issdt.years) ~ who_inc_Pareek2011,
               data = IMPUTED_sample)

ggsurvplot(fit_tb,
           data = IMPUTED_sample,
           risk.table = TRUE,
           ylim = c(0.975, 1),
           xlim = c(0, 20),
           main = "TB notification in EWNI")#,
           # conf.int = TRUE,
           # ggtheme = theme_minimal(),
           # risk.table.y.text.col = T,
           # risk.table.y.text = FALSE)

fit_exit <- survfit(Surv(IMPUTED_sample$date_exit_uk1_issdt.years) ~ who_inc_Pareek2011,
               data = IMPUTED_sample)

ggsurvplot(fit_exit,
           data = IMPUTED_sample,
           risk.table = TRUE,
           main = "Exit EWNI")

