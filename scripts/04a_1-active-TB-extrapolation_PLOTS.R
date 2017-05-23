


## plots


### pdf

plot(x = year_prob.activetb_cens_exituk,
     ylim = c(0, 0.003),
     xlim = c(0, 100),
     type = "o",
     main = "LTBI to active TB annual transition probabilities",#\n with censoring at exit uk times",
     xlab = "Time (years)",
     ylab = "Probability")

lines(x = year_prob.activetb_cmprsk_exituk,
      ylim = c(0, 0.006),
      xlim = c(0, 50),
      type = "o",
      main = "LTBI to active TB annual transition probabilities",
      xlab = "Time (years)",
      ylab = "Probability", col = "red")

legend("topright", legend = c("with censoring", "competing risk"), col = c("black", "red"), lty = 1, bty = 'n')


### cdf

plot(x = res_etm_cens_exituk$est["9","1",],
     # ylim = c(0, 0.006),
     # xlim = c(0, 50),
     type = "o",
     main = "LTBI to active TB cumulative transition probabilities",#\n with censoring at exit uk times",
     xlab = "Time (years)",
     ylab = "Probability")

lines(x = res_etm_cmprsk_exituk$est["9","1",],
      type = "o",
      col = "red")

legend("topleft", legend = c("with censoring", "competing risk"), col = c("black", "red"), lty = 1, bty = 'n')



plot(x = seq_along(year_prob.activetb_cens_exituk) - 1,
     y = year_prob.activetb_cens_exituk,
     type = "o",
     xlim = c(0, 20),
     ylim = c(0, 0.003),
     main = "LTBI to active TB annual transition probabilities\n with censoring at exit uk times",
     xlab = "Time (year)", ylab = "Probability")
lines(x = seq_along(year_prob.activetb_cens_exituk[1:max_years_obs]) - 1,
      y = year_prob.activetb_cens_exituk[1:max_years_obs],
      type = "o",
      col = 2) #observed
abline(h = 0.001,
       col = "blue")
segments(x0 = max_years_obs - 1, y0 = year_prob.activetb_cens_exituk[max_years_obs],
         x1 = fup_max_year, y1 = year_prob.activetb_cens_exituk[max_years_obs],
         col = "green")



plot(x = seq_along(year_prob.activetb_cmprsk_exituk) - 1,
     y = year_prob.activetb_cmprsk_exituk,
     type = "o",
     xlim = c(0, 20),
     ylim = c(0, 0.003),
     main = "LTBI to active TB annual transition probabilities\n competing risks model",
     xlab = "Time (year)", ylab = "Probability")
lines(x = seq_along(year_prob.activetb_cmprsk_exituk[1:max_years_obs]) - 1,
      y = year_prob.activetb_cmprsk_exituk[1:max_years_obs],
      type = "o",
      col = 2) #observed
abline(h = 0.001,
       col = "blue")
segments(x0 = max_years_obs - 1, y0 = year_prob.activetb_cmprsk_exituk[max_years_obs],
         x1 = fup_max_year, y1 = year_prob.activetb_cmprsk_exituk[max_years_obs],
         col = "green")




x11()
plot(NULL, type = 'n',
     xlim = c(0, 80), ylim = c(0, 1),
     xlab = "Time (years)", ylab = "Transition Probability")
lines(res_etm_cmprsk_exituk_imputed_uk_tb,
      col = rainbow(4, start = 0.6, end = 0.1))
legend("topright",
       legend = c("Event-free", "Active TB", "Exit EWNI", "Death"), lty = c(1, 2, 3, 4), bty = 'n',
       col = rainbow(4, start = 0.6, end = 0.1))


plot(x = supsmu(seq_along(year_prob.tb_uk_impute),
                year_prob.tb_uk_impute, bass = 5),
     xlim = c(0, 100), ylim = c(0, 0.003),
     type = "o",
     main = "LTBI to active TB annual transition probabilities",
     xlab = "Time (years)", ylab = "Probability",
     col = "blue")
