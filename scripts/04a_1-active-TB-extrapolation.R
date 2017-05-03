#
# project: LTBI screening
# N Green
# Oct 2016
#
# estimate by year (impute or proportionally)
# numbers of LTBI to active TB progression for
#   - individuals who leave the UK
#   - remain in EWNI after followup date


##TODO: add error to survival estimates


# sensitivity analysis:
# assume _everyone_ has LTBI
# LTBI_status <- rep(1, pop_year)


# estimate active TB transition probabilities -----------------------------

trans_mat_cmprsk_exituk <-
  mstate::trans.comprisk(K = 3,
                         names = c(1, 2, 3)) %>%
  is.na() %>%
  not()

trans_mat_cens_exituk <-
  mstate::trans.comprisk(K = 2,
                         names = c(1, 3)) %>%
  is.na() %>%
  not()


# empirical transition probabilities
res_etm_cens_exituk <- etm::etm(data = data_etm_cens_exituk,
                                state.names = c(9, 1, 3),
                                cens.name = 0,
                                tra = trans_mat_cens_exituk,
                                s = 0)

# tranform from cumulative
year_prob.activetb_cens_exituk <- diff(c(0, 0, res_etm_cens_exituk$est["9","1",]))

plot(x = year_prob.activetb_cens_exituk,
     ylim = c(0, 0.006),
     xlim = c(0, 50),
     type = "o",
     main = "LTBI to active TB annual transition probabilities\n with censoring at exit uk times",
     xlab = "Time (years)",
     ylab = "Probability")


res_etm_cmprsk_exituk <- etm::etm(data = data_etm,
                                  state.names = c(9, 1, 2, 3),
                                  cens.name = 0,
                                  tra = trans_mat_cmprsk_exituk,
                                  s = 0)

# transform from cumulative
year_prob.activetb_cmprsk_exituk <- diff(c(0, 0, res_etm_cmprsk_exituk$est["9","1",]))

lines(x = year_prob.activetb_cmprsk_exituk,
     ylim = c(0, 0.006),
     xlim = c(0, 50),
     type = "o",
     main = "LTBI to active TB annual transition probabilities",
     xlab = "Time (years)",
     ylab = "Probability", col = "red")



# fit exponential distn to trans probs & extrapolate  ----------------------------

fup_max_year <- 100

max_years_obs <- length(year_prob.activetb_cmprsk_exituk)

# constant prob at older ages
# asymptote to non-zero
offset <- 0 #0.001

# only use decreasing section of curve
decreasing_years <- seq(from = 7,
                        to = max_years_obs)


#  cens exit uk times ----------------------------------------------------


year_prob.activetb.log <- model.frame(formula = logy ~ year,
                                      data = data.frame(logy = log(year_prob.activetb_cens_exituk - offset)[decreasing_years],
                                                        year = decreasing_years))
fit.lm <- lm(year_prob.activetb.log)

years <- seq(from = max_years_obs + 1,
             to = fup_max_year)

year_prob.activetb_estimated <- exp(years*fit.lm$coefficients["year"] + fit.lm$coefficients["(Intercept)"])

# append estimates to observed
year_prob.activetb_cens_exituk <- c(year_prob.activetb_cens_exituk,
                                    year_prob.activetb_estimated + offset)


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


#  competing risk exit uk times ----------------------------------------------------

year_prob.activetb.log <- model.frame(formula = logy ~ year,
                                      data = data.frame(logy = log(year_prob.activetb_cmprsk_exituk - offset)[decreasing_years],
                                                        year = decreasing_years))
fit.lm <- lm(year_prob.activetb.log)

years <- seq(from = max_years_obs + 1,
             to = fup_max_year)

year_prob.activetb_estimated <- exp(years*fit.lm$coefficients["year"] + fit.lm$coefficients["(Intercept)"])

# append estimates to observed
year_prob.activetb_cmprsk_exituk <- c(year_prob.activetb_cmprsk_exituk,
                                      year_prob.activetb_estimated + offset)


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

# sensitivity analysis:
## constant yearly hazard
#
# year_prob.activetb <- rep(0.001, 100)


# post-follow-up estimate active TB transition probabilities -----------------------------

trans_mat_cmprsk_exituk <-
  mstate::trans.comprisk(K = 3,
                         names = c(1, 2, 3)) %>%
  is.na() %>%
  not()


res_etm_cmprsk_exituk <- etm::etm(data = dat_surv_imputed_uk_tb,
                                  state.names = c(9, 1, 2, 3),
                                  cens.name = 0,
                                  tra = trans_mat_cmprsk_exituk,
                                  s = 0)
x11()
plot(NULL, type = 'n', xlim = c(0, 80), ylim = c(0, 1), ylab = "Transition Probability", xlab = "Time (years)")
lines(res_etm_cmprsk_exituk, col = rainbow(4, start = .6, end = .1))
legend("topright", legend = c("Event-free", "Active TB", "Exit EWNI", "Death"), lty = c(1,2,3,4), bty = 'n', col = rainbow(4, start = .6, end = .1))

# transform from cumulative
year_prob.tb_uk_impute <- diff(c(0, 0, res_etm_cmprsk_exituk$est["9","1",]))

plot(x = supsmu(seq_along(year_prob.tb_uk_impute),
                year_prob.tb_uk_impute - 0.0001, bass = 5),
     xlim = c(0,100),
     ylim = c(0, 0.003),
     type = "o",
     main = "LTBI to active TB annual transition probabilities",
     xlab = "Time (years)",
     ylab = "Probability", col = "blue")



