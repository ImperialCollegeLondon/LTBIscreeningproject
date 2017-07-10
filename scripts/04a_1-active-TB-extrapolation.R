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


data("activetb_year_pmf_sutherland")
data("incidence_Lancet")


# sensitivity analysis scenario:
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

res_etm_cmprsk_exituk <- etm::etm(data = data_etm,
                                  state.names = c(9, 1, 2, 3),
                                  cens.name = 0,
                                  tra = trans_mat_cmprsk_exituk,
                                  s = 0)

# cdf -> pmf
year_prob.activetb_cens_exituk <-
  c(0, res_etm_cens_exituk$est["9", "1", ]) %>%
  diff()

year_prob.activetb_cmprsk_exituk <-
  c(0, res_etm_cmprsk_exituk$est["9", "1", ]) %>%
  diff()


# fit exponential distn to trans probs & extrapolate  ----------------------------

max_years_obs <- length(year_prob.activetb_cmprsk_exituk)

# constant prob at older ages
# asymptote to non-zero
offset <- 0 #0.001

# only use decreasing section of curve
decreasing_years <- seq(from = 7,
                        to = max_years_obs)

extrap_years <- seq(from = max_years_obs + 1,
                    to = FUP_MAX_YEAR)


#  cens exit uk times ----------------------------------------------------

mf.cens.log <- model.frame(formula = logy ~ year,
                           data = data.frame(logy = log(year_prob.activetb_cens_exituk - offset)[decreasing_years],
                                             year = decreasing_years))
fit.cens <- lm(mf.cens.log)

year_prob.cens_exp <- exp(extrap_years * fit.cens$coefficients["year"] +
                            fit.cens$coefficients["(Intercept)"])

## append estimates to observed

## fit + exponential
# year_prob.activetb_cens_exituk <- c(year_prob.activetb_cens_exituk,
#                                     year_prob.cens_exp + offset)
## fit + Sutherland
# year_prob.activetb_cens_exituk <- c(year_prob.activetb_cens_exituk,
#                                     activetb_year_pmf_sutherland[max_years_obs + 1:(FUP_MAX_YEAR - max_years_obs)])
## Sutherland only
year_prob.activetb_cens_exituk <- activetb_year_pmf_sutherland


#  competing risk times ----------------------------------------------------

mf.cmprsk.log <- model.frame(formula = logy ~ year,
                             data = data.frame(logy = log(year_prob.activetb_cmprsk_exituk - offset)[decreasing_years],
                                               year = decreasing_years))
fit.cmprsk <- lm(mf.cmprsk.log)

year_prob.cmprsk_exp <- exp(extrap_years * fit.cmprsk$coefficients["year"] +
                              fit.cmprsk$coefficients["(Intercept)"])

## append estimates to observed

## fit + exponential
# year_prob.activetb_cmprsk_exituk <- c(year_prob.activetb_cmprsk_exituk,
#                                       year_prob.cmprsk_exp + offset)
## fit + Sutherland
# year_prob.activetb_cmprsk_exituk <- c(year_prob.activetb_cmprsk_exituk,
#                                       activetb_year_pmf_sutherland[max_years_obs + 1:(FUP_MAX_YEAR - max_years_obs)])
## Rob's Lancet paper plot
year_prob.activetb_cmprsk_exituk <- c(incidence_Lancet$mean/100000,
                                      activetb_year_pmf_sutherland[max_years_obs + 1:(FUP_MAX_YEAR - max_years_obs)])



# sensitivity analysis:
## constant yearly hazard
#
# year_prob.activetb <- rep(0.001, 100)


# # post-follow-up estimate active TB transition probabilities -----------------
#
# res_etm_cmprsk_exituk_imputed_uk_tb <- etm::etm(data = dat_surv_etm_imputed_uk_tb,
#                                                 state.names = c(9, 1, 2, 3),
#                                                 cens.name = 0,
#                                                 tra = trans_mat_cmprsk_exituk,
#                                                 s = 0)
# # cdf -> pmf
# year_prob.tb_uk_impute <-
#   c(0, 0, res_etm_cmprsk_exituk_imputed_uk_tb$est["9", "1", ]) %>%
#   diff()

