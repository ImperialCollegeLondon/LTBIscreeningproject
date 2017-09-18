#
# project: LTBI screening
# N Green
# Oct 2016
#
# estimate by year (impute or proportionally)
# numbers of LTBI to active TB progression for
#   - individuals who leave the UK
#   - remain in EWNI after followup date
#
# commented-out different ways to produce curves
#   - Sutherland
#   - etm estimated
#   - Aldridge Lancet


##TODO: add error to survival estimates


data("activetb_year_pmf_sutherland")
data("incidence_Lancet")


# sensitivity analysis scenario:
# assume _everyone_ has LTBI
# LTBI_status <- rep(1, pop_year)


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


# rescale Sutherland for (lifetime) risk

activetb_year_pmf_sutherland <-
  activetb_year_pmf_sutherland/sum(activetb_year_pmf_sutherland)*LIFETIME_RISK





# survival analysis -------------------------------------------------------

# create final state vectors full sample
# event <- rep(0, nrow(IMPUTED_sample))                          #event-free i.e. censored at followup
# event[IMPUTED_sample$death1] <- 3
# event[IMPUTED_sample$exit_uk1] <- 2
# event[IMPUTED_sample$uk_tb_orig == "1"] <- 1
#
# # etm:: format
# # id from to time_days time
# # 4   9   2  1436.080    4
# # 14  9   0  1961.000    6
#
# data_etm <-
#   data.frame(id = seq_len(nrow(IMPUTED_sample)),
#              from = 9,
#              to = event,
#              time_days = IMPUTED_sample$fup_issdt_days,
#              time = as.numeric(IMPUTED_sample$fup_issdt),
#              LTBI_or_activeTB = IMPUTED_sample$LTBI_or_activeTB) %>%
#   dplyr::filter(LTBI_or_activeTB == TRUE,
#                 time > 0) %>%
#   dplyr::select(-LTBI_or_activeTB)
#
# data_etm_cens_exituk <-
#   data_etm %>%
#   dplyr::mutate(to = ifelse(to == 2, 0, to))
#
# # empirical transition probabilities --------------------------------------
#
# res_etm_cens_exituk <- etm::etm(data = data_etm_cens_exituk,
#                                 state.names = c(9, 1, 3),
#                                 cens.name = 0,
#                                 tra = trans_mat_cens_exituk,
#                                 s = 0)
#
# res_etm_cmprsk_exituk <- etm::etm(data = data_etm,
#                                   state.names = c(9, 1, 2, 3),
#                                   cens.name = 0,
#                                   tra = trans_mat_cmprsk_exituk,
#                                   s = 0)
#
# # cdf -> pmf
# year_prob.activetb_cens_exituk <-
#   c(0, res_etm_cens_exituk$est["9", "1", ]) %>%
#   diff()
#
# year_prob.activetb_cmprsk_exituk <-
#   c(0, res_etm_cmprsk_exituk$est["9", "1", ]) %>%
#   diff()


# fit exponential distn to trans probs & extrapolate  ----------------------------

year_prob.activetb_cmprsk_exituk_0 <- year_prob.activetb_cmprsk_exituk
year_prob.activetb_cmprsk_exituk <- incidence_Lancet$mean/100000

max_years_obs <- length(year_prob.activetb_cmprsk_exituk)

# constant prob at older ages
# asymptote to non-zero
offset <- 0 #0.001

# only use decreasing section of curve
decreasing_years <- seq(from = 4,
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

## Rob's Lancet paper plot + Sutherland
year_prob.activetb_cmprsk_exituk <- c(incidence_Lancet$mean/100000,
                                      activetb_year_pmf_sutherland[max_years_obs + 1:(FUP_MAX_YEAR - max_years_obs)])


# save

save(year_prob.activetb_cens_exituk, file = "ext-data/year_prob.activetb_cens_exituk.RData")
save(year_prob.activetb_cmprsk_exituk, file = "ext-data/year_prob.activetb_cmprsk_exituk.RData")


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

