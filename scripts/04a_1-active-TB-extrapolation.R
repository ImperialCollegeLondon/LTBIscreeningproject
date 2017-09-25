# **************************************
# project: LTBI screening
# N Green
# Oct 2016
#


data("activetb_year_pmf_sutherland")
data("incidence_Lancet")


# rescale Sutherland for (lifetime) risk
# activetb_year_pmf_sutherland <-
#   activetb_year_pmf_sutherland/sum(activetb_year_pmf_sutherland)*LIFETIME_RISK


# fit exponential distn & extrapolate  ----------------------------

Lancet_prob <- incidence_Lancet$mean/100000

max_years_obs <- length(Lancet_prob)

# only use decreasing section of curve
decreasing_years <- seq(from = 4,
                        to = max_years_obs)

extrap_years <- seq(from = max_years_obs + 1,
                    to = FUP_MAX_YEAR)

mf.cens.log <- model.frame(formula = logy ~ year,
                           data = data.frame(logy = log(Lancet_prob)[decreasing_years],
                                             year = decreasing_years))
fit.cens <- lm(mf.cens.log)

fit_year_prob <- exp(extrap_years * fit.cens$coefficients["year"] +
                   fit.cens$coefficients["(Intercept)"])

names(fit_year_prob) <- extrap_years - 1


# append estimates to observed --------------------------------------------
## Rob's Lancet paper plot + Sutherland

res_year_prob <- c(Lancet_prob,
                   activetb_year_pmf_sutherland[max_years_obs:(FUP_MAX_YEAR - max_years_obs)])


# plots -------------------------------------------------------------------

# plot(incidence_Lancet$year, Lancet_prob, xlim = c(0,100), type = 'l', ylim = c(0, 0.003))
# lines(as.numeric(names(fit_year_prob)), fit_year_prob, col = "blue")
# lines(extrap_years - 1, activetb_year_pmf_sutherland[extrap_years - 1], col = "red")
# lines(1:length(res_year_prob) - 1, res_year_prob, col = "green")
# abline(v = 0:20, col = "gray", lty = 3)


# save
year_prob.activetb_cens_exituk <- year_prob.activetb_cmprsk_exituk <- res_year_prob #assume the same

save(year_prob.activetb_cens_exituk, file = "ext-data/year_prob.activetb_cens_exituk.RData")
save(year_prob.activetb_cmprsk_exituk, file = "ext-data/year_prob.activetb_cmprsk_exituk.RData")

