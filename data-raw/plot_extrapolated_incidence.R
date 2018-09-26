
#' ---
#' title: "LTBI screening model:
#' plot extrapolated exponential vs Sutherland incidence curves"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

library(LTBIscreeningproject)

data("p_incid_sutherland")
data("incidence_Lancet")

# rescale Sutherland for (lifetime) risk
# activetb_year_pmf_sutherland <-
#   activetb_year_pmf_sutherland/sum(activetb_year_pmf_sutherland)*LIFETIME_RISK

FUP_MAX_YEAR <- 100

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
                   p_incid_sutherland[max_years_obs:(FUP_MAX_YEAR - max_years_obs)])


# plots -------------------------------------------------------------------

x11()
plot(incidence_Lancet$year, Lancet_prob,
     xlim = c(0,100), ylim = c(0, 0.003),
     type = 'l', xlab = "Years", ylab = "Probability")
lines(as.numeric(names(fit_year_prob)), fit_year_prob, col = "blue")
lines(1:length(res_year_prob) - 1, res_year_prob, col = "green")
lines(extrap_years - 1, p_incid_sutherland[extrap_years - 1], col = "red")
# abline(v = 0:20, col = "gray", lty = 3)
legend("topright", legend = c("Lancet", "Exponential", "Sutherland"), col = c("green", "blue", "red"), lty = c(1,1,1))




