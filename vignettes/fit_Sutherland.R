#
# active TB progression probs
# extraplation from
# Sutherland journal paper values
#
# N Green


library(readr)

MAX_SUTHERLAND <- 5

Sutherland <- read_csv("C:/Users/ngreen1/Dropbox/TB/TB progression docs/Sutherland - year-since-infectection__cumul-incidence.csv",
                       col_names = FALSE)


prob.log <- model.frame(formula = logy ~ year,
                        data = data.frame(logy = exp(Sutherland$X2)[Sutherland$X1 > MAX_SUTHERLAND],
                                          year = Sutherland$X1[Sutherland$X1 > MAX_SUTHERLAND]))
fit.lm <- lm(prob.log)

years <- seq(1, 100)
prob_estimated <- log(years*fit.lm$coefficients["year"] + fit.lm$coefficients["(Intercept)"])

Sutherland_yearly <- sapply(1:MAX_SUTHERLAND, function(x)max(Sutherland$X2[Sutherland$X1 < x]))

prob_estimated <- c(Sutherland_yearly,
                    prob_estimated[-(1:MAX_SUTHERLAND)])

activetb_year_pmf_sutherland <- diff(c(0, prob_estimated))/100

# lifetime risk
# max(prob_estimated)


save(activetb_year_pmf_sutherland, file = "data/activetb_year_pmf_sutherland")


# add  to ETS plot
# plot(years[-1][years[-1] > 7],
#      activetb_year_pmf_sutherland[years[-1] > 7],
#      type = 'l', col = "orange")



# Ferebee <- read_csv("C:/Users/ngreen1/Dropbox/TB/Ferebee - years-since-contact__incidence.csv",
#                     col_names = FALSE)
#
# plot(Ferebee$X1, Ferebee$X2)


# plots -------------------------------------------------------------------

plot(x = Sutherland$X1, y = Sutherland$X2,
     type = "p",
     ylab = "cumulative incidence",
     xlab = "years since infection",
     xlim = c(0, 96),
     ylim = c(0, 15))
lines(years, prob_estimated, type = "l", col = "red")

plot(prob_estimated, type = 'o', pch = 19)
points(Sutherland_yearly, col = "red", pch = 19)
legend(x = 80, y = 6, legend = c("Sutherland (1980)", "Exponential extrapolation"),
       col = c("black", "red"), pch = c(19,19), bty = "n")
