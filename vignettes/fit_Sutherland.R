#
# active TB progression probs
# Sutherland journal paper values
#
# N Green


library(readr)

Sutherland <- read_csv("C:/Users/ngreen1/Dropbox/TB/TB progression docs/Sutherland - year-since-infectection__cumul-incidence.csv",
                       col_names = FALSE)

plot(Sutherland$X1, Sutherland$X2, type = "o",
     ylab = "cumulative incidence",
     xlab = "years since infection",
     xlim = c(0, 96),
     ylim = c(0, 15))

prob.log <- model.frame(formula = logy ~ year,
                        data = data.frame(logy = exp(Sutherland$X2)[Sutherland$X1 > 5],
                                          year = Sutherland$X1[Sutherland$X1 > 5]))
fit.lm <- lm(prob.log)

years <- seq(1,100)
prob_estimated <- log(years*fit.lm$coefficients["year"] + fit.lm$coefficients["(Intercept)"])

lines(years, prob_estimated, type = "l", col = "red")

activetb_year_pmf_sutherland <- diff(prob_estimated)/100

# lifetime risk
max(prob_estimated)


save(activetb_year_pmf_sutherland, file = "data/activetb_year_pmf_sutherland")


# add  to ETS plot
plot(years[-1][years[-1] > 7],
     activetb_year_pmf_sutherland[years[-1] > 7],
     type = 'l', col = "orange")



# Ferebee <- read_csv("C:/Users/ngreen1/Dropbox/TB/Ferebee - years-since-contact__incidence.csv",
#                     col_names = FALSE)
#
# plot(Ferebee$X1, Ferebee$X2)






