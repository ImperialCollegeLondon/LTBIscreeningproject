#
# active TB progression probs
# upper_bound journal paper values
#
# N Green


library(readr)

upper_bound <- read_csv("C:/Users/ngreen1/Dropbox/TB/TB progression docs/upper-75pc.csv",
                        col_names = FALSE)

plot(upper_bound$X1, upper_bound$X2, type = "o",
     ylab = "cumulative incidence",
     xlab = "years since infection",
     xlim = c(0, 53),
     ylim = c(0, 25))

prob.log <- model.frame(formula = logy ~ year,
                        data = data.frame(logy = exp(upper_bound$X2)[upper_bound$X1 > 5],
                                          year = upper_bound$X1[upper_bound$X1 > 5]))
fit.lm <- lm(prob.log)

years <- seq(1,100)
prob_estimated <- log(years*fit.lm$coefficients["year"] + fit.lm$coefficients["(Intercept)"])

lines(years, prob_estimated, type = "l", col = "red")

activetb_year_pmf_upper_bound <- diff(prob_estimated)/100

# lifetime risk
max(prob_estimated, na.rm = T)


save(activetb_year_pmf_upper_bound, file = "data/activetb_year_pmf_upper_bound")


# add  to ETS plot
plot(years[-1][years[-1] > 7],
     activetb_year_pmf_upper_bound[years[-1] > 7],
     type = 'l', col = "orange")



# Ferebee <- read_csv("C:/Users/ngreen1/Dropbox/TB/Ferebee - years-since-contact__incidence.csv",
#                     col_names = FALSE)
#
# plot(Ferebee$X1, Ferebee$X2)






