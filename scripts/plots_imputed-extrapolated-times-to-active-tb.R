#
# project: LTBI screening
# N Green
# April 2017
#
# plots of number of active tb cases over time


hist(ceiling(IMPUTED_sample$exituk_tb.years), col = "blue",
     breaks = 150)#, xlim = c(0, 50), ylim = c(0,25),
     # xlab = "year", main = "")

hist(ceiling(IMPUTED_sample$rNotificationDate_issdt.years), add = T, col = "green",
     breaks = 150)#, xlim = c(0, 50), ylim = c(0,25),
# xlab = "year", main = "")

hist(ceiling(IMPUTED_sample$rNotificationDate_issdt.years[IMPUTED_sample$uk_tb_orig == 1]), add = T,
     breaks = 12, col = "red")#, xlim = c(0, 50), ylim = c(0,25),
# xlab = "year", main = "")

plot(x = 0:19,
     y = num_uk_tb_year[1:20],
     type = "o", ylim = c(0, 85),
     lty = 1,
     xlab = "Time (year)", ylab = "Frequency",
     main = "Active TB cases")

lines(num_exituk_tb_year, type = 'l', xlim = c(0, 20),
      col = "red") #rgb(0, 0, 0, 0.5))

# lines(x = seq_along(obs_uk_tb_year) - 1,
#       y = obs_uk_tb_year,
#       type = "o",
#       col = "red")

lines(x = seq_along(num_exituk_tb_year) - 1,
      y = num_exituk_tb_year,
      col = "green",
      lty = 2,
      type = 'o')

lines(x = seq_along(num_all_tb_year) - 1,
      y = num_all_tb_year,
      col = "blue",
      lty = 3,
      type = 'o')

legend("topright",
       # legend = c("Observed", "Estimated uk", "Estimated exit", "Total"),
       # col = c("red", "black", "green", "blue"), lty = 1)
legend = c("Estimated uk", "Estimated exit", "Total"),
col = c("black", "green", "blue"), lty = c(1,2,3))


# multiple samples
# for (i in 1:5) {
#
#   exituk_tb.years <- sim_aTB_times(pop = pop_year,
#                                    data = cohort,
#                                    prob = year_prob.activetb_cens_exituk)
#
#   x <- hist(exituk_tb.years,
#             breaks = 100, xlim = c(0, 20), ylim = c(0,25),
#             xlab = "year", main = "", plot = F)
#
#   points(x$mids[x$counts > 0] + rnorm(sum(x$counts > 0), 0, 0.5),
#          x$counts[x$counts > 0] + rnorm(sum(x$counts > 0), 0, 0.5),
#          pch = 16, col = rgb(0, 0, 0, 0.3))
# }
