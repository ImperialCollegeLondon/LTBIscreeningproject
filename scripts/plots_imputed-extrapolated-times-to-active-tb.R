#
# project: LTBI screening
# N Green
# April 2017
#
# plots


hist(IMPUTED_sample_year_cohort$exituk_tb.years,
     breaks = 50, xlim = c(0, 50), ylim = c(0,25),
     xlab = "year", main = "")

lines(num_exituk_tb_year, type = 'l', xlim = c(0, 20),
      col = "red") #rgb(0, 0, 0, 0.5))

plot(x = 0:19,
     y = num_uk_tb_year[1:20],
     type = "o", ylim = c(0, 85),
     lty = 1,
     xlab = "Time (year)", ylab = "Frequency",
     main = "Active TB cases")

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


