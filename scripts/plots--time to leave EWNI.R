#
# project: LTBI screening
# N Green
# April 2018
#
# time to exit EWNI plots





hist(IMPUTED_sample$date_exit_uk1_issdt.years, 100,
     main = "", xlab = "Time (years)")
abline(v = 5, col = "red", lty = 2)

hist(cohort$date_exit_uk1_issdt.years, 100,
     main = "", xlab = "Time (years)")







