#
# project: LTBI screening
# N Green
# Jan 2017
#
# active TB imputation survival plots



plot(diff(cumprob.activetb), type = "h",
     main = "annual probability of active TB progression",
     xlab = "Time from UK entry (years)")


plot(x = 1:length(exituk_tb.years),
     y = exituk_tb.years,
     main = "Histogram of number of\n active TB cases outside of UK",
     ylim = c(0, 200), xlab = "Time from UK entry (years)", type = "h")


plot(issdt_exit_year.tab[-1],
     main = "annual number of recent arrivals cohort leaving UK",
     xlab = "Time from UK entry (years)")


# combined plots
windows(rescale = "R")

par(mar = c(5,4,4,5) + .1)
plot(issdt_exit_year.tab[-1][1:9],
     type = "s", col = "red",
     xlab = "", ylab = "Number leaving UK")

par(new = TRUE)
plot(exituk_tb_year,
     main = "",
     ylim = c(0, 200), lty = 2,
     ylab = "", xlab = "Time from UK entry (years)", type = "s", col = "blue",
     xaxt = "n", yaxt = "n")
axis(4)
mtext("Number of active TB cases of leavers", side = 4, line = 3)
legend("topright", col = c("red","blue"), lty = c(1,2), legend = c("Leave UK","Active TB"))

