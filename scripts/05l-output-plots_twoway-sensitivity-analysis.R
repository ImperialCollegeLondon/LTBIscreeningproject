#
# 05l-output-plots_twoway-sensitivity-analysis
# two-way sensitivity analysis surface plots



twoway_plot <- function(x, y, z) {

  X_LAB <- deparse(substitute(x))
  Y_LAB <- deparse(substitute(y))

  x_ticks <- unique(x)
  y_ticks <- unique(y)
  dat <- matrix(z, nrow = length(x_ticks))

  lattice::levelplot(dat,
                     row.values = x_ticks, column.values = y_ticks,
                     xlim = c(min(x_ticks), max(x_ticks)), ylim = c(min(y_ticks), max(y_ticks)),
                     xlab = X_LAB, ylab = Y_LAB)
}


if (!exists("scenario_parameter_p")) scenario_parameter_p <- read_excel(parameter_values_file,
                                                                        sheet = "p")
params <- cbind(scenario_parameter_p,
                # 'LTBI test cost' = scenario_parameter_cost$min,
                INMB = calc.INMB(e = e.total, c = c.total, wtp = wtp_threshold),
                ICER = calc.ICER(e = e.total, c = c.total))



twoway_plot(x = params$'Agree to Screen',
            y = params$'Start Treatment',
            z = params$'INMB')

twoway_plot(x = params$'Agree to Screen',
            y = params$'Start Treatment',
            z = params$'ICER')
