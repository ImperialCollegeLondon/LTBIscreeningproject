# *****************************************************
# LTBI screening
# N Green
# 2017
#
# cost-effectiveness acceptability curves


# BCEA::ceac.plot(screen.bcea)
#
# BCEA::ceac.plot(screen.bcea, graph = "ggplot2") + theme(legend.position = c(0.2, 0.4)) +
#   geom_abline(slope = 0, intercept = 0.5) +
#   scale_color_discrete(labels = SCENARIO_LABELS) + xlim(10000,30000) +
#   geom_vline(xintercept = 20000)

#  ------------------------------------------------------------------------

my_ceac.plot <- function(screen.bcea) {

  require(RColorBrewer)

  NUM_SCENARIOS <- ncol(screen.bcea$ceac)

  rainbow_cols <-  colorRampPalette(c('red','blue','green'))(NUM_SCENARIOS)

  windows(width = 100, height = 50)

  plot(x = seq(0, 50000, by = 100), y = screen.bcea$ceac[,1],
       ylim = c(0, 1), xlim = c(10000, 30000),
       type = 'l',
       xlab = "Willingness to pay (£)",
       ylab = "Probability cost-effective")

  for (i in 1:NUM_SCENARIOS) {
    # for (i in 9:16) {

    lines(x = seq(0, 50000, by = 100), y = screen.bcea$ceac[,i],
          col = rainbow_cols[i], lty = i)
  }

  legend('topleft',
         legend = 1:NUM_SCENARIOS,
         col = rainbow_cols,
         lty = 1:NUM_SCENARIOS)
  abline(v = 20000, col = "grey")
}


my_ceac.plot(screen.bcea)

