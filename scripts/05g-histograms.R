#
# project: LTBI screening
# N Green
# Oct 2016
#
# histogram plots
# this is only useful for the complete set of scenarios


calc.INMB(e = e.total,
          c = c.total,
          wtp = wtp_threshold) %>%
  hist(breaks = 10)

calc.ICER(e = e.total,
          c = c.total) %>%
  hist()
