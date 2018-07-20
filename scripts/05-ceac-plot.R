# *****************************************************
# LTBI screening
# N Green
# 2017
#
# cost-effectiveness acceptability curves

##TODO: save separate files for subsets (possibly single) lines


# BCEA::ceac.plot(screen.bcea, graph = "ggplot2") +
#   theme(legend.position = c(0.2, 0.4)) +
#   geom_abline(slope = 0, intercept = 0.5) +
#   scale_color_discrete(labels = SCENARIO_LABELS) +
#   xlim(10000,30000) +
#   geom_vline(xintercept = 20000)


#  custom function --------------------------------------------------------

# filename <- paste(folders$plots$scenario, "ceac.png", sep = "/")
filename <- paste(folders$plots$scenario, "ceac.png", sep = "/")

png(filename, width = 400, height = 350, res = 45)

# for (i in seq_len(screen.bcea$n.comparators)) {
try(
  print(
    BCEA::ceac.plot(screen.bcea)))
    # my_ceac.plot(screen.bcea)))#, new_window = TRUE)))
# }

dev.off()


##TODO:
# ggplot2::ggsave(file = filename,
#        width = 30, height = 20, units = "cm")


