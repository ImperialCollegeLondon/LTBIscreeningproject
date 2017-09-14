# *****************************************************
# LTBI screening
# N Green
# 2017
#
# cost-effectiveness acceptability curves


# using BCEA package functions --------------------------------------------

# BCEA::ceac.plot(screen.bcea)
#
# BCEA::ceac.plot(screen.bcea, graph = "ggplot2") +
#   theme(legend.position = c(0.2, 0.4)) +
#   geom_abline(slope = 0, intercept = 0.5) +
#   scale_color_discrete(labels = SCENARIO_LABELS) +
#   xlim(10000,30000) +
#   geom_vline(xintercept = 20000)


#  custom function --------------------------------------------------------

filename <- paste(plots_folder_scenario, "ceac.png", sep = "/")

png(filename)

try(
  print(my_ceac.plot(screen.bcea), new_window = TRUE),
  silent = TRUE)

##TODO:
# ggsave(file = filename,
#        width = 30, height = 20, units = "cm")

dev.off()
