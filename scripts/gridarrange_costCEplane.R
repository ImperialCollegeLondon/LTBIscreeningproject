

library(gridExtra)
library(cowplot)
library(lattice)
library(grid)

load("C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/ext-data/runs_1.1/18_to_35_in_2009/policy_003/folders.RData")

load(paste(folders$plots$scenario, "ridgeplot.RData", sep = "/"))
load(paste(folders$plots$scenario, "CE_plane.RData", sep = "/"))


font_size <- theme(axis.text = element_text(size = 10),
                   plot.subtitle = element_text(size = 20))

p <-
  grid.arrange(ceplane + coord_cartesian(ylim = c(0, 200), xlim = c(0, 0.005)) + labs(subtitle = "A") + font_size + theme(plot.margin = unit(c(0,0.5,0.1,0.5), "cm")) + geom_abline(slope = 20000, colour = "white", lty = 2),
               ridgeplot + labs(subtitle = "B") + font_size + theme(plot.margin = unit(c(0.5,0.2,0,0), "cm")),
               nrow = 1)

ggplot2::ggsave(file = here::here("output", "plots", "manuscript baseline ceplane.png"),
                plot = p,
                width = 45, height = 30, units = "cm")
