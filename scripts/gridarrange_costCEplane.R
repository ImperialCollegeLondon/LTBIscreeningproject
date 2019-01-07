

library(gridExtra)
library(cowplot)
library(lattice)
library(grid)

load(paste(folders$plots$scenario, "ridgeplot.RData", sep = "/"))
load(paste(folders$plots$scenario, "CE_plane.RData", sep = "/"))


font_size <- theme(axis.text = element_text(size = 10),
                   plot.subtitle = element_text(size = 20))

p <-
  grid.arrange(ceplane + coord_cartesian(ylim = c(0, 200), xlim = c(0, 0.005)) + labs(subtitle = "A") + font_size + theme(plot.margin = unit(c(0,0.5,0.1,0.5), "cm")),
               ridgeplot + labs(subtitle = "B") + font_size + theme(plot.margin = unit(c(0.5,0.2,0,0), "cm")),
               nrow = 1)

ggplot2::ggsave(file = here::here("output", "plots", "manuscript baseline ceplane.png"),
                plot = p,
                width = 45, height = 30, units = "cm")
