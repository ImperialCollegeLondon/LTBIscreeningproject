
load(pastef(folders$plots$scenario, "ceac.RData"))
load(pastef(folders$plots$scenario, "inmb_levelplot.RData"))
load(pastef(folders$plots$scenario, "prob_ce_gridplot.RData"))


library(gridExtra)
library(cowplot)
library(lattice)
library(grid)

margin <- theme(plot.margin = unit(c(-0.5,-0,-0.5,-0), "cm"))
margin_ceac <- theme(plot.margin = unit(c(0,0.5,0,0.5), "cm"))
font_size <- theme(axis.text = element_text(size = 10),
                   plot.subtitle = element_text(size = 20))

grid.arrange(inmb_levelplot + labs(subtitle = "A") + font_size + margin,
             inmb_levelplot + labs(subtitle = "B") + font_size + margin,
             inmb_levelplot + labs(subtitle = "C") + font_size + margin,
             prob_ce_gridplot + labs(subtitle = "D") + font_size + margin,
             prob_ce_gridplot + labs(subtitle = "E") + font_size + margin,
             prob_ce_gridplot + labs(subtitle = "F") + font_size + margin,
             out + theme(text = element_text(size = 10)) + labs(subtitle = "G") + font_size + margin_ceac,
             out + theme(text = element_text(size = 10)) + labs(subtitle = "H") + font_size + margin_ceac,
             out + theme(text = element_text(size = 10)) + labs(subtitle = "I") + font_size + margin_ceac,
             nrow = 3)

