
#' ---
#' title: "LTBI screening model:
#' combine output plots in a grid for manuscript"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


library(gridExtra)
library(cowplot)
library(lattice)
library(grid)


load(here::here("output", "plots", "runs_5", "5.1", "ceac.RData"))
load(here::here("output", "plots", "runs_5", "5.1", "CE_plane.RData"))
load(here::here("output", "plots", "runs_5", "5.1", "INMB_boxplot.RData"))
load(here::here("output", "plots", "runs_5", "5.1", "INMB_hist.RData"))


margin <- theme(plot.margin = unit(c(0,0,0,0), "cm"))
font_size <- theme(axis.text = element_text(size = 10),
                   plot.subtitle = element_text(size = 20))

p <-
  grid.arrange(ceac + labs(subtitle = "A") + theme(text = element_text(size = 15)),
               ceplane + labs(subtitle = "B") + theme(text = element_text(size = 15)) + coord_cartesian(ylim = c(0, 200), xlim = c(0, 0.01)),
               INMB_boxplot + labs(subtitle = "C") + theme(text = element_text(size = 15)) + xlab("Targetted subgroup") + scale_x_discrete(labels = c("25" = "All", "50" = ">150/100,000", "100" = ">250/100,000")),
               INMB_hist + labs(subtitle = "D") + theme(text = element_text(size = 15)),
               nrow = 2)
p

ggplot2::ggsave(file = here::here("output", "plots", "manuscript grid of plots target 25GBP.png"),
                plot = p,
                width = 35, height = 35, units = "cm")

# -------

load(here::here("output", "plots", "runs_5", "5.2", "ceac.RData"))
load(here::here("output", "plots", "runs_5", "5.2", "CE_plane.RData"))
load(here::here("output", "plots", "runs_5", "5.2", "INMB_boxplot.RData"))
load(here::here("output", "plots", "runs_5", "5.2", "INMB_hist.RData"))

p <-
  grid.arrange(ceac + labs(subtitle = "A") + theme(text = element_text(size = 15)),
               ceplane + labs(subtitle = "B") + theme(text = element_text(size = 15)) + coord_cartesian(ylim = c(0, 200), xlim = c(0, 0.01)),
               INMB_boxplot + labs(subtitle = "C") + theme(text = element_text(size = 15)) + xlab("Targetted subgroup") + scale_x_discrete(labels = c("25" = "All", "50" = ">150/100,000", "100" = ">250/100,000")),
               INMB_hist + labs(subtitle = "D") + theme(text = element_text(size = 15)),
               nrow = 2)
p

ggplot2::ggsave(file = here::here("output", "plots", "manuscript grid of plots target 50GBP.png"),
                plot = p,
                width = 35, height = 35, units = "cm")

# -------

load(here::here("output", "plots", "runs_5", "5.3", "ceac.RData"))
load(here::here("output", "plots", "runs_5", "5.3", "CE_plane.RData"))
load(here::here("output", "plots", "runs_5", "5.3", "INMB_boxplot.RData"))
load(here::here("output", "plots", "runs_5", "5.3", "INMB_hist.RData"))

p <-
  grid.arrange(ceac + labs(subtitle = "A") + theme(text = element_text(size = 15)),
               ceplane + labs(subtitle = "B") + theme(text = element_text(size = 15)) + coord_cartesian(ylim = c(0, 200), xlim = c(0, 0.01)),
               INMB_boxplot + labs(subtitle = "C") + theme(text = element_text(size = 15)) + xlab("Targetted subgroup") + scale_x_discrete(labels = c("25" = "All", "50" = ">150/100,000", "100" = ">250/100,000")),
               INMB_hist + labs(subtitle = "D") + theme(text = element_text(size = 15)),
               nrow = 2)
p

ggplot2::ggsave(file = here::here("output", "plots", "manuscript grid of plots target 100GBP.png"),
                plot = p,
                width = 35, height = 35, units = "cm")

