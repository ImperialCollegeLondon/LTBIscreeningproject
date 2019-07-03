
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
library(ggthemes)


load(here::here("output", "plots", "runs_6_25GBP", "policy_003", "ceac.RData"))
load(here::here("output", "plots", "runs_6_25GBP", "policy_003", "inmb_levelplot.RData"))
load(here::here("output", "plots", "runs_6_25GBP", "policy_003", "prob_ce_gridplot.RData"))

out_25 <- out
inmb_levelplot_25 <- inmb_levelplot
prob_ce_gridplot_25 <- prob_ce_gridplot

load(here::here("output", "plots", "runs_6_50GBP", "policy_003", "ceac.RData"))
load(here::here("output", "plots", "runs_6_50GBP", "policy_003", "inmb_levelplot.RData"))
load(here::here("output", "plots", "runs_6_50GBP", "policy_003", "prob_ce_gridplot.RData"))

out_50 <- out
inmb_levelplot_50 <- inmb_levelplot
prob_ce_gridplot_50 <- prob_ce_gridplot

load(here::here("output", "plots", "runs_6_100GBP", "policy_003", "ceac.RData"))
load(here::here("output", "plots", "runs_6_100GBP", "policy_003", "inmb_levelplot.RData"))
load(here::here("output", "plots", "runs_6_100GBP", "policy_003", "prob_ce_gridplot.RData"))

out_100 <- out
inmb_levelplot_100 <- inmb_levelplot
prob_ce_gridplot_100 <- prob_ce_gridplot


margin <- theme(plot.margin = unit(c(0,0,0,0), "cm"))
margin_ceac <- theme(plot.margin = unit(c(0,0.5,0,0.5), "cm"))
font_size <- theme(axis.text = element_text(size = 10),
                   plot.subtitle = element_text(size = 20))

pal2 <- tableau_div_gradient_pal("Classic Red-White-Green")(seq(0, 1, length = 12))

p <-
  grid.arrange(
    arrangeGrob(
      inmb_levelplot_100 + labs(subtitle = "A") + font_size + margin + guides(fill = guide_legend("INMB\n(£/person)")),
      prob_ce_gridplot_100 + labs(subtitle = "B") + font_size + theme(plot.margin = unit(c(0,1,0,0), "cm")) + guides(fill = guide_legend("Probability\ncost-effective")) + guides(fill = FALSE) + scale_fill_gradientn(colours = pal2, limits = c(0, 1)),
      out_100 + theme(text = element_text(size = 10)) + labs(subtitle = "C") + font_size + margin_ceac, top = "£100"),
    arrangeGrob(
      inmb_levelplot_50 + labs(subtitle = "D") + font_size + margin + guides(fill = guide_legend("INMB\n(£/person)")) + ylab(""),
      prob_ce_gridplot_50 + labs(subtitle = "E") + font_size + theme(plot.margin = unit(c(0,2,0,-1), "cm")) + guides(fill = guide_legend("Probability\ncost-effective")) + guides(fill = FALSE) + ylab("") + scale_fill_gradientn(colours = pal2, limits = c(0, 1)),
      out_50 + theme(text = element_text(size = 10)) + labs(subtitle = "F") + font_size + margin_ceac + ylab(""), top = "£50"),
    arrangeGrob(
      inmb_levelplot_25 + labs(subtitle = "G") + font_size + margin + guides(fill = guide_legend("INMB\n(£/person)")) + ylab(""),
      prob_ce_gridplot_25 + labs(subtitle = "H") + font_size + theme(plot.margin = unit(c(0,0,0,-2), "cm")) + guides(fill = guide_legend("Probability\ncost-effective")) + ylab("") + scale_fill_gradientn(colours = pal2, limits = c(0, 1)),
      out_25 + theme(text = element_text(size = 10)) + labs(subtitle = "I") + font_size + margin_ceac + ylab(""), top = "£25"),
    ncol = 3)

p

ggplot2::ggsave(file = here::here("output", "plots", "manuscript grid of plots.png"),
                plot = p,
                width = 35, height = 35, units = "cm")
