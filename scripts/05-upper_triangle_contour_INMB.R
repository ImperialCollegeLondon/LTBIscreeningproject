#*******************************************************
# project: LTBI screening
# N Green
# Oct 2017
#
# upper triangle contour INMB
# like paired scatterplots


library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)


levels_range <- seq(0, 250, 5)

## £20,000
s1 <-
  lattice::levelplot(INMB~Agree*Start, subset(pred_INMB_20000, Complete == 70 & Effective == 70),
                     xlab = "Agree (%)", ylab = "Start (%)",
                     at = levels_range,
                     main = "Complete == 70 & Effective == 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s2 <-
  lattice::levelplot(INMB~Agree*Complete, subset(pred_INMB_20000, Start == 70 & Effective == 70),
                     xlab = "Agree (%)", ylab = "Complete (%)",
                     at = levels_range,
                     main = "Start = 70 & Effective = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s3 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_INMB_20000, Start == 70 & Complete == 70),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = levels_range,
                     main = "Start = 70 & Complete = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s4 <-
  lattice::levelplot(INMB~Start*Complete, subset(pred_INMB_20000, Agree == 70 & Effective == 70),
                     xlab = "Start (%)", ylab = "Complete (%)",
                     at = levels_range,
                     main = "Agree = 70 & Effective = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s5 <-
  lattice::levelplot(INMB~Start*Effective, subset(pred_INMB_20000, Agree == 70 & Complete == 70),
                     xlab = "Start (%)", ylab = "Effective (%)",
                     at = levels_range,
                     main = "Agree = 70 & Complete = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s6 <-
  lattice::levelplot(INMB~Complete*Effective, subset(pred_INMB_20000, Agree == 70 & Start == 70),
                     xlab = "Complete (%)", ylab = "Effective (%)",
                     at = levels_range,
                     main = "Agree = 70 & Start = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))

ng <- nullGrob()

print(
  grid.arrange(arrangeGrob(s3, s2, s1),
               arrangeGrob(s5, s4, ng),
               arrangeGrob(s6, ng, ng),
               ncol = 3)
)



g <- arrangeGrob(s3, s5, s6,
                 s2, s4, ng,
                 s1, ng, ng,
                 nrow = 3)

filename <- paste(plots_folder_scenario, "upper_triangle_contour_20000.png", sep = "/")
ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")


## £30,000
s1 <-
  lattice::levelplot(INMB~Agree*Start, subset(pred_INMB_30000, Complete == 70 & Effective == 70),
                     xlab = "Agree (%)", ylab = "Start (%)",
                     at = levels_range,
                     main = "Complete = 70 & Effective = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s2 <-
  lattice::levelplot(INMB~Agree*Complete, subset(pred_INMB_30000, Start == 70 & Effective == 70),
                     xlab = "Agree (%)", ylab = "Complete (%)",
                     at = levels_range,
                     main = "Start = 70 & Effective = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s3 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_INMB_30000, Start == 70 & Complete == 70),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = levels_range,
                     main = "Start = 70 & Complete = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s4 <-
  lattice::levelplot(INMB~Start*Complete, subset(pred_INMB_30000, Agree == 70 & Effective == 70),
                     xlab = "Start (%)", ylab = "Complete (%)",
                     at = levels_range,
                     main = "Agree = 70 & Effective = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s5 <-
  lattice::levelplot(INMB~Start*Effective, subset(pred_INMB_30000, Agree == 70 & Complete == 70),
                     xlab = "Start (%)", ylab = "Effective (%)",
                     at = levels_range,
                     main = "Agree = 70 & Complete = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))
s6 <-
  lattice::levelplot(INMB~Complete*Effective, subset(pred_INMB_30000, Agree == 70 & Start == 70),
                     xlab = "Complete (%)", ylab = "Effective (%)",
                     at = levels_range,
                     main = "Agree = 70 & Start = 70",
                     col.regions = rainbow(n = 70, start = 3/6, end = 1/6))#topo.colors(70))

ng <- nullGrob()

print(
  grid.arrange(arrangeGrob(s3, s2, s1),
               arrangeGrob(s5, s4, ng),
               arrangeGrob(s6, ng, ng),
               ncol = 3)
)


g <- arrangeGrob(s3, s5, s6,
                 s2, s4, ng,
                 s1, ng, ng,
                 nrow = 3)

filename <- paste(plots_folder_scenario, "upper_triangle_contour_30000.png", sep = "/")
ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")


