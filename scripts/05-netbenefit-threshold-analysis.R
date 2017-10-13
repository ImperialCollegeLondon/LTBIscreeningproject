# ************************************
# LTBI screening
# N Green
# Sept 2017
#
# net benefit threshold analysis plots
# predict using fit model


library(directlabels)
library(cowplot)
library(lattice)
library(gridExtra)


# pred_grid_values <- seq(50, 100, 10)
# plot_grid_values <- seq(50, 100, 10)

pred_grid_values <- seq(50, 100, 1)
plot_grid_values <- c(50, 100)


# input parameter values

## full-factorial grid
pred_data <-
  expand.grid("Agree" = pred_grid_values,
              "Start" = pred_grid_values,
              "Complete" = pred_grid_values,
              "Effective" = pred_grid_values,
              "policy" = c("screened", "statusquo"))

pred_data_20000 <- pred_data_30000 <- pred_data

## from file
# scenario_parameter_p <- readxl::read_excel("data/scenario-parameter-values_main.xlsx", sheet = "p")
# scenario_parameter_p <- readxl::read_excel("data/scenario-parameter-values_fullfactorial_QFT-GIT.xlsx", sheet = "p")
#
# pred_data <-
#   scenario_parameter_p[ ,c("Agree to Screen", "Start Treatment", "Complete Treatment", "Effective")] %>%
#   set_names("Agree", "Start", "Complete", "Effective") * 100
#
# pred_data <- pred_data[!duplicated(pred_data), ]
#
# pred_data_20000 <- pred_data_30000 <- bind_rows(list("screened" = pred_data, "statusquo" = pred_data), .id = "policy")


# predictions -------------------------------------------------------------

pred_data_30000$pred <- predict(lm_multi$`30000`, pred_data_30000, type = "response")
pred_data_20000$pred <- predict(lm_multi$`20000`, pred_data_20000, type = "response")

# wide format with INMB
pred_data_30000 <-
  tidyr::spread(pred_data_30000, policy, pred) %>%
  mutate(INMB = screened - statusquo,
         CE = INMB > 0) %>%
  arrange(Effective, Agree, Complete, Start)

pred_data_20000 <-
  tidyr::spread(pred_data_20000, policy, pred) %>%
  mutate(INMB = screened - statusquo,
         CE = INMB > 0) %>%
  arrange(Effective, Agree, Complete, Start)

# keep plotting values only
plot_data_30000 <- dplyr::filter(pred_data_30000,
                                 Start %in% plot_grid_values,
                                 Complete %in% plot_grid_values)

plot_data_20000 <- dplyr::filter(pred_data_20000,
                                 Start %in% plot_grid_values,
                                 Complete %in% plot_grid_values)

# long format wrt policy
plot_data <- dplyr::filter(pred_data,
                           Start %in% plot_grid_values,
                           Complete %in% plot_grid_values)


# NMB contour plots ---------------------------------------------------------

p <- ggplot2::ggplot(plot_data_20000, aes(x = Agree, y = Effective, z = INMB)) +
  facet_wrap(Start ~ Complete, labeller = label_both) +
  scale_fill_gradient(limits = range(plot_data_20000$INMB), high = 'white', low = 'red') +
  geom_contour(aes(colour = ..level..), size = 1.2) +
  # stat_contour(geom = "polygon", aes(fill = ..level..)) +
  # coord_cartesian(xlim = c(min(plot_data$Agree), max(plot_data$Agree)),
  #                 ylim = c(min(plot_data$Effective), max(plot_data$Effective))) +
  # scale_colour_gradient(guide = 'none') +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = "none")
  # stat_contour(breaks = 0)

print(
  direct.label(p, list("bottom.pieces", colour = 'black'))
)

filename <- paste(plots_folder_scenario, "NMB_contours_grid_20000.png", sep = "/")
ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")


# cost-effectiveness boundary plot ----------------------------------------

print(
  ggplot2::ggplot(plot_data_20000, aes(x = Agree, y = Effective)) +
    facet_wrap(Start ~ Complete, labeller = label_both) +
    geom_point(aes(colour = factor(CE)), size = 2, shape = 15) +
    # geom_polygon(aes(fill = CE)) + ##TODO:
    theme(legend.position = "none") +
    scale_colour_grey(start = 0.7, end = 0.3)
)

filename <- paste(plots_folder_scenario, "CE_boundary_grid_20000.png", sep = "/")
ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")


# NMB contour plots ---------------------------------------------------------

p <- ggplot2::ggplot(plot_data_30000, aes(x = Agree, y = Effective, z = INMB)) +
  facet_wrap(Start ~ Complete, labeller = label_both) +
  scale_fill_gradient(limits = range(plot_data_30000$INMB), high = 'white', low = 'red') +
  geom_contour(aes(colour = ..level..), size = 1.2) +
  # stat_contour(geom = "polygon", aes(fill = ..level..)) +
  # coord_cartesian(xlim = c(min(plot_data$Agree), max(plot_data$Agree)),
  #                 ylim = c(min(plot_data$Effective), max(plot_data$Effective))) +
  # scale_colour_gradient(guide = 'none') +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = "none")
# stat_contour(breaks = 0)

print(
  direct.label(p, list("bottom.pieces", colour = 'black'))
)

filename <- paste(plots_folder_scenario, "NMB_contours_grid_30000.png", sep = "/")
ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")


# cost-effectiveness boundary plot ----------------------------------------

print(
  ggplot2::ggplot(plot_data_30000, aes(x = Agree, y = Effective)) +
    facet_wrap(Start ~ Complete, labeller = label_both) +
    geom_point(aes(colour = factor(CE)), size = 2, shape = 15) +
    # geom_polygon(aes(fill = CE)) + ##TODO:
    theme(legend.position = "none") +
    scale_colour_grey(start = 0.7, end = 0.3)
)

filename <- paste(plots_folder_scenario, "CE_boundary_grid_30000.png", sep = "/")
ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")



# base graphics filled contour plots ---------------------------------------

## £20,000

s1 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_data_20000, Start == 50 & Complete == 50),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = seq(-50, 50, 5),
                     main = "Start = 50 & Complete = 50",
                     col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s2 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_data_20000, Start == 50 & Complete == 100),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = seq(-50, 50, 5),
                     main = "Start = 50 & Complete== 100",
                     col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s3 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_data_20000, Start == 100 & Complete == 50),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = seq(-50, 50, 5),
                     main = "Start = 100 & Complete = 50",
                     col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s4 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_data_20000, Start == 100 & Complete == 100),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = seq(-50, 50, 5),
                     main = "Start = 100 & Complete = 100",
                     col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
print(
  grid.arrange(arrangeGrob(s1, s2),
               arrangeGrob(s3, s4),
               ncol = 2)
)

g <- arrangeGrob(s1, s2, s3, s4, nrow = 2)

filename <- paste(plots_folder_scenario, "filled_contour_grid_20000.png", sep = "/")
ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")


## £30,000

s1 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_data_30000, Start == 50 & Complete == 50),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = seq(-50, 50, 5),
                     main = "Start = 50 & Complete = 50",
                     col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s2 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_data_30000, Start == 50 & Complete == 100),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = seq(-50, 50, 5),
                     main = "Start = 50 & Complete== 100",
                     col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s3 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_data_30000, Start == 100 & Complete == 50),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = seq(-50, 50, 5),
                     main = "Start = 100 & Complete = 50",
                     col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s4 <-
  lattice::levelplot(INMB~Agree*Effective, subset(pred_data_30000, Start == 100 & Complete == 100),
                     xlab = "Agree (%)", ylab = "Effective (%)",
                     at = seq(-50, 50, 5),
                     main = "Start = 100 & Complete = 100",
                     col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
print(
  grid.arrange(arrangeGrob(s1, s2),
               arrangeGrob(s3, s4),
               ncol = 2)
)

g <- arrangeGrob(s1, s2, s3, s4, nrow = 2)

filename <- paste(plots_folder_scenario, "filled_contour_grid_30000.png", sep = "/")
ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")





# filled-contour plot simulations results --------------------------------------------
##TODO:
# sim_data_30000 <-
#   design_matrix %>%
#   group_by(scenario, Start, Complete, Agree, Effective, policy, wtp) %>%
#   dplyr::summarise(ENMB = mean(NMB)) %>%
#   tidyr::spread(policy, ENMB) %>%
#   mutate(INMB = screened - statusquo,
#          CE = INMB > 0)
#
# plot_data_30000 <- dplyr::filter(sim_data_30000,
#                                  Start == 100,
#                                  Complete == 100)
#
# lattice::levelplot(INMB~Agree*Effective, pred_data_30000,
#                    xlab = "Agree (%)", ylab = "Effective (%)",
#                    at = seq(-50, 50, 5),
#                    main = "Start = 100 & Complete = 100",
#                    col.regions = topo.colors(100))


