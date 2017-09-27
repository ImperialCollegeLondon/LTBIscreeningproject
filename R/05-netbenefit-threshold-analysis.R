# ************************************
# LTBI screening
# N Green
# Sept 2017
#
# net benefit threshold analysis
#


library(directlabels)
library(cowplot)


pred_grid_values <- seq(50, 100, 1)

# predict using fit model
pred_data <- expand.grid("Agree" = pred_grid_values,
                         "Start" = pred_grid_values,
                         "Complete" = pred_grid_values,
                         "Effective" = pred_grid_values,
                         "policy" = c("screened", "statusquo"))

pred_data$pred <- predict(lm_multi$`30000`, pred_data, type = "response")

pred_data <-
  tidyr::spread(pred_data, policy, pred) %>%
  mutate(INMB = screened - statusquo,
         CE = INMB > 0)

plot_grid_values <- c(50, 100)
plot_data <- dplyr::filter(pred_data,
                           Start %in% plot_grid_values,
                           Complete %in% plot_grid_values)



# NMB contours ------------------------------------------------------------

p <- ggplot2::ggplot(plot_data, aes(x = Agree, y = Effective, z = INMB)) +
  facet_wrap(Start ~ Complete, labeller = label_both) +
  scale_fill_gradient(limits = range(plot_data$INMB), high = 'white', low = 'red') +
  geom_contour(aes(colour = ..level..), size = 1.2) +
  # stat_contour(geom = "polygon", aes(fill = ..level..)) +
  # coord_cartesian(xlim = c(min(plot_data$Agree), max(plot_data$Agree)),
  #                 ylim = c(min(plot_data$Effective), max(plot_data$Effective))) +
    # scale_colour_gradient(guide = 'none') +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = "none")
  # stat_contour(breaks = 0)

direct.label(p, list("bottom.pieces", colour = 'black'))


filename <- paste(plots_folder_scenario, "NMB_contours_grid.png", sep = "/")
ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")



# cost-effectiveness boundary plot ----------------------------------------

ggplot2::ggplot(plot_data, aes(x = Agree, y = Effective)) +
  facet_wrap(Start ~ Complete, labeller = label_both) +
  geom_point(aes(colour = factor(CE)), size = 2) +
  # geom_polygon(aes(fill = CE)) + ##TODO:
  theme(legend.position = "none") +
  scale_colour_grey(start = 0.7, end = 0.3)

filename <- paste(plots_folder_scenario, "CE_boundary_grid.png", sep = "/")
ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")
