
#' nmb_contour_plot
#'
#' @param plot_data
#' @param plots_folder
#'
#' @return
#' @export
#'
#' @examples
nmb_contour_plot <- function(plot_data,
                             folders) {

  p <-
    ggplot(plot_data, aes(x = Agree_to_Screen_p,
                          y = Effective_p,
                          z = INMB)) +
    facet_wrap(Start_Treatment_p ~ Complete_Treatment_p,
               labeller = label_both) +
    scale_fill_gradient(limits = range(plot_data$INMB),
                        high = 'white',
                        low = 'red') +
    geom_contour(aes(colour = ..level..), size = 1.2) #+

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

  filename <- paste(folders$plots$scenario, "NMB_contours_grid.png", sep = "/")
  ggsave(file = filename, width = 30, height = 20, units = "cm")

  p
}


#' ce_boundary_plot
#'
#' @param plot_data
#'
#' @return
#' @export
#'
#' @examples
ce_boundary_plot <- function(plot_data,
                             folders) {

  p <-
    ggplot(plot_data, aes(x = Agree_to_Screen_p,
                          y = Effective_p)) +
    facet_wrap(Start_Treatment_p ~ Complete_Treatment_p,
               labeller = label_both) +
    geom_point(aes(colour = factor(CE)), size = 2, shape = 15) +
    # geom_polygon(aes(fill = CE)) + ##TODO:
    theme(legend.position = "none") +
    scale_colour_grey(start = 0.7, end = 0.3)

  filename <- paste(folders$plots$scenario, "CE_boundary_grid.png", sep = "/")
  ggsave(file = filename, width = 30, height = 20, units = "cm")

  p
}


#' base_filled_contour_plot
#'
#' @param plot_data
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
base_filled_contour_plot <- function(plot_data,
                                     folders) {

  levels_range <- seq(0, 100, 5)

  COL_REG <- rainbow(n = 100, start = 3/6, end = 1/6)

  s1 <-
    lattice::levelplot(INMB ~ Agree_to_Screen_p*Effective_p,
                       subset(plot_data, Start_Treatment_p == 0.5 & Complete_Treatment_p == 0.5),
                       # plot_data,
                       xlab = "Agree (%)", ylab = "Effective (%)",
                       at = levels_range,
                       main = "Start = 50 & Complete = 50",
                       col.regions = COL_REG)#topo.colors(100))
  s2 <-
    lattice::levelplot(INMB ~ Agree_to_Screen_p*Effective_p,
                       subset(plot_data, Start_Treatment_p == 0.5 & Complete_Treatment_p == 1),
                       xlab = "Agree (%)", ylab = "Effective (%)",
                       at = levels_range,
                       main = "Start = 50 & Complete = 100",
                       col.regions = COL_REG)#topo.colors(100))
  s3 <-
    lattice::levelplot(INMB ~ Agree_to_Screen_p*Effective_p,
                       subset(plot_data, Start_Treatment_p == 1 & Complete_Treatment_p == 0.5),
                       xlab = "Agree (%)", ylab = "Effective (%)",
                       at = levels_range,
                       main = "Start = 100 & Complete = 50",
                       col.regions = COL_REG)#topo.colors(100))
  s4 <-
    lattice::levelplot(INMB ~ Agree_to_Screen_p*Effective_p,
                       subset(plot_data, Start_Treatment_p == 1 & Complete_Treatment_p == 1),
                       xlab = "Agree (%)", ylab = "Effective (%)",
                       at = levels_range,
                       main = "Start = 100 & Complete = 100",
                       col.regions = COL_REG)#topo.colors(100))
  print(
    grid.arrange(arrangeGrob(s1, s2),
                 arrangeGrob(s3, s4),
                 ncol = 2)
  )

  p <- arrangeGrob(s1, s2, s3, s4, nrow = 2)

  filename <- paste(folders$plots$scenario, "filled_contour_grid.png", sep = "/")
  ggsave(file = filename, plot = p, width = 30, height = 20, units = "cm")

  p
}
