
#' nmb_contour_plot
#'
#' Single or multiple contour plot.
#'
#' @param plot_data
#' @param folders List
#' @param x_var string
#' @param y_var string
#' @param facet_vars Vector of strings
#'
#' @return
#' @export
#'
#' @examples
nmb_contour_plot <- function(plot_data,
                             folders,
                             x_var = "Start_Treatment_p",
                             y_var = "Complete_Treatment_p",
                             facet_vars = c("Agree_to_Screen_p", "Effective_p")) {

  p <-
    ggplot(plot_data, aes_string(x = x_var,
                                 y = y_var,
                                 z = "INMB")) +
    theme_bw() +
    xlab(gsub(x = x_var, "_|[_p]$", " ")) +
    ylab(gsub(x = y_var, "_|[_p]$", " ")) +
    theme(text = element_text(size = 20)) +
    xlim(min(plot_data[ ,x_var]), 1) +
    ylim(min(plot_data[ ,y_var]), 1) +
    # geom_contour(aes(colour = ..level..), size = 1.2) #+  # multiple contours
    geom_contour(mapping = aes_string(x = x_var,       # single contour at breaks
                                      y = y_var,
                                      z = "INMB"),
                 breaks = 0) #+

  # facet_wrap(Start_Treatment_p ~ Complete_Treatment_p,
  # facet_wrap(facet_vars,
  #            labeller = label_both) +
  # scale_fill_gradient(limits = range(plot_data$INMB),
  #                     high = 'white',
  #                     low = 'red') +
  # stat_contour(geom = "polygon", aes(fill = ..level..)) +
  # coord_cartesian(xlim = c(min(plot_data$Agree), max(plot_data$Agree)),
  #                 ylim = c(min(plot_data$Effective), max(plot_data$Effective))) +
  # scale_colour_gradient(guide = 'none') +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  # theme(legend.position = "none")
  # stat_contour(breaks = 0)

  ##TODO: new error??
  # print(
  #   p <- direct.label(p, list("bottom.pieces", colour = 'black'))
  # )

  filename <- pastef(folders$plots$scenario, "NMB_contours_grid.png")
  ggsave(file = filename, plot = p, width = 30, height = 20, units = "cm")

  p
}


#' ce_boundary_line_plot
#'
#' @param folders
#' @param x_var
#' @param y_var
#' @param plot_data
#'
#' @return
#' @export
#'
#' @examples
ce_boundary_line_plot <- function(plot_data,
                                  folders = NA,
                                  x_var = "Start_Treatment_p",
                                  y_var = "Complete_Treatment_p") {

  INMB_names <- grep(pattern = "INMB",
                     names(plot_data),
                     value = TRUE)
  # in case duplicate names
  INMB_names_new <- paste0(INMB_names, seq_along(INMB_names))
  names(plot_data)[names(plot_data) %in% INMB_names] <- INMB_names_new

  plot_data <- plot_data[ , c(x_var, y_var, INMB_names_new)]

  ##TODO: hard-coded
  plot_data <-
    plot_data %>%
    reshape::rename(c(INMB1 = "60%",
                      INMB2 = "65%",
                      INMB3 = "70%",
                      INMB4 = "75%",
                      INMB5 = "80%",
                      INMB6 = "85%",
                      INMB7 = "90%",
                      INMB8 = "95%",
                      INMB9 = "100%"))

  plot_data_melt <- melt(plot_data,
                         id.vars = c("Start_Treatment_p", "Complete_Treatment_p"))
  value <- "value"
  p <-
    ggplot(plot_data_melt, aes_string(x = x_var,
                                      y = y_var,
                                      z = value)) +
    theme_bw() +
    xlab(gsub(x = x_var, "_|[_p]$", " ")) +
    ylab(gsub(x = y_var, "_|[_p]$", " ")) +
    theme(text = element_text(size = 20)) +
    xlim(min(plot_data[ ,x_var]), 1) +
    ylim(min(plot_data[ ,y_var]), 1) +
    geom_contour(aes(col = variable), breaks = 0)

  p <- direct.label(p, list("last.points", colour = 'black'))

  p <- p +
    geom_point(aes(x = 0.935, y = 0.725, size = 10)) +
    theme(legend.position = "none")

  # filename <- pastef(folders$plots$scenario, "ce_boundary_line_plot.png")
  # ggsave(file = filename, plot = p, width = 30, height = 20, units = "cm")

  invisible(p)
}




#' ce_boundary_points_plot
#'
#' @param plot_data
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
ce_boundary_points_plot <- function(plot_data,
                                    folders) {

  p <-
    ggplot(plot_data, aes(x = Start_Treatment_p,
                          y = Complete_Treatment_p)) +
    # facet_wrap(Start_Treatment_p ~ Complete_Treatment_p,
    #            labeller = label_both) +
    geom_point(aes(colour = factor(CE)), size = 2, shape = 15) +
    # geom_polygon(aes(fill = CE)) + ##TODO:
    theme(legend.position = "none") +
    scale_colour_grey(start = 0.7, end = 0.3)

  filename <- pastef(folders$plots$scenario, "CE_boundary_grid.png")
  ggsave(file = filename, plot = p, width = 30, height = 20, units = "cm")

  invisible(p)
}


#' base_filled_contour_grid
#'
#' @param plot_data
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
base_filled_contour_grid <- function(plot_data,
                                     folders) {


  ##TODO: update

  filename <- paste(folders$plots$scenario,
                    "filled_contour_grid.png", sep = "/")

  s1 <- inmb_levelplot(plot_data, 0.5, 0.5)
  s2 <- inmb_levelplot(plot_data, 0.5, 1)
  s3 <- inmb_levelplot(plot_data, 1, 0.5)
  s4 <- inmb_levelplot(plot_data, 1, 1)

  png(filename)

  print(
    gridExtra::grid.arrange(arrangeGrob(s1, s2),
                            arrangeGrob(s3, s4),
                            ncol = 2)
  )

  dev.off()

  # p <- arrangeGrob(s1, s2, s3, s4, nrow = 2)
  # ggsave(file = filename, plot = p,
  #        width = 30, height = 20, units = "cm")
  #
  #   p
}

