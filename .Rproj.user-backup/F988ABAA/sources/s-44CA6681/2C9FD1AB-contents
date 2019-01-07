
#' inmb_levelplot
#'
#' @param plot_data
#' @param formula Default: \code{INMB ~ Start_Treatment_p*Complete_Treatment_p}
#' @param start
#' @param complete
#' @param levels_range
#' @param folders
#' @param plot_type \code{base} or \code{ggplot2}
#'
#' @return
#' @export
#'
inmb_levelplot <- function(plot_data,
                           formula = as.formula(INMB ~ Start_Treatment_p*Complete_Treatment_p),
                           start = NA,
                           complete = NA,
                           # levels_range = NA,
                           # levels_range = seq(-30, 120, by = 5), #effectiveness
                           levels_range = seq(-70, 50, by = 5), #unit cost
                           folders = NA,
                           plot_type = "base") {

  COL_REG <- rainbow(n = 100, start = 3/6, end = 1/6)

  if (anyNA(levels_range)) {
    max_min <- range(plot_data$INMB)
    levels_range <- seq(max_min[1] - 1, max_min[2] + 1, by = 1)
  }

  design <-
    pastef(folders$output$parent,
           "scenario_params_df.csv") %>%
    read.csv() %>%
    design_matrix()

  grid_points <-
    design %>%
    dplyr::filter(Complete_Treatment_p %in% c(0.5,0.75,0.9),
                  Start_Treatment_p %in% c(0.5,0.75,0.9)) %>%
    mutate(lab_num = row_number())

  filename <- pastef(folders$plots$scenario, "inmb_levelplot.png")

  if (plot_type == "base") {

    png(filename)

    print(
      inmb_levelplot <-
        lattice::levelplot(formula,
                           plot_data,
                           # subset(plot_data,
                           #        Start_Treatment_p == start & Complete_Treatment_p == complete),
                           xlab = list(cex = 2, label = "Start (%)"),
                           ylab = list(cex = 2, label = "Complete (%)"),
                           scales = list(x = list(cex = 2), y = list(cex = 2)),
                           panel = function(...){
                             panel.levelplot(...)
                             grid.text(grid_points$lab_num,
                                       x = (grid_points$Start_Treatment_p - 0.48)*1.9,
                                       y = (grid_points$Complete_Treatment_p - 0.48)*1.9,
                                       gp = gpar(cex = 2))
                             grid.points(0.935, 0.725,
                                         pch = 19,
                                         gp = gpar(cex = 2))}, # baseline
                           at = levels_range,
                           # main = paste("Start =", start, "& Complete =", complete),
                           col.regions = COL_REG) #topo.colors(100))
    )
    dev.off()
  }

  if (plot_type == "ggplot2") {

    plot_breakpoints <- seq(-70, 35, 5)
    plot_data$INMB_cut <- cut(plot_data$INMB, plot_breakpoints)
    plot_data$INMB_cut <- factor(plot_data$INMB_cut, levels = rev(levels(plot_data$INMB_cut)))

    # pal_wes <- wesanderson::wes_palette("Zissou1", n = 5, type = "discrete")
    pal <- colorRampPalette(c("blue", "yellow", "red"))(length(levels(plot_data$INMB_cut)))

    inmb_levelplot <-
      ggplot(plot_data, aes(Start_Treatment_p, Complete_Treatment_p, z = INMB_cut)) +
      geom_tile(aes(fill = INMB_cut)) +
      scale_fill_manual(values = setNames(pal, levels(plot_data$INMB_cut))) +
      # scale_fill_gradientn(colours = pal_wes, limits = c(-70, 35)) + #continuous colours
      coord_equal() +
      theme_bw() +
      xlab("Start (%)") +
      ylab("Complete (%)") +
      geom_text(data = grid_points,
                aes(x = Start_Treatment_p,
                    y = Complete_Treatment_p,
                    label = lab_num), size = 5, inherit.aes = FALSE) +
      geom_point(aes(x = 0.935, y = 0.725), size = 5) +
      theme(panel.border = element_blank())

    print(inmb_levelplot)

    ggsave(filename = pastef(folders$plots$scenario, "inmb_levelplot_ggplot.png"),
           plot = inmb_levelplot,
           width = 20, height = 20, units = "cm")
  }

  save(inmb_levelplot, file = pastef(folders$plots$scenario, "inmb_levelplot.RData"))

  return()
}

