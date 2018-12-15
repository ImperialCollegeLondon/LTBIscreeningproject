
#' prob_ce_gridplot
#'
#' @param out_sim
#' @param formula Defalut: \code{prob_CE ~ Start_Treatment_p * Complete_Treatment_p}
#' @param folders list
#' @param plot_type \code{base} or \code{ggplot2}
#'
#' @return
#' @export
#'
prob_ce_gridplot <- function(out_sim,
                             formula = as.formula(prob_CE ~ Start_Treatment_p * Complete_Treatment_p),
                             folders = NA,
                             plot_type = "base") {

  col_l <- colorRampPalette(c('blue', 'green', 'purple', 'yellow', 'red'))(30)

  filename <- pastef(folders$plots$scenario, "prob_ce_gridplot.png")

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

  if (plot_type == "base") {

    png(filename)

    print(
      prob_ce_gridplot <-
        lattice::levelplot(formula,
                           # col.regions = col_l,
                           out_sim,
                           scales = list(x = list(cex = 2), y = list(cex = 2)),
                           xlab = list(cex = 2, label = "Start (%)"),
                           ylab = list(cex = 2, label = "Complete (%)"),
                           at = seq(0, 1, 0.05),
                           panel = function(...){
                             panel.levelplot(...)
                             grid.text(grid_points$lab_num,
                                       x = (grid_points$Start_Treatment_p - 0.48)*1.9,
                                       y = (grid_points$Complete_Treatment_p - 0.48)*1.9,
                                       gp = gpar(cex = 2))
                             grid.points(0.935, 0.725,
                                         pch = 19,
                                         gp = gpar(cex = 2)) # baseline
                           })
    )
    dev.off()
  }

  if (plot_type == "ggplot2") {

    pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

    prob_ce_gridplot <-
      ggplot(out_sim, aes(Start_Treatment_p, Complete_Treatment_p, z = prob_CE)) +
      geom_tile(aes(fill = prob_CE)) +
      scale_fill_gradientn(colours = pal) +
      coord_equal() +
      theme_bw() +
      xlab("Start (%)") +
      ylab("Complete (%)") +
      geom_text(data = grid_points,
                aes(x = Start_Treatment_p,
                    y = Complete_Treatment_p,
                    label = lab_num), size = 5, inherit.aes = FALSE) +
      geom_point(aes(x = 0.935, y = 0.725), size = 5)#,

    print(prob_ce_gridplot)

    ggsave(filename = pastef(folders$plots$scenario, "prob_ce_gridplot_ggplot.png"),
           plot = prob_ce_gridplot,
           width = 20, height = 20, units = "cm")
  }

  save(prob_ce_gridplot, file = pastef(folders$plots$scenario, "prob_ce_gridplot.RData"))

  return()
}
