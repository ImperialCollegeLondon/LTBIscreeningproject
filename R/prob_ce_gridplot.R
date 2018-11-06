
#' prob_ce_gridplot
#'
#' @param out_sim
#' @param formula
#' @param folders
#'
#' @return
#' @export
#'
prob_ce_gridplot <- function(out_sim,
                             formula = as.formula(prob_CE ~ Start_Treatment_p * Complete_Treatment_p),
                             folders = NA) {

  col_l <- colorRampPalette(c('blue', 'green', 'purple', 'yellow', 'red'))(30)

  filename <- pastef(folders$plots$scenario, "prob_ce_gridplot.png")

  png(filename)

  print(
    lattice::levelplot(formula,
                       # col.regions = col_l,
                       out_sim,
                       scales = list(x = list(cex = 2), y = list(cex = 2)),
                       xlab = list(cex = 2, label = "Start (%)"),
                       ylab = list(cex = 2, label = "Complete (%)"),
                       at = seq(0, 1, 0.05),
                       panel = function(...){
                         panel.levelplot(...)
                         grid.text(seq_len(nrow(grid_points)),
                                   x = (grid_points$Start_Treatment_p - 0.48)*1.9,
                                   y = (grid_points$Complete_Treatment_p - 0.48)*1.9,
                                   gp = gpar(cex = 2))
                         grid.points(0.935, 0.725,
                                     pch = 19,
                                     gp = gpar(cex = 2)) # baseline
                       }
                       )
  )

  dev.off()

  return()
}
