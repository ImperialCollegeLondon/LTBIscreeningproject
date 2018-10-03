
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

  col.l <- colorRampPalette(c('blue', 'green', 'purple', 'yellow', 'red'))(30)

  filename <- pastef(folders$plots$scenario, "prob_ce_gridplot.png")

  png(filename)

  print(
    lattice::levelplot(formula,
                       # col.regions = col.l,
                       out_sim,
                       scales = list(x = list(cex = 2), y = list(cex = 2)),
                       xlab = "Start (%)", ylab = "Complete (%)",
                       at = seq(0, 1, 0.05))
  )

  dev.off()

  return()
}
