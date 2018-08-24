
#' plot_CE_contours
#'
#' @param pred_INMB
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
plot_CE_contours <- function(pred_INMB,
                             folders) {

  fldr <- folders$plots$scenario

  for (i in names(pred_INMB)) {

    folders$plots$scenario <- paste(fldr, i, sep = "/")
    dir.create(folders$plots$scenario, showWarnings = FALSE)

    nmb_contour_plot(pred_INMB[[i]], folders)

    # ce_boundary_plot(plot_data[[i]], folders)

    base_filled_contour_grid(plot_data = pred_INMB[[i]], folders)
  }
}
