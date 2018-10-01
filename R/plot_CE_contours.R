
#' plot_CE_contours
#'
#' @param dat_INMB from nmb_predictions()
#' @param folders list
#'
#' @return
#' @export
#'
#' @examples
#'
plot_CE_contours <- function(dat_INMB,
                             folders) {

  fldr <- folders$plots$scenario

  for (i in names(dat_INMB)) {

    plot_data <- dat_INMB[[i]]

    folders$plots$scenario <- paste(fldr, i, sep = "/")
    dir.create(folders$plots$scenario, showWarnings = FALSE)

    nmb_contour_plot(plot_data, folders)

    #TODO: fix and tidy
    # ce_boundary_plot(plot_data, folders)
    # base_filled_contour_grid(plot_data, folders)

    inmb_levelplot(plot_data, folders = folders)
  }
}
