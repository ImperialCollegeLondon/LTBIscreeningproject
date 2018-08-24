
#' plot_CE_contours
#'
#' @param pred_INMB
#'
#' @return
#' @export
#'
#' @examples
#'
plot_CE_contours <- function(plot_data) {

  nmb_contour_plot(plot_data, folders)

  # ce_boundary_plot(plot_data, folders)

  base_filled_contour_plot(plot_data, folders)
}
