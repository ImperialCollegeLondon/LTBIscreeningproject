
#' ceplane_plot_and_save
#'
#' @param folders
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
ceplane_plot_and_save <- function(folders, ...) {

  if (!missing(folders)) {

    filename <- paste(folders$plots$scenario, "CE_plane2.png", sep = "/")

    png(filename, width = 400, height = 350, res = 45)

    on.exit(dev.off())
  }

  # gg <- contour2(screen_bcea, graph = "ggplot2", wtp = 20000)
  # suppressMessages(gg + scale_colour_manual(values = cbPalette))

  # cbPalette <- colorRampPalette(c("red", "orange", "green", "blue"))(screen_bcea$n.comparisons)

  suppressMessages(
    try(
      print(my_contour2(...,
                        graph = "ggplot2",
                        wtp = 20000,
                        CONTOUR_PC = "50%") +
              coord_cartesian(xlim = c(0, 0.04),
                              ylim = c(-200, 200)) +
              scale_colour_manual(values = cbPalette)),
      silent = TRUE))

  # ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")

  return()
}

