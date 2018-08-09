
ceplane_plot_and_save <- function(bcea, ...) {
  UseMethod("ceplane_plot_and_save", bcea)
}

#' ceplane_plot_and_save
#'
#' @param bcea
#' @param folders
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
ceplane_plot_and_save.bcea <- function(bcea,
                                       folders, ...) {

  if (!missing(folders)) {

    filename <- paste(folders$plots$scenario, "CE_plane2.png", sep = "/")

    png(filename, width = 400, height = 350, res = 45)

    on.exit(dev.off())
  }

  # gg <- contour2(bcea, graph = "ggplot2", wtp = 20000)
  # suppressMessages(gg + scale_colour_manual(values = cbPalette))

  cbPalette <- colorRampPalette(c("red", "orange", "green", "blue"))(bcea$n.comparisons)

  # suppressMessages(
  try(
    print(my_contour2(bcea,
                      graph = "ggplot2",
                      wtp = 20000,
                      CONTOUR_PC = "50%", ...) +
            coord_cartesian(xlim = c(0, 0.04),
                            ylim = c(-200, 200)) +
            scale_colour_manual(values = cbPalette)),
    silent = TRUE)
  # )

  # ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")

  return()
}

