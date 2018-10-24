
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
ceplane_plot_and_save <- function(bcea, ...) {
  UseMethod("ceplane_plot_and_save", bcea)
}


#' @rdname ceplane_plot_and_save
#'
ceplane_plot_and_save.bcea <- function(bcea,
                                       folders, ...) {

  if (!missing(folders)) {

    filename <- paste(folders$plots$scenario, "CE_plane2.png", sep = "/")

    # png(filename, width = 400, height = 350, res = 45)
    # on.exit(dev.off())
  }

  # gg <- contour2(bcea, graph = "ggplot2", wtp = 20000)
  # suppressMessages(gg + scale_colour_manual(values = cbPalette))

  cbPalette <- colorRampPalette(c("red", "orange", "green", "blue"))(bcea$n.comparisons)

  # suppressMessages(
  try(
    print(
      out <-
        my_contour2(bcea,
                    graph = "ggplot2",
                    wtp = 20000,
                    # CONTOUR_PC = "50%") +
                    CONTOUR_PC = "50%", ...) +
        coord_cartesian(xlim = c(0, 0.01),
                        ylim = c(-100, 200)) +
        scale_colour_manual(values = cbPalette) +
        theme(text = element_text(size = 20)) +
        ggtitle("")
    ),
    silent = TRUE)
  # )

  ggplot2::ggsave(file = filename, plot = out,
                  width = 20, height = 20, units = "cm")

  return()
}

