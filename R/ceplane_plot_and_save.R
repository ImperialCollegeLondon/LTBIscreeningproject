
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
                    CONTOUR_PC = "50%") +
                    # CONTOUR_PC = "50%", ...) +
        coord_cartesian(xlim = c(0, 0.01),
                        ylim = c(-100, 200)) +
        scale_colour_manual(values = cbPalette) +
        theme(text = element_text(size = 20)) +
        ggtitle("")
    ),
    silent = TRUE)
  # )

  design_mat <-
    pastef(folders$output$parent,
           "scenario_params_df.csv") %>%
    read.csv() %>%
    design_matrix()

  dat <- data.frame(
    design_mat,
    mean_e = colMeans(bcea$delta.e),
    mean_c = colMeans(bcea$delta.c)
  )
  dat <- rbind(c(0,0,0,0), dat)
  dat$mean_e_shift <- c(dat$mean_e[-1], NA)
  dat$mean_c_shift <- c(dat$mean_c[-1], NA)

  # trajectory
  out <- out +
    geom_segment(data = dat,
                 mapping = aes(x = mean_e, y = mean_c, xend = mean_e_shift, yend = mean_c_shift),
                 inherit.aes = FALSE,
                 arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
    xlab("QALYs gained/person") + ylab("Cost incurred/person (£)") +
    theme(legend.position = "none")

  ggplot2::ggsave(file = filename, plot = out,
                  width = 20, height = 20, units = "cm")

  return()
}

