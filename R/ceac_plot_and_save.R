
#' ceac_plot_and_save
#'
#' @param folders
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
ceac_plot_and_save <- function(folders, ...) {

  ##TODO: save separate files for subsets (possibly single) lines

  # BCEA::ceac.plot(screen.bcea, graph = "ggplot2") +
  #   theme(legend.position = c(0.2, 0.4)) +
  #   geom_abline(slope = 0, intercept = 0.5) +
  #   scale_color_discrete(labels = SCENARIO_LABELS) +
  #   xlim(10000,30000) +
  #   geom_vline(xintercept = 20000)


  if (!missing(folders)) {

    # filename <- paste(folders$plots$scenario, "ceac.png", sep = "/")
    filename <- paste(folders$plots$scenario, "ceac.png", sep = "/")

    png(filename, width = 400, height = 350, res = 45)

    on.exit(dev.off())
  }

  # for (i in seq_len(screen.bcea$n.comparators)) {
  try(
    print(
      BCEA::ceac.plot(...)))
  # my_ceac.plot(screen.bcea)))#, new_window = TRUE)))
  # }

  ##TODO:
  # ggplot2::ggsave(file = filename,
  #        width = 30, height = 20, units = "cm")

  return()
}

