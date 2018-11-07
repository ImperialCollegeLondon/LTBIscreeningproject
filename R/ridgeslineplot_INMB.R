
#' ridgeslineplot_INMB
#'
#' @param bcea
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
ridgeslineplot_INMB <- function(bcea, ...) {

  UseMethod("ridgeslineplot_INMB", bcea)
}


#' @rdname histogram_INMB
#'
ridgeslineplot_INMB.bcea <- function(bcea,
                                folders = NA,
                                wtp_threshold = 20000) {

  ##TODO: oneway/twoway conditions

  if (!all(is.na(folders))) {

    filename <- pastef(folders$plots$scenario, "ridgeslineplot_INMB.png")
  }

  dat <-
    bcea$ib[bcea$k == wtp_threshold, , ] %>%
    melt() %>%
    mutate(X2 = factor(X2))

  print(
    out <-
      ggplot(dat, aes(x = value, y = X2, fill = X2)) +
      ggridges::geom_density_ridges(show.legend = FALSE, quantile_lines = TRUE,
                                    jittered_points = TRUE,
                                    position = position_points_jitter(width = 0.05, height = 0),
                                    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7) +
      theme_bw() +
      theme(text = element_text(size = 30)) +
      ylab(paste0("INB (", intToUtf8(163), ")")) +
      ylab("density") +
      geom_vline(xintercept = 0, linetype = "dashed")
  )

  ggplot2::ggsave(file = filename,
                  plot = out,
                  width = 30, height = 20, units = "cm")
}
