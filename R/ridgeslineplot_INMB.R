
#' ridges line plot INMB
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

    design_mat <-
      pastef(folders$output$parent,
             "scenario_params_df.csv") %>%
      read.csv() %>%
      design_matrix()

    filename <- pastef(folders$plots$scenario, "ridgeslineplot_INMB.png")
  }

  dat <-
    bcea$ib[bcea$k == wtp_threshold, , ] %>%
    melt() %>%
    merge(design_mat, by.x = "X2", by.y = "scenario") %>%
    mutate(X2 = factor(X2),
           Agree_to_Screen_cost = factor(Agree_to_Screen_cost))

  print(
    ridgeplot <-
      ggplot(dat, aes(x = value, y = Agree_to_Screen_cost, fill = X2)) +
      ggridges::geom_density_ridges(show.legend = FALSE, quantile_lines = TRUE,
                                    jittered_points = TRUE,
                                    position = ggridges::position_points_jitter(width = 0.05, height = 0),
                                    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.2) +
      theme_bw() +
      theme(text = element_text(size = 30)) +
      xlab(paste0("INB (", intToUtf8(163), ")")) +
      ylab(paste0("Density/unit test cost (", intToUtf8(163), ")")) +
      geom_vline(xintercept = 0, linetype = "dashed")
  )

  ggplot2::ggsave(file = filename,
                  plot = ridgeplot,
                  width = 30, height = 20, units = "cm")

  save(ridgeplot, file = paste(folders$plots$scenario, "ridgeplot.RData", sep = "/"))
}
