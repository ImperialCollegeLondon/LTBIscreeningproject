
#' tornado_plot_INMB
#'
#' @param bcea
#' @param ...
#'
#' @export
#'
#' @examples
#'
tornado_plot_INMB <- function(bcea, ...) {

  UseMethod("tornado_plot_INMB", bcea)
}


#' @rdname tornado_plot_INMB
#'
tornado_plot_INMB.bcea <- function(bcea,
                                   folders) {

  design_mat <-
    pastef(folders$output$parent,
           "scenario_params_df.csv") %>%
    read.csv() %>%
    design_matrix()

  design_INMB <-
    data.frame(design_mat,
               INMB = c(
                 bcea$eib[bcea_incr$k == 10000, ],
                 bcea$eib[bcea_incr$k == 20000, ],
                 bcea$eib[bcea_incr$k == 30000, ]),
               wtp = rep(c(10000, 20000, 30000),
                         each = nrow(design_mat))) %>%
    dplyr::select(-scenario)

  tornado_data <-
    model.frame(formula = INMB ~ .,
                data = design_INMB) %>%
    s_analysis_to_tornado_plot_data()

  ## save plot ---

  png(pastef(folders$plots$scenario, "tornado_INMB.png"),
      width = 400, height = 350, res = 45)

  print(
    ggplot_tornado(dat = tornado_data,
                   ORDER = FALSE) +
      ylab("INMB") +
      # ylim(0, 150) +
      coord_cartesian(ylim = c(0, 150)) +
      coord_flip() +
      theme(legend.position = "none")
  )

  dev.off()
}
