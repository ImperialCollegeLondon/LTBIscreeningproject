
#' tornado_plot_ICER
#'
#' @param bcea
#' @param ...
#'
#' @export
#'
#' @examples
#'
tornado_plot_ICER <- function(bcea, ...) {

  UseMethod("tornado_plot_ICER", bcea)
}


#' @rdname tornado_plot_ICER
#'
tornado_plot_ICER.bcea <- function(bcea,
                                   folders) {

  design_ICER <-
    pastef(folders$output$parent,
           "scenario_params_df.csv") %>%
    read.csv() %>%
    design_matrix() %>%
    cbind(ICER = bcea_incr$ICER) %>%
    dplyr::select(-scenario)

  tornado_data <-
    model.frame(formula = ICER ~ .,
                data = design_ICER,
                na.action = 'na.pass') %>%
    s_analysis_to_tornado_plot_data()

  ## save plot ---

  print(
    out <-
      ggplot_tornado(dat = tornado_data,
                   ORDER = FALSE) +
      ylab("ICER") +
      # ylim(0, 150) +
      coord_cartesian(ylim = c(0, 150)) +
      coord_flip() +
      theme(legend.position = "none")
  )

  ggplot2::ggsave(file = pastef(folders$plots$scenario, "tornado_ICER.png"),
                  plot = out,
                  width = 30, height = 20, units = "cm")
}
