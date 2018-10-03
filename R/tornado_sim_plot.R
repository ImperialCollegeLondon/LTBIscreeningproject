
#' tornado_sim_plot
#'
#' @param ce_res
#' @param folders List
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
tornado_sim_plot <- function(ce_res,
                             folders) {

  scenario_params_df <-
    pastef(folders$output$parent,
           "scenario_params_df.csv") %>%
    read.csv()

  n_scenarios <- max(scenario_params_df$scenario)

  total <- list(e = ce_res$ce_incr$e,
                c = ce_res$ce_incr$c)

  tornado_dat <-
    scenario_params_df %>%
    design_matrix() %>%
    cbind(
      # tb_cases_avoided = ##TODO,
      INMB = c(
        calc.INMB(e = total$e, c = total$c, wtp = 10000),
        calc.INMB(e = total$e, c = total$c, wtp = 20000),
        calc.INMB(e = total$e, c = total$c, wtp = 30000)),
      wtp = rep(c(10000, 20000, 30000),
                each = n_scenarios),
      ICER = calc.ICER(e = total$e, c = total$c))

  var_names <-
    names(tornado_dat)[names(tornado_dat) %in% c("wtp",
                                                 "Agree_to_Screen_cost",
                                                 "Agree_to_Screen_p",
                                                 "Sensitivity_p",
                                                 "Specificity_p",
                                                 "Complete_Treatment_p",
                                                 "Effective_p",
                                                 "Start_Treatment_p")]
  rhs <- paste(var_names, collapse = " + ")

  ss <-
    model.frame(formula = as.formula(paste0("INMB ~ ", rhs)),
                data = tornado_dat %>% dplyr::select(-scenario),
                na.action = 'na.pass')
  p1 <-
    s_analysis_to_tornado_plot_data(ss) %>%
    ggplot_tornado() +
    ylab("INMB") +
    # ylim(-170,100) +
    theme(legend.position = "none") +
    ggtitle("(a) INMB")

  ss <-
    model.frame(formula = as.formula(paste0("ICER ~ ", rhs)),
                data = tornado_dat %>% dplyr::select(-scenario),
                na.action = 'na.pass')
  p2 <-
    s_analysis_to_tornado_plot_data(ss) %>%
    ggplot_tornado() +
    ylab("ICER") +
    # ylim(-100,350) +
    theme(legend.position = "none") +
    ggtitle("(b) ICER")

  out <- gridExtra::grid.arrange(p1, p2, nrow = 2)
  ggplot2::ggsave(file = pastef(folders$plots$scenario, "tornado.png"),
                  plot = out,
                  width = 30, height = 20, units = "cm")
}
