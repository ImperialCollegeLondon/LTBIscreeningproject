
#' ---
#' title: "LTBI screening model:
#' one-ways sensitivity analysis
#' tornado plot"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html


## change these locations ##
scenario_params_df <- read.csv(here("ext-data", "current_tech", "18_to_35_in_2009", "scenario_params_df.csv"))
load(here("ext-data", "current_tech", "18_to_35_in_2009", "policy_003", "e_and_c_totals.RData"))

total <- list(e = e.total,
              c = c.total)

tornado_dat <-
  scenario_params_df %>%
  design_matrix() %>%
  cbind(
    # tb_cases_avoided = ##TODO,
    INMB10000 = calc.INMB(e = total$e, c = total$c, wtp = 10000),
    INMB20000 = calc.INMB(e = total$e, c = total$c, wtp = 20000),
    INMB30000 = calc.INMB(e = total$e, c = total$c, wtp = 30000),
    ICER = calc.ICER(e = total$e, c = total$c))


ss <- model.frame(formula = as.formula(INMB10000 ~ Agree_to_Screen_cost + Agree_to_Screen_p + Sensitivity_p + Specificity_p + Complete_Treatment_p + Effective_p + Start_Treatment_p),
                  data = tornado_dat,
                  na.action = 'na.pass')
p1 <- s_analysis_to_tornado_plot_data(ss) %>%
  ggplot_tornado(baseline_output = tail(ss[ ,1], 1)) + ylab("INMB") + ylim(-100,350) + theme(legend.position = "none") + ggtitle("(a) WTP £10,000")

ss <- model.frame(formula = as.formula(INMB20000 ~ Agree_to_Screen_cost + Agree_to_Screen_p + Sensitivity_p + Specificity_p + Complete_Treatment_p + Effective_p + Start_Treatment_p),
                  data = tornado_dat,
                  na.action = 'na.pass')
p2 <- s_analysis_to_tornado_plot_data(ss) %>%
  ggplot_tornado(baseline_output = tail(ss[ ,1], 1)) + ylab("INMB") + ylim(-100,350) + theme(legend.position = "none") + ggtitle("(b) WTP £20,000")

ss <- model.frame(formula = as.formula(INMB30000 ~ Agree_to_Screen_cost + Agree_to_Screen_p + Sensitivity_p + Specificity_p + Complete_Treatment_p + Effective_p + Start_Treatment_p),
                  data = tornado_dat,
                  na.action = 'na.pass')
p3 <- s_analysis_to_tornado_plot_data(ss) %>%
  ggplot_tornado(baseline_output = tail(ss[ ,1], 1)) + ylab("INMB") + ylim(-100,350) + theme(legend.position = "none") + ggtitle("(c) WTP £30,000")

gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
