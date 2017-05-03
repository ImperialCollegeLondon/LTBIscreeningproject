#
# project: LTBI screening
# N Green
#
# Fri Jan 13 16:53:18 2017
#
# tornado plots


positive_branch_only <- gsub(pattern = "Not ",
                             replacement = "",
                             x = names(scenario_parameter_p)) %>% unique()

params <- cbind(scenario_parameter_p[positive_branch_only],
                'LTBI test cost' = scenario_parameter_cost$min,
                INMB = calc.INMB(e = e.total, c = c.total, wtp = wtp_threshold),
                ICER = calc.ICER(e = e.total, c = c.total))

s_analysis_ICER <- model.frame(formula = ICER ~ .,
                               data = select(params ,-scenario, -INMB))

s_analysis_INMB <- model.frame(formula = INMB ~ .,
                               data = select(params ,-scenario, -ICER))


##TODO: check ICER against BCEA object


tornado_plot_data_ICER <- s_analysis_to_tornado_plot_data(s_analysis = s_analysis_ICER)
tornado_plot_data_INMB <- s_analysis_to_tornado_plot_data(s_analysis = s_analysis_INMB)

ggplot_tornado(dat = tornado_plot_data_INMB,
               baseline_output = -8105832) +
            ylab("INMB")

ggplot_tornado(dat = tornado_plot_data_ICER,
               baseline_output = 1673628) +
            ylab("ICER")

