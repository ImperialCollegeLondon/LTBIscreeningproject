# ****************************************
# project: LTBI screening
# N Green
#
# Fri Jan 13 16:53:18 2017
#
# tornado plots for ICER, INMB (tb cases avoided)


##TODO: include costs...


# prep data ---------------------------------------------------------------

# get combined e.total and c.total
# source("05b-output-plots_cost_effectiveness.R")

# wtp_threshold <- 20000

if (!exists("scenario_parameter_p")) scenario_parameter_p <- readxl::read_excel("data/scenario-parameter-values_fullfactorial.xlsx",
                                                                        sheet = "p")

positive_branch_only <-
  names(scenario_parameter_p) %>%
  gsub(pattern = "Not |1 - ",
       replacement = "",
       x = .) %>%
  unique()

params <-
  scenario_parameter_p[positive_branch_only] %>%
  cbind(# 'LTBI test cost' = scenario_parameter_cost$min,
    INMB = calc.INMB(e = e.total, c = c.total, wtp = wtp_threshold),
    ICER = calc.ICER(e = e.total, c = c.total))


# tornado plots -----------------------------------------------------------

s_analysis_ICER <- model.frame(formula = ICER ~ .,
                               data = dplyr::select(params ,-scenario, -INMB))

s_analysis_INMB <- model.frame(formula = INMB ~ .,
                               data = dplyr::select(params ,-scenario, -ICER))

tornado_plot_data_ICER <- s_analysis_to_tornado_plot_data(s_analysis = s_analysis_ICER)
tornado_plot_data_INMB <- s_analysis_to_tornado_plot_data(s_analysis = s_analysis_INMB)

png(paste(plots_folder_scenario, "tornado_INMB.png", sep = "/"))

print(ggplot_tornado(dat = tornado_plot_data_INMB) +
        ylab("INMB"))

dev.off()

png(paste(plots_folder_scenario, "tornado_ICER.png", sep = "/"))

print(ggplot_tornado(dat = tornado_plot_data_ICER) +
        ylab("ICER"))

dev.off()


##TODO: tb cases avoided
# ggplot_tornado(dat = tornado_plot_data_ICER,
#                baseline_output = s_analysis_ICER$ICER[1]) +
#   ylab("ICER")
