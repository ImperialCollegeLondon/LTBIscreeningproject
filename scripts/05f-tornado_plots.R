# ****************************************
# project: LTBI screening
# N Green
#
# Fri Jan 13 16:53:18 2017
#
# tornado plots for ICER, INMB (tb cases avoided)


##TODO: include costs...


# prep data ---------------------------------------------------------------

# drop columns
positive_branch_only <-
  names(design_matrix) %>%
  gsub(pattern = "Not |1 - ",
       replacement = "") %>%
  unique()

tornado_params <-
  design_matrix[positive_branch_only] %>%
  cbind(
    # tb_cases_avoided = ,
    "10000" = calc.INMB(e = e.total, c = c.total, wtp = 10000),
    "20000" = calc.INMB(e = e.total, c = c.total, wtp = 20000),
    "30000" = calc.INMB(e = e.total, c = c.total, wtp = 30000),
    ICER = calc.ICER(e = e.total, c = c.total)) %>%
  reshape::melt(measure.vars = c("10000", "20000", "30000"),
       variable_name = 'wtp') %>%
  dplyr::rename(INMB = value)

# harmonise names
tornado_pred <- tornado_params
  # dplyr::rename(tornado_params,
  #               'Start' = 'Start Treatment',
  #               'Complete' = 'Complete Treatment',
  #               'Agree' = 'Agree to Screen')

# convert to percent
# tornado_pred[ ,names(tornado_pred) %in% c("Agree", "Start", "Complete", "Effective")] <-
#   tornado_pred[ ,names(tornado_pred) %in% c("Agree", "Start", "Complete", "Effective")] * 100

tornado_pred <- merge(x = tornado_pred,
                      y = rbind(pred_INMB_10000,
                                pred_INMB_20000,
                                pred_INMB_30000),
                      by = c('Start', 'Complete', 'Agree', 'Effective', 'wtp'),
                      suffixes = c('_sim', '_pred'),
                      all.y = FALSE)

tornado_pred$wtp <- as.numeric(as.character(tornado_pred$wtp))


# tornado plots with regression mean predictions --------------------------

s_analysis_INMB <- model.frame(formula = INMB_pred ~ .,
                               data = dplyr::select(tornado_pred ,-scenario, -ICER, -Sensitivity, -Specificity, -INMB_sim, -screened, -statusquo, -CE))

tornado_plot_data_INMB <- s_analysis_to_tornado_plot_data(s_analysis_INMB)


png(paste(plots_folder_scenario, "tornado_INMB_pred.png", sep = "/"),
    width = 400, height = 350, res = 45)

print(ggplot_tornado(dat = tornado_plot_data_INMB,
                     ORDER = FALSE) +
        ylab("INMB") +
        # ylim(0, 150) +
        coord_cartesian(ylim = c(0, 150)) +
        coord_flip() +
        theme(legend.position = "none")
      )

dev.off()



# tb cases avoided --------------------------------------------------------

##TODO:
# ggplot_tornado(dat = tornado_plot_data_ICER,
#                baseline_output = s_analysis_ICER$ICER[1]) +
#   ylab("ICER")


# simulation values -------------------------------------------------------

# tornado_params_20000 <-
#   scenario_parameter_p[positive_branch_only] %>%
#   cbind(
#     # tb_cases_avoided = ,
#     INMB = calc.INMB(e = e.total, c = c.total, wtp = 20000),
#     ICER = calc.ICER(e = e.total, c = c.total))
# harmonise
# tornado_pred_20000 <-
#   dplyr::rename(tornado_params_20000,
#                 'Start' = 'Start Treatment',
#                 'Complete' = 'Complete Treatment',
#                 'Agree' = 'Agree to Screen')
#
# tornado_pred_20000[ ,c("Agree", "Start", "Complete", "Effective")] <-
#   tornado_pred_20000[ ,c("Agree", "Start", "Complete", "Effective")] * 100
#
# tornado_pred_20000 <- merge(x = tornado_pred_20000,
#                             y = pred_INMB_20000,
#                             by = c('Start', 'Complete', 'Agree', 'Effective'),
#                             suffixes = c('_sim', '_pred'),
#                             all.y = FALSE)
#
# s_analysis_ICER <- model.frame(formula = ICER ~ .,
#                                data = dplyr::select(tornado_params_20000 ,-scenario, -INMB, -Sensitivity, -Specificity))
#
# s_analysis_INMB <- model.frame(formula = INMB_sim ~ .,
#                                data = dplyr::select(tornado_pred_20000 ,-scenario, -ICER, -Sensitivity, -Specificity, -INMB_pred, -screened, -statusquo, -CE))
#
# tornado_plot_data_ICER <- s_analysis_to_tornado_plot_data(s_analysis = s_analysis_ICER)
# tornado_plot_data_INMB <- s_analysis_to_tornado_plot_data(s_analysis = s_analysis_INMB)
#
# png(paste(plots_folder_scenario, "tornado_INMB_20000.png", sep = "/"),
#     width = 400, height = 350, res = 45)
#
# print(ggplot_tornado(dat = tornado_plot_data_INMB,
#                      ORDER = FALSE) +
#         ylab("INMB") + ylim(0, 150))
#
# dev.off()
#
# png(paste(plots_folder_scenario, "tornado_ICER.png", sep = "/"),
#     width = 400, height = 350, res = 45)
#
# print(ggplot_tornado(dat = tornado_plot_data_ICER,
#                      ORDER = FALSE) +
#         ylab("ICER"))
#
# dev.off()
