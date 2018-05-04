
#' policy_run
#'
#' a single policy simulation
#'
#' @return empty
#' @export
#'
#' @examples
#'
policy_run <- function() {

  data("intervention_constants")
  data("cost_effectiveness_params")
  data("scenario_parameters")
  data("model_input_cohort")
  data("policies"); data("policies_ls")


  # set-up ---------------------------------------------------------------

  source("scripts/01aa-data-prep_constants-GLOBAL.R")#, echo = TRUE)
  source("scripts/setup_folders.R")#, echo = TRUE)
  source("scripts/01cc-data-prep_modelling-GLOBAL.R")#, echo = TRUE)

  source("scripts/prep-decisiontree.R")#, echo = TRUE)


  # modelling ------------------------------------------------------------

  if (!interv$cluster) source("scripts/02-parallel-decision-tree.R")#, echo = TRUE)
  if (interv$cluster)  source("Q:/R/cluster--LTBI-decision-tree/cluster-master.R")

  source("scripts/04c-cost-effectiveness_QALY-costs.R")#, echo = TRUE)
  source("scripts/04-combine_dectree_and_cmprk_model_output.R")#, echo = TRUE)


  # plots/tables ---------------------------------------------------------

  source("scripts/05b-output-plots_cost-effectiveness.R")
  source("scripts/05j-strat_pop_year_plots.R")

  table_costeffectiveness(screen.bcea, wtp_threshold, diroutput)
  num_subset_dectree(cohort, dectree_res, diroutput)
  num_subset_tb(cohort, dectree_res, diroutput)
  cbind_all_subsets(diroutput)

  # source("scripts/05-ceac-plot.R")
  # source("scripts/05-net-benefit.R")
  # source("scripts/05-netbenefit-threshold-analysis.R")
  # source("scripts/05-ternary plots.R")
  # source("scripts/05f-tornado_plots.R")
  # source("scripts/05-bayesglm_predictions.R")
  # source("scripts/05-stan_predictions.R")
  # source("scripts/05-partial_correlation_coefficients.R")
  # source("scripts/05-CE_plane_trajectories.R")
  # source("scripts/05-upper_triangle_contour_INMB.R")

  # source("scripts/05-QALY_cost_distns_by_scenario_plots.R")
  # source("scripts/05a-output-plots_competing-risks.R")
  # source("scripts/05e-output-plots_cost-effectiveness_active-TB-cases.R")
  # source("scripts/05g-INMB_ICER_histograms.R")
  # source("scripts/05h-CE_summary_stats.R")
  # source("scripts/05k-other-C-E-plots.R")
  # source("scripts/05l-output-plots_twoway-sensitivity-analysis.R")
  # source("scripts/05-pop_count_histograms.R")
  # source("scripts/05-plot-pop_QALYloss_over_time.R")
}
