
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

  folders <- setup_folders(policy_name = policies_ls[policy], interv)

  ## use single-migrant-cohort here instead?
  source("scripts/data-prep_constants-policy.R")#, echo = TRUE)

  source("scripts/prep-decisiontree.R")#, echo = TRUE)


  # modelling ------------------------------------------------------------

  if (!interv$cluster) {
    dectree_res <- parallel_decision_tree(scenario_parameters, interv, folders)
  }

  if (interv$cluster) source("Q:/R/cluster--LTBI-decision-tree/cluster-master.R")

  source("scripts/04c-cost-effectiveness_QALY-costs.R")#, echo = TRUE)
  source("scripts/04-combine_dectree_and_cmprk_model_output.R")#, echo = TRUE)


  # plots/tables ---------------------------------------------------------

  source("scripts/05b-output-plots_cost-effectiveness.R")
  source("scripts/05j-strat_pop_year.R")

  table_costeffectiveness(screen.bcea, wtp_threshold, folders$output$scenario)

  num_subset_dectree(cohort, dectree_res, folders$output$scenario)
  prob_subset_dectree(cohort, dectree_res, folders$output$scenario)

  num_subset_tb(cohort, dectree_res, folders$output$scenario)

  cbind_all_subsets(folders$output$scenario)

  source("scripts/05-ceac-plot.R")


  ## this is for agree-start-complete-effective probs:
  #
  # source("scripts/05-net-benefit.R")
  # source("scripts/05-netbenefit-threshold-analysis.R")
  # source("scripts/05-ternary plots.R")
  # source("scripts/05f-tornado_plots.R")
  # source("scripts/05-bayesglm_predictions.R")
  # source("scripts/05-stan_predictions.R")
  # source("scripts/05-partial_correlation_coefficients.R")
  # CE_plane_trajectories(design_matrix)
  # source("scripts/05-upper_triangle_contour_INMB.R")



  # plot_QALY_cost_distns_by_scenario(aTB_CE_stats, folders)
  # source("scripts/05e-output-plots_cost-effectiveness_active-TB-cases.R")
  # source("scripts/05g-INMB_ICER_histograms.R")
  #
  # source("scripts/05h-CE_summary_stats.R")
  #
  # source("scripts/05k-other-C-E-plots.R")
  # source("scripts/05l-output-plots_twoway-sensitivity-analysis.R")
  # source("scripts/05-pop_count_histograms.R")
  # source("scripts/05-plot-pop_QALYloss_over_time.R")

  rm(cohort)
}
