
#' plots_and_tables_scenarios
#'
#' @param cohort
#' @param dectree_res
#' @param folders
#' @param total
#'
#' @return
#' @export
#'
#' @examples
#'
plots_and_tables_scenarios <- function(cohort,
                                       dectree_res,
                                       total,
                                       aTB_CE_stats,
                                       folders) {

  screen_bcea <- screen_bcea(total)

  source("scripts/05b-output-plots_cost-effectiveness.R", local = TRUE)
  # ce_plane_with_annotations()

  source("scripts/05j-strat_pop_year.R", local = TRUE)

  table_costeffectiveness(screen_bcea, wtp_threshold, folders$output$scenario)

  num_subset_dectree(cohort, dectree_res, folders$output$scenario)
  prob_subset_dectree(cohort, dectree_res, folders$output$scenario)

  num_subset_tb(cohort, dectree_res, folders$output$scenario)

  cbind_all_subsets(folders$output$scenario)

  ceac_plot_and_save(folders, screen_bcea)

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

  ##TODO: where should this ideally go?
  nmb_matrix(dectree_res, cohort, interv, aTB_CE_stats)

  return()
}
