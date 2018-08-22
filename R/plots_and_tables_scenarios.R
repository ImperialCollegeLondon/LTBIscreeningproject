
#' plots_and_tables_scenarios
#'
#' Multiple types of figures.
#'
#' @param cohort
#' @param dectree_res
#' @param ce1
#' @param aTB_CE_stats
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
plots_and_tables_scenarios <- function(cohort,
                                       dectree_res,
                                       ce1,
                                       aTB_CE_stats,
                                       folders) {

  screen_bcea <- screen_bcea(ce1)

  ceplane_plot_and_save(screen_bcea, folders)
  # ce_plane_with_annotations()

  source("scripts/05j-strat_pop_year.R", local = TRUE)

  table_costeffectiveness(screen_bcea, wtp_threshold, folders$output$scenario)

  num_subset_dectree(cohort, dectree_res, folders$output$scenario)
  prob_subset_dectree(cohort, dectree_res, folders$output$scenario)

  num_subset_tb(cohort, dectree_res, folders$output$scenario)

  cbind_all_subsets(folders$output$scenario)

  ceac_plot_and_save(screen_bcea, folders)

  pred_INMB <- nmb_predictions(aTB_CE_stats, folders)

  ##TODO:...
  # plot_CE_contours(pred_INMB)


  ##TODO:...
  # source("scripts/05-bayesglm_predictions.R")
  # source("scripts/05-stan_predictions.R")

  # source("scripts/05-ternary plots.R")
  # source("scripts/05f-tornado_plots.R")
  # source("scripts/05-partial_correlation_coefficients.R")
  # source("scripts/05-upper_triangle_contour_INMB.R")
  # CE_plane_trajectories(design_matrix)

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

  return()
}
