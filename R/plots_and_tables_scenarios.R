
#' plots_and_tables_scenarios
#'
#' Multiple types of figures and tables.
#'
#' @param cohort
#' @param dectree_res
#' @param popmod_res
#' @param ce_res
#' @param folders list of input/output folders
#'
#' @return Side effects only
#' @export
#'
#' @examples
#'
plots_and_tables_scenarios <- function(cohort,
                                       dectree_res,
                                       popmod_res,
                                       ce_res,
                                       folders) {

  bcea_incr <- bcea_incremental(ce_res$ce_incr)

  bcea_default <- bcea(e = -ce_res$ce_default$e,
                       c = -ce_res$ce_default$c,
                       ref = 1)

  ceplane_plot_and_save(bcea_incr, folders)
  # ce_plane_with_annotations()

  source("scripts/05j-strat_pop_year.R", local = TRUE)

  table_costeffectiveness(bcea_default, wtp_threshold, folders$output$scenario)

  num_subset_dectree(cohort, dectree_res, folders$output$scenario)
  prob_subset_dectree(cohort, dectree_res, folders$output$scenario)

  num_subset_tb(cohort, dectree_res, folders$output$scenario)

  cbind_all_subsets(folders$output$scenario)

  ceac_plot_and_save(bcea_incr, folders)

  boxplot_INMB(bcea_incr, folders)

  pred_INMB <- nmb_predictions(ce_res, folders)
  plot_CE_contours(pred_INMB, folders)

  # out_INMB <- inmb_from_bcea(bcea_default, folders)
  # plot_CE_contours(out_INMB, folders)


  ##TODO: update this and extract to function & test
  # source("scripts/05-bayesglm_predictions.R") ##TODO:
  # source("scripts/05-stan_predictions.R") ##TODO:




  # source("scripts/05-ternary plots.R")
  # source("scripts/05f-tornado_plots.R")
  # source("scripts/05-partial_correlation_coefficients.R")
  # source("scripts/05-upper_triangle_contour_INMB.R")
  # CE_plane_trajectories(design_matrix)

  # plot_QALY_cost_distns_by_scenario(popmod_res, folders)
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
