
#' run_policy
#'
#' A single policy simulation
#'
#' @param make_plots
#' @param cohort_data
#'
#' @return
#' @export
#'
#' @examples
#'
run_policy <- function(cohort = NA,
                       make_plots = TRUE) {

  data("policies"); data("policies_ls")
  data("intervention_constants")
  data("cost_effectiveness_params")
  data("unit_costs")
  data("scenario_params")
  data("model_input_cohort")

  on.exit(rm(cohort))

  # set-up ---------------------------------------------------------------

  folders <- setup_folders(policy_name = policies_ls[policy], interv)

  ## use (single-migrant) user-defined cohort instead
  if (!all(is.na(cohort))) IMPUTED_sample <- cohort

  source("scripts/data-prep_constants-policy.R", local = TRUE)
  source("scripts/prep-decisiontree.R", local = TRUE)


  # modelling ------------------------------------------------------------

  dectree_res <- parallel_decision_tree(scenario_params, interv, folders)
  popmod_res <- activetb_qaly_cost(dectree_res, interv, cohort, folders)

  ce_res <- combine_popmod_dectree_res(cohort, interv, popmod_res, dectree_res, folders)

  if (make_plots) plots_and_tables_scenarios(cohort, dectree_res, popmod_res, ce_res, folders)

  return()
}
