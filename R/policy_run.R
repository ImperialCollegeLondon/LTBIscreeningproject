
#' policy_run
#'
#' A single policy simulation
#'
#' @return
#' @export
#'
#' @examples
#'
policy_run <- function() {

  data("policies"); data("policies_ls")
  data("intervention_constants")
  data("cost_effectiveness_params")
  data("scenario_params")
  data("model_input_cohort")

  on.exit(rm(cohort))

  # set-up ---------------------------------------------------------------

  folders <- setup_folders(policy_name = policies_ls[policy], interv)

  ## use single-migrant-cohort here instead?
  # cohort <- cohort_single[[4]]

  source("scripts/data-prep_constants-policy.R", local = TRUE)
  source("scripts/prep-decisiontree.R", local = TRUE)


  # modelling ------------------------------------------------------------

  dectree_res <- parallel_decision_tree(scenario_params, interv, folders)
  popmod_res <- activetb_qaly_cost(dectree_res, interv, cohort, folders)

  incr_ce <- total_incremental_ce(cohort, interv, popmod_res, dectree_res, folders)

  plots_and_tables_scenarios(cohort, dectree_res, popmod_res, incr_ce, folders, interv)

  return()
}
