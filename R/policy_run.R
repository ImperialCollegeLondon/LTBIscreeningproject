
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
  source("scripts/data-prep_constants-policy.R", local = TRUE)#, echo = TRUE)
  source("scripts/prep-decisiontree.R", local = TRUE)#, echo = TRUE)


  # modelling ------------------------------------------------------------

  dectree_res <- parallel_decision_tree(scenario_parameters, interv, folders)
  aTB_CE_stats <- activetb_qaly_cost(dectree_res, interv, cohort, folders)

  total <- combine_dectree_and_pop_outputs(cohort, interv, aTB_CE_stats, dectree_res, folders)

  plots_and_tables_scenarios(cohort, dectree_res, total, folders)

  rm(cohort)
}
