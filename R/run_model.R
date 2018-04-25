
#' run_model
#'
#' @param policies
#' @param sink output to file?
#'
#' @return none
#' @export
#' @import crayon
#'
#' @examples
#'
run_model <- function(policies,
                      sink = TRUE) {

  data("intervention_constants")
  data("cost_effectiveness_params")
  data("scenario_parameters")
  data("model_input_cohort")
  data("policies"); data("policies_ls")

  home_dir <- find.package("LTBIscreeningproject")
  sources_correctly <- NULL
  runtime <- proc.time()

  if (sink) {
    msges <- file("output/messages.Rout", open = "wt")
    sink(msges, type = "message")
    on.exit(sink(type = "message"))
  }

  for (policy in policies) {

    try_out <- try(
      source("scripts/00-main.R")
    )

    if (inherits(try_out, "try-error")) {
      setwd(home_dir)
      sink(type = "message")
    }

    sources_correctly <- c(sources_correctly,
                           !inherits(try_out, "try-error"))
  }

  try(source("scripts/combine-costeffectiveness-tables.R"))
  try(source("scripts/e_and_c_totals_by_scenario.R"))
  try(source("scripts/wide_combined_ce_tables.R"))
  try(source("scripts/CE_plane_by_scenario.R"))

  elapsed <- proc.time() - runtime

  message(" run time: ", green(elapsed['elapsed']/60))
  message(" scenarios source correctly: ", green(sources_correctly))
}
