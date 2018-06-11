
#' Run model
#'
#' @param policies Index number
#' @param sink_out output to file? Default: FALSE
#'
#' @return none
#' @export
#' @import crayon
#'
#' @examples
#'
run_model <- function(policies,
                      sink_out = FALSE) {

  home_dir <- find.package("LTBIscreeningproject")
  sources_correctly <- NULL
  runtime <- proc.time()

  if (sink_out) {
    msges <- file("output/messages.Rout", open = "wt")
    sink(msges, type = "message")
    on.exit(sink(type = "message"))
  }

  policy <<- NA

  for (pp in policies) {

    policy <<- pp

    try_out <- try(
      policy_run()
    )

    if (inherits(try_out, "try-error")) {
      setwd(home_dir)
      sink(type = "message")
    }

    sources_correctly <- c(sources_correctly,
                           !inherits(try_out, "try-error"))
  }

  ###################
  # table and plots #
  ###################

  try(source("scripts/combine_costeffectiveness_tables.R"))

  combine_freq_tables(parent_folder, file_name = "all_subsets.csv")
  combine_freq_tables(parent_folder, file_name = "prob_subset_dectree.csv")

  try(source("scripts/e_and_c_totals_by_scenario.R"))

  try(source("scripts/wide_combined_ce_tables.R"))

  try(source("scripts/CE_plane_by_scenario.R"))

  plot_care_cascade(parent_folder, prob_or_num = "num")
  plot_care_cascade(parent_folder, prob_or_num = "prob")

  elapsed <- proc.time() - runtime

  message(" run time: ", green(elapsed['elapsed']/60), " mins")
  message(" scenarios source correctly: ", green(sources_correctly))
}
