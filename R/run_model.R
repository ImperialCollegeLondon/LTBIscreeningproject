
#' run_model
#'
#' @param policies
#' @param sink_out output to file?
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

  try(source("scripts/combine-costeffectiveness-tables.R"))
  try(source("scripts/combine-num_screen-tables.R"))
  try(source("scripts/e_and_c_totals_by_scenario.R"))
  try(source("scripts/wide_combined_ce_tables.R"))
  try(source("scripts/CE_plane_by_scenario.R"))

  elapsed <- proc.time() - runtime

  message(" run time: ", green(elapsed['elapsed']/60))
  message(" scenarios source correctly: ", green(sources_correctly))
}