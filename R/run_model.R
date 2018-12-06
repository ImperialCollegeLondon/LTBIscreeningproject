
#' Run model
#'
#' @param cohort_data
#' @param make_plots
#' @param sink_out output to file? Default: FALSE
#'
#' @return
#' @export
#' @import crayon
#'
#' @examples
#'
run_model <- function(cohort_data = NA,
                      make_plots = TRUE,
                      sink_out = FALSE) {

  data("policies_ls")
  policies <- seq_along(policies_ls)

  run <- list(src_correct = NULL,
              start_runtime = proc.time())

  if (sink_out) {
    msges <- file("output/messages.Rout", open = "wt")
    sink(msges, type = "message")
    on.exit(sink(type = "message"))
  }

  policy <<- NA

  for (pp in policies) {

    policy <<- pp

    try_out <-
      try(
        run_policy(cohort_data,
                   make_plot)
      )

    handle_try_error(try_out)

    run$src_correct <- c(run$src_correct,
                         !inherits(try_out, "try-error"))
  }

  if (make_plots) plots_and_tables_policies()

  run_final_message(run)
}
