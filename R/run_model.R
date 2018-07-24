
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

  plots_and_tables_policies()

  elapsed <- proc.time() - runtime

  message(" run time: ", green(elapsed['elapsed']/60), " mins")
  message(" scenarios source correctly: ", green(sources_correctly))
}
