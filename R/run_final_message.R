
#' run_final_message
#'
#' @param run
#'
#' @return
#' @export
run_final_message <- function(run) {

  elapsed <- proc.time() - run$start_runtime

  message(" run time: ", green(elapsed['elapsed']/60), " mins")
  message(" scenarios source correctly: ", green(run$src_correct))

  return()
}
