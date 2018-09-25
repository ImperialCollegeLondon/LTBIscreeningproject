
#' handle_try_error
#'
#' @param try_out
#'
#' @return
#' @export
handle_try_error <- function(try_out) {

  if (inherits(try_out, "try-error")) {
    setwd(here::here())
    sink(type = "message")
  }
}
