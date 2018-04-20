
#' save_session_info
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
save_session_info <- function(file) {

  sink(file)
  sessioninfo::session_info()
  git2r::repository()
  sink()
}
