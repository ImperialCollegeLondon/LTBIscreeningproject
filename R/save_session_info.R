
#' save_session_info
#'
#' @param file
#'
#' @return
#' @export
#' @import sessioninfo
#' @import git2r
#'
#' @examples
save_session_info <- function(file) {

  sink(file)
  on.exit(sink())

  print(sessioninfo::session_info())
  print(git2r::repository())
}
