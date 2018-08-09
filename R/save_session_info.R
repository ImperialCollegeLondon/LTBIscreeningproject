
#' Save session info
#'
#' @param file string
#'
#' @return
#' @export
#' @import sessioninfo
#' @import git2r
#'
save_session_info <- function(file) {

  sink(here(file))
  on.exit(sink())

  print(sessioninfo::session_info())
  print(git2r::repository())
}
