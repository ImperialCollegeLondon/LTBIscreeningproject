
#' rows_first_n_ids
#'
#' @param id_avoided
#' @param n_avoided
#'
#' @return
#' @export
#'
#' @examples
rows_first_n_ids <- function(id_avoided, n_avoided) {

  id_avoided <- sort(id_avoided)[seq_num(n_avoided)]
  id_avoided %in% id_avoided
}
