
#' rows_first_n_ids
#'
#' Finds the rows corresponding to the first \code{n} individuals
#' by ascending id numbers.
#'
#' @param id_avoided IDs, may have gaps/missing numbers
#' @param n_avoided Number integer
#'
#' @return logical vector
#' @export
#'
#' @examples
rows_first_n_ids <- function(id_avoided, n_avoided) {

  id_avoided <- sort(id_avoided)[seq_len(n_avoided)]
  id_avoided %in% id_avoided
}
