
#' rows_first_n_ids
#'
#' Finds the rows corresponding to the first \code{n} individuals
#' by ascending id numbers.
#'
#' @param id_avoid IDs, may have gaps/missing numbers
#' @param n_avoid Number. Converted to integer floor
#'
#' @return logical vector
#' @export
#'
#' @examples
rows_first_n_ids <- function(id_avoid,
                             n_avoid) {

  n_avoid <-
    floor(n_avoid) %>%
    as.integer()

  if (is.na(n_avoid)) return(NULL)

  who_avoid <- sort(id_avoid)[seq_len(n_avoid)]
  id_avoid %in% who_avoid
}
