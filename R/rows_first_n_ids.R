
#' rows_first_n_ids
#'
#' Finds the rows corresponding to the first \code{n} individuals
#' by ascending id numbers.
#'
#' @param id_avoid IDs, may have gaps/missing numbers
#' @param prop_avoid
#'
#' @return logical vector
#' @export
#'
#' @examples
rows_first_n_ids <- function(id_avoid,
                             prop_avoid) {

  if (is.na(prop_avoid)) return(NULL)

  # remove NAs
  id_avoid_only <- id_avoid[!is.na(id_avoid)]

  index_avoid <- seq_len(length(id_avoid_only)*prop_avoid)

  who_avoid <- sort(id_avoid_only)[index_avoid]
  id_avoid %in% who_avoid
}


