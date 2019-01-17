
#' rows_first_n_ids
#'
#' Finds the rows corresponding to the first \code{n} individuals
#' by ascending id numbers.
#'
#' @param id_avoid IDs, may have gaps/missing numbers
#' @param prop_avoid probability
#'
#' @return logical vector length ids
#' @export
#'
#' @seealso sample_avoid_lg, rsample_n_ids
#'
rows_first_n_ids <- function(id_avoid,
                             prop_avoid) {

  if (is.na(prop_avoid)) return(NULL)

  # remove NAs
  id_avoid_only <- id_avoid[!is.na(id_avoid)]

  index_avoid <- seq_len(length(id_avoid_only)*prop_avoid)

  who_avoid <- sort(id_avoid_only)[index_avoid]

  return(id_avoid %in% who_avoid)
}


#' sample_avoid_lg
#'
#' @param id_avoided_tb
#' @param prop_avoided probability
#' @param ordered random or ordered; TRUE/FALSE
#'
#' @return
#' @export
#'
#' @seealso rows_first_n_ids, rsample_n_ids
#'
sample_avoid_lg <- function(id_avoided_tb,
                            prop_avoided,
                            ordered) {
  res <-
    if (ordered) {
      rows_first_n_ids(id_avoided_tb,
                       prop_avoided)
    }else{
      rsample_n_ids(id_avoided_tb,
                    prop_avoided)}

  return(res)
}


#' rsample_n_ids
#'
#' @param id_avoid
#' @param prop_avoid probability
#'
#' @return
#' @export
#'
#' @seealso sample_avoid_lg, rows_first_n_ids
#'
rsample_n_ids <- function(id_avoid,
                          prop_avoid) {

  num_tb <- length(id_avoid)
  index_id <-
    sample(seq_along(id_avoid),
           size = prop_avoid*num_tb)
  res <- rep(FALSE, num_tb)
  res[index_id] <- TRUE

  return(res)
}

