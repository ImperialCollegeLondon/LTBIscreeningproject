#' test constructor
#'
#' @param sens
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
#'
test <- function(sens,
                 spec) {
  res <-
    list(sens = sens,
         spec = spec)

  class(res) <- c("test", class(res))

  return(res)
}

#' branch_unif_params constructor
#'
#' @param pmin
#' @param pmax
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#'
branch_unif_params <- function(pmin,
                               pmax,
                               name) {
  res <-
    list(pmin = pmin,
         pmax = pmax,
         name = name)

  class(res) <- c("branch_unif_params", class(res))

  return(res)
}


##TODO: where sens, spec used can now use method... eg branch assignment
