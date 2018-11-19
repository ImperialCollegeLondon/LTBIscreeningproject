
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

  stopifnot(inherits(sens, "branch_unif_params"))
  stopifnot(inherits(spec, "branch_unif_params"))

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

  stopifnot(pmin >= 0, pmax <= 1)

  res <-
    list(pmin = pmin,
         pmax = pmax,
         name = name)

  class(res) <- c("branch_unif_params", class(res))

  return(res)
}

