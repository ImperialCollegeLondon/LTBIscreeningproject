
#' Incremental Net (Monetary) Benefit
#'
#' Differences are `intervention` - `status-quo`.
#'
#' @param e Effectiveness i.e. health e.g. QALYs
#' @param c Costs
#' @param ref Reference column. Default: 1
#' @param wtp Willingness to pay threshold
#'
#' @return Vector
#' @export
#'
#' @examples
#'
calc.INMB <- function(e, c, ref = 1, wtp = 20000){

  if (wtp < 0)
    stop("Willingness to pay must be non-negative.")

  if (any(dim(c) != dim(e)))
    warning("Dimensions of e and c do not match.")

  n.comparators <- dim(e)[2]

  # Define reference & comparator intervention
  ints <- 1:n.comparators
  comp <- ints[-ref]

  delta.e <- as.data.frame(e[ ,comp] - e[ ,ref])
  delta.c <- as.data.frame(c[ ,comp] - c[ ,ref])

  INMB <- (colMeans(delta.e) * wtp) - colMeans(delta.c)

  return(INMB)
}


#' Incremental Cost Effectiveness Ratio
#'
#' Differences are `intervention` - `status-quo`.
#'
#' @param e Effectiveness i.e. health e.g. QALYs
#' @param c Costs
#' @param ref Reference column. Defaults: 1
#'
#' @return
#' @export
#'
#' @examples
#'
calc.ICER <- function(e, c, ref = 1){

  if (any(dim(c) != dim(e)))
    warning("Dimensions of e and c do not match.")

  n.comparators <- dim(e)[2]

  # Define reference & comparator intervention
  ints <- 1:n.comparators
  comp <- ints[-ref]

  delta.e <- as.data.frame(e[ ,comp] - e[ ,ref])
  delta.c <- as.data.frame(c[ ,comp] - c[ ,ref])

  ICER <- colMeans(delta.c)/colMeans(delta.e)

  return(ICER)
}
