
#' Incremental Net (Monetary) Benefit
#'
#' Differences are [intervention] - [status-quo].
#'
#' @param delta.e Difference in effectivness i.e. health e.g. QALYs
#' @param delta.c Difference in costs
#' @param wtp Willingness to pay threshold
#'
#' @return Value
#' @export
#'
#' @examples
#'
calc.INMB <- function(delta.e, delta.c, wtp = 20000){

  if(wtp<0)
    stop("Willingness to pay must be non-negative.")

  return((delta.e * wtp) - delta.c)
}


#' Incremental Cost Effectiveness Ratio
#'
#' Differences are [intervention] - [status-quo].
#'
#' @param e effectivness i.e. health e.g. QALYs
#' @param c costs
#' @param ref reference column. Defaults to 1
#'
#' @return
#' @export
#'
#' @examples
#'
calc.ICER <- function(e, c, ref = 1){

  if(any(dim(c)!=dim(e)))
    warning("Dimensions of e and c do not match.")

  n.sim <- dim(e)[1]
  n.comparators <- dim(e)[2]

  # Define reference & comparator intervention
  ints <- 1:n.comparators
  comp <- ints[-ref]
  n.comparisons <- n.comparators - 1

  delta.e <- as.data.frame(e[ ,comp] - e[ ,ref])
  delta.c <- as.data.frame(c[ ,comp] - c[ ,ref])

  ICER <- colMeans(delta.c)/colMeans(delta.e)

  return(ICER)
}
