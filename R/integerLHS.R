
#' integerLHS
#'
#' \url{https://stat.ethz.ch/pipermail/r-help/2007-January/124143.html}
#'
#' @param n
#' @param intGroups
#'
#' @return
#' @export
#'
#' @examples
#' integerLHS(10, list(1:10, 31:40))
#' integerLHS(5, list(1:10, 31:40))
#' integerLHS(2, list(1:10, 31:40))
#' integerLHS(5, list(1:20, 31:60, 101:115))
#' integerLHS(5, list(seq(2,20,2), 31:60, 101:115))
#'
integerLHS <- function(n,
                       intGroups) {

  stopifnot(all(lapply(intGroups, function(X)
    length(X) %% n) == 0))
  stopifnot(require(lhs))
  stopifnot(is.list(intGroups))
  ranges <- lapply(intGroups, function(X)
    max(X) - min(X))
  A <- matrix(nrow = n, ncol = length(intGroups))

  for (j in 1:length(ranges)) {
    sequ <- order(runif(n))
    if (length(intGroups[[1]]) > 1) {
      spacing <- intGroups[[j]][2] - intGroups[[j]][1]
    } else
      stop("must have more than 1 intGroup")

    for (k in 1:n) {
      i <- sequ[k]
      a <- min(intGroups[[j]]) + (i - 1) * (ranges[[j]] + spacing) / n
      b <- min(intGroups[[j]]) + i * (ranges[[j]] + spacing) / n - 1
      if (a < b) {
        A[k, j] <- sample(seq(a, b, spacing), 1)
      } else if (a == b) {
        A[k, j] <- a
      } else
        stop("error")
    }
  }
  return(A)
}
