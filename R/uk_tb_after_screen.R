#'  Sample updated active TB status after screening
#'
#' @param is.tb is individual active TB case (logical)
#' @param prob probability of success (e.g. completing treatment)
#'
#' @return
#' @export
#'
#' @examples
#'
uk_tb_after_screen <- function(is.tb, prob){

  n.tb <- sum(is.tb)
  as.numeric(prob[is.tb] < runif(n.tb))
}
