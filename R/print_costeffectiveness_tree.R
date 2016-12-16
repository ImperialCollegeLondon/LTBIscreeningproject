
#' Print Method for Cost-Effectiveness Trees
#'
#' @param osNode
#'
#' @return
#' @export
#'
#' @examples
print.costeffectiveness_tree <- function(decisiontree){

  do.call(print, c(decisiontree, decisiontree$fieldsAll), limit = NULL)
}
