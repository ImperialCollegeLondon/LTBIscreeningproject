
#' insert_dectree_cost
#'
#' ##TODO: use for prep script
#'
#' @param names node labels
#' @param costs
#' @param osNode
#'
#' @return
#' @export
#'
#' @examples
#'
insert_dectree_cost <- function(names,
                                costs,
                                osNode) {

  map2(.x = costs,
       .y = names,
       .f = function(x,y) {
         osNode$Set(min = x,
                    filterFun = function(x) x$name == y)
         osNode$Set(max = x,
                    filterFun = function(x) x$name == y)
       })
}
