
#' insert_dectree_cost
#'
#' ##TODO: use for prep script
#'
#' @param names node labels
#' @param costs
#'
#' @return
#' @export
#'
#' @examples
#'
insert_dectree_cost <- function(names, costs) {

  map2(.x = costs,
       .y = names,
       .f = function(x,y) osNode.cost$Set(min = x,
                                          filterFun = function(x) x$name == y))
  map2(.x = costs,
       .y = names,
       .f = function(x,y) osNode.cost$Set(max = x,
                                          filterFun = function(x) x$name == y))
}
