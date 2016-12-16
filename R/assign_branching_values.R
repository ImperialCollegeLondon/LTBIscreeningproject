
#' Assign Branching Values to Decision Tree
#'
#' @param osNode.cost data.tree object
#' @param osNode.health data.tree object
#' @param scenario_parameter_p
#' @param scenario_parameter_cost
#'
#' @return
#' @export
#'
#' @examples
assign_branch_values <- function(osNode.cost,
                                 osNode.health,
                                 scenario_parameter_p,
                                 scenario_parameter_cost) {

  if(class(osNode.cost)!="costeffectiveness_tree") stop("")
  if(class(osNode.health)!="costeffectiveness_tree") stop("")

  names.cost <- unique(scenario_parameter_cost$node)
  names.p <- unique(scenario_parameter_p.melt$node)

  # assign branching _probabilities_
  for (node_p in names.p){

    vals <- subset(scenario_parameter_p, node==node_p, select = p)

    osNode.cost$Set(p = vals,
                    filterFun = function(x) x$name==node_p)

    osNode.health$Set(p = vals,
                      filterFun = function(x) x$name==node_p)
  }

  # assign branching _costs_
  for (node_cost in names.cost){

    vals <- subset(x = scenario_parameter_cost, subset = node==node_cost)

    osNode.cost$Set(distn = vals$distn,
                    filterFun = function(x) x$name==node_cost)

    osNode.cost$Set(min = vals$min,
                    filterFun = function(x) x$name==node_cost)

    osNode.cost$Set(max = vals$max,
                    filterFun = function(x) x$name==node_cost)
  }
}
