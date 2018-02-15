
#' decision_tree
#'
#' @param scenario_parameters
#' @param osNode.cost
#' @param osNode.health
#' @param N.mc
#'
#' @return
#' @export
#'
#' @examples
decision_tree <- function(scenario_parameters,
                          osNode.cost,
                          osNode.health,
                          N.mc){

  assign_branch_values(osNode.cost,
                       osNode.health,
                       parameter_p = scenario_parameters[scenario_parameters$val_type == "QALYloss", ],
                       parameter_cost = scenario_parameters[scenario_parameters$val_type == "cost", ])

  path_probs.screen <- calc_pathway_probs(osNode.cost)
  osNode.cost$Set(path_probs = path_probs.screen)

  p_complete_Tx <- p_complete_Tx(osNode.cost,
                                 who_levels = c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"))

  mc_cost <- MonteCarlo_expectedValues(osNode = osNode.cost,
                                       n = N.mc)

  mc_health <- MonteCarlo_expectedValues(osNode = osNode.health,
                                         n = N.mc)

  list(mc_cost = as.numeric(mc_cost$`expected values`),
       mc_health = as.numeric(mc_health$`expected values`),
       p_complete_Tx = as.numeric(p_complete_Tx))
}


