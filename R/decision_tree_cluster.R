
#' decision_tree_cluster
#'
#' Calculate decision tree expected costs and QALY loss
#' for N simulations
#'
#' @param parameters
#' @param N.mc
#' @param n.uk_tb
#' @param n.exit_tb
#' @param cost_dectree
#' @param health_dectree
#'
#' @return
#' @export
#'
#' @examples
decision_tree_cluster <- function(parameters,
                                  N.mc = 2,
                                  n.uk_tb,
                                  n.exit_tb,
                                  cost_dectree = "osNode_cost_2009.Rds",
                                  health_dectree = "osNode_health_2009.Rds"){

  mcall <- match.call()

  osNode.cost <- readRDS(file = cost_dectree)
  osNode.health <- readRDS(file = health_dectree)

  assign_branch_values(osNode.cost,
                       osNode.health,
                       parameter_p = subset(parameters, val_type == "QALYloss"),
                       parameter_cost = subset(parameters, val_type == "cost"))

  path_probs.screen <- calc_pathway_probs(osNode.cost)
  osNode.cost$Set(path_probs = path_probs.screen)

  p_LTBI_to_effectiveTx <- p_complete_Tx(osNode.cost = osNode.cost,
                                         who_levels = c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"))

  mc_n.tb_screen <- MonteCarlo_n.tb_screen(p_LTBI_to_effectiveTx,
                                           n.uk_tb = n.uk_tb,
                                           n.all_tb = n.uk_tb + n.exit_tb,
                                           n = N.mc)

  mc_cost <- MonteCarlo_expectedValues(osNode = osNode.cost,
                                       n = N.mc)

  mc_health <- MonteCarlo_expectedValues(osNode = osNode.health,
                                         n = N.mc)

  subset_pop <- subset_pop_dectree(osNode.cost)

  list(mc_cost = as.numeric(mc_cost$`expected values`),
       mc_health = as.numeric(mc_health$`expected values`),
       mc_n.tb_screen = mc_n.tb_screen,
       p_LTBI_to_effectiveTx = p_LTBI_to_effectiveTx,
       subset_pop = subset_pop,
       call = mcall,
       N.mc = N.mc)
}

