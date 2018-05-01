
#' decision_tree_cluster
#'
#' Calculate decision tree expected costs and QALY loss
#' for \code{N} simulations
#'
#' @param params long format array
#' @param N.mc integer
#' @param n.uk_tb integer
#' @param n.exit_tb integer
#' @param cost_dectree Rds file names
#' @param health_dectree Rds file names
#'
#' @return list
#' @export
#'
#' @examples
#'
decision_tree_cluster <- function(params,
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
                       parameter_p = subset(params, val_type == "QALYloss"),
                       parameter_cost = subset(params, val_type == "cost"))

  osNode.cost$Set(path_probs = calc_pathway_probs(osNode.cost))
  osNode.health$Set(path_probs = calc_pathway_probs(osNode.health))

  subset_pop <- sample_subset_pop_dectree(osNode = osNode.cost,
                                          n = N.mc,
                                          sample_p = TRUE)

  p_LTBI_to_cured <- mean(subset_pop[ ,'p_LTBI_to_cured'])

  n_tb_screen <- MonteCarlo_n.tb_screen(p_LTBI_to_cured,
                                        n.uk_tb = n.uk_tb,
                                        n.all_tb = n.uk_tb + n.exit_tb,
                                        n = N.mc)

  mc_cost <- MonteCarlo_expectedValues(osNode = osNode.cost,
                                       n = N.mc)

  mc_health <- MonteCarlo_expectedValues(osNode = osNode.health,
                                         n = N.mc)

  osNode.cost$Set(weighted_sampled =
                    osNode.cost$Get('path_probs') * osNode.cost$Get('sampled'))
  osNode.health$Set(weighted_sampled =
                      osNode.health$Get('path_probs') * osNode.health$Get('sampled'))

  list(mc_cost = as.numeric(mc_cost$`expected values`),
       mc_health = as.numeric(mc_health$`expected values`),
       n_tb_screen_all = n_tb_screen$n.tb_screen.all_tb,
       n_tb_screen_uk  = n_tb_screen$n.tb_screen.uk_tb,
       p_LTBI_to_effectiveTx = p_LTBI_to_cured,
       subset_pop = subset_pop,
       osNode.cost = osNode.cost,
       osNode.health = osNode.health,
       call = mcall,
       N.mc = N.mc)
}

