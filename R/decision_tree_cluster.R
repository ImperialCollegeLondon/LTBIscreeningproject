
#' Decision tree
#'
#' Calculate decision tree expected costs and QALY loss
#' for \code{N} simulations.
#'
#' @param params an element of a scenario list with probabilities and costs to substitue into decision tree; long format array
#' @param N.mc number of simulations; integer
#' @param cost_dectree data.tree saved as Rds file names (string)
#' @param health_dectree data.tree saved as Rds file names (string)
#' @param out_datatree Output full datatree object? This may be large. Default: FALSE
#'
#' @return list
#' \itemize{
#'   \item mc_cost: each simulation total expected cost
#'   \item mc_health: each simulation total expected QALy loss
#'   \item subset_pop: cohort population sizes and probabilities at specific node or groups of nodes. Specifically calculates for individuals with LTBI since these are the subset of particular interest in term of cure; dataframe heading are LTBI_pre, tests, positive, startTx, completeTx, cured, LTBI_post, p_LTBI_to_cured, LTBI_tests, LTBI_positive, LTBI_startTx, LTBI_completeTx
#'   \item osNode.cost: data.tree object
#'   \item osNode.health: data.tree object
#'   \item call: original call with arguments
#'   \item N.mc: number of mnote-carlo simulations
#' }
#' @export
#'
#' @examples
#'
decision_tree_cluster <- function(params,
                                  N.mc = 2,
                                  cost_dectree = "osNode_cost_2009.Rds",
                                  health_dectree = "osNode_health_2009.Rds",
                                  out_datatree = FALSE){

  mcall <- match.call()

  osNode.cost <- readRDS(file = cost_dectree)
  osNode.health <- readRDS(file = health_dectree)

  assign_branch_values(osNode.cost,
                       osNode.health,
                       parameter_p = subset(params, val_type == "p"),
                       parameter_cost = subset(params, val_type == "cost"))

  osNode.cost$Set(path_probs = calc_pathway_probs(osNode.cost))
  osNode.health$Set(path_probs = calc_pathway_probs(osNode.health))

  subset_pop <- sample_subset_pop_dectree(osNode = osNode.cost,
                                          n = N.mc,
                                          sample_p = TRUE)

  mc_cost <- MonteCarlo_expectedValues(osNode = osNode.cost,
                                       n = N.mc)

  mc_health <- MonteCarlo_expectedValues(osNode = osNode.health,
                                         n = N.mc)

  osNode.cost$Set(weighted_sampled =
                    osNode.cost$Get('path_probs') * osNode.cost$Get('sampled'))
  osNode.health$Set(weighted_sampled =
                      osNode.health$Get('path_probs') * osNode.health$Get('sampled'))
  ##TODO: use Get(osNode, 'path_probs'*'sampled') and separate function defn

  if (!out_datatree) {
    osNode.cost <- NULL
    osNode.health <- NULL
  }

  list(mc_cost = as.numeric(mc_cost$`expected values`),
       mc_health = as.numeric(mc_health$`expected values`),
       subset_pop = subset_pop,
       osNode.cost = osNode.cost,
       osNode.health = osNode.health,
       call = mcall,
       N.mc = N.mc)
}

