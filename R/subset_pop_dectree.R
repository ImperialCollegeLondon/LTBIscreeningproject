
#' Subset populations of decision tree
#'
#' Specific to the LTBI screening model,
#' this gives the total probabilities of
#' particular state on the pathway by summing
#' across nodes, using pathprobs.
#'
#' @param osNode data.tree object
#'
#' @return data.frame of probabilities
#' @export
#'
#' @examples
#'
subset_pop_dectree <- function(osNode) {

  osNode$Set(node_names = osNode$Get('name'))

  LTBI_trees <- Traverse(osNode, traversal = "post-order",
                         filterFun = function(x) x$node_names == 'LTBI')

  nonLTBI_trees <- Traverse(osNode, traversal = "post-order",
                            filterFun = function(x) x$node_names == 'non-LTBI')

  LTBI_pre <- osNode$Get('path_probs',
                         filterFun = function(x) x$node_names == 'LTBI')

  tests <- osNode$Get('path_probs',
                      filterFun = function(x) x$node_names == "Agree to Screen")

  LTBI_tests    <-
    LTBI_trees %>%
    map_dbl(function(x) x$Get('path_probs',
                              filterFun = function(x) x$node_names == "Agree to Screen"))

  LTBI_positive <- map_dbl(LTBI_trees,
                           function(x) x$Get('path_probs',
                                             filterFun = function(x) x$node_names == "Sensitivity"))
  nonLTBI_positive <- map_dbl(nonLTBI_trees,
                              function(x) x$Get('path_probs',
                                                filterFun = function(x) x$node_names == "1-Specificity"))

  startTx <- osNode$Get('path_probs',
                        filterFun = function(x) x$node_names == "Start Treatment")

  LTBI_startTx <- map_dbl(LTBI_trees,
                          function(x) x$Get('path_probs',
                                            filterFun = function(x) x$node_names == "Start Treatment"))

  completeTx <- osNode$Get('path_probs',
                           filterFun = function(x) x$node_names == "Complete Treatment")

  LTBI_completeTx <- map_dbl(LTBI_trees,
                             function(x) sum(x$Get('path_probs',
                                                   filterFun = function(x) x$node_names == "Complete Treatment")))

  cured <- osNode$Get('path_probs',
                      filterFun = function(x) x$node_names == "Effective")

  data.frame(LTBI_pre = sum(LTBI_pre),
             tests = sum(tests),
             positive = sum(LTBI_positive + nonLTBI_positive),
             startTx = sum(startTx),
             completeTx = sum(completeTx),
             cured = sum(cured),
             LTBI_post = sum(LTBI_pre) - sum(cured)) %>%
    mutate(p_LTBI_to_cured = cured/LTBI_pre,
           LTBI_tests = sum(LTBI_tests)/LTBI_pre,
           LTBI_positive = sum(LTBI_positive)/LTBI_pre,
           LTBI_startTx = sum(LTBI_startTx)/LTBI_pre,
           LTBI_completeTx = sum(LTBI_completeTx)/LTBI_pre)
}
