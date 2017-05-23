
# total prob successfully cured of LTBI for each WHO category
# number of ways to effectively complete Tx per LTBI

p_complete_Tx <- function(osNode.cost,
                          who_levels) {

  LTBItreeClone <- Clone(osNode.cost$`(50,150]`$LTBI,
                         pruneFun = function(x) myPruneFun(x, "Effective"))

  effective.leafCount <- LTBItreeClone$leafCount

  osNode.p_complete_Tx <- osNode.cost$Get('path_probs',
                                          filterFun = function(x) x$name == "Effective")

  p_LTBI <- osNode.cost$Get('path_probs',
                            filterFun = function(x) x$name == "LTBI")

  # sum path_probs over all leafs in WHO groups

  effective.groups <- rep(seq_along(p_LTBI),
                          each = effective.leafCount)

  p_effective <-
    aggregate(osNode.p_complete_Tx,
              by = list(effective.groups),
              FUN = sum) %>%
    select(x) %>%
    unlist()

  p_complete_Tx <- set_names(x = p_effective/p_LTBI,
                             nm = who_levels)

  return(p_complete_Tx)
}
