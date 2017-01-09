
library(testthat)
library(data.tree)
library(magrittr)


CEtree <- treeSimR::costeffectiveness_tree(yaml_tree = "C:/Users/Nathan/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/LTBI_dtree-cost_SIMPLE-TEST.yaml")
osNode.cost <- CEtree$osNode

# calculate total probabilities along each branch, from root to leaf
path_probs.screen <- treeSimR::calc_pathway_probs(osNode.cost)
osNode.cost$Set(path_probs = 10000 * path_probs.screen)

print(osNode.cost, "type", "p", "path_probs", "distn",
      "mean", "sd", "min", "max", "a", "b", "shape", "scale", limit = NULL)


costs <- list()
costs$test <- 30
costs$treat_init <- 50
costs$drugs <- 100
costs$treatment_support <- 100
costs$active_TB <- 5000


# Cost components	checks -------------------------------------------------------

# No. tests
expect_equal(osNode.cost$Get('path_probs',
                             filterFun = function(x) x$name=="Agree to Screen") %>% sum(),
             3500)

# Test positive
expect_equal(osNode.cost$Get('path_probs',
                             filterFun = function(x) x$name=="Test Positive") %>% sum(),
             1120)

# No.start treatment
expect_equal(osNode.cost$Get('path_probs',
                             filterFun = function(x) x$name=="Start Treatment") %>% sum(),
             560)

# No. completing
expect_equal(osNode.cost$Get('path_probs',
                             filterFun = function(x) x$name=="Complete Treatment") %>% sum(),
             504)

# Active TB
expect_equal(0.05 * (3000 - osNode.cost$Get('path_probs',
                                            filterFun = function(x) x$name=="Effective") %>% sum()),
             129.800625)


