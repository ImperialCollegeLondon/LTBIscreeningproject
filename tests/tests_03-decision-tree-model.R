
library(data.tree)
library(magrittr)


osNode.cost <- treeSimR::costeffectiveness_tree(yaml_tree = "C:/Users/Nathan/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/LTBI_dtree-cost_SIMPLE-TEST.yaml")


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


# Cost components		 -------------------------------------------------------

# No. tests 3500
osNode.cost$Get('path_probs', filterFun = function(x) x$name=="Agree to Screen") %>% sum()

# Test positive		1120
osNode.cost$Get('path_probs', filterFun = function(x) x$name=="Test Positive") %>% sum()

# No.start treatment		560
osNode.cost$Get('path_probs', filterFun = function(x) x$name=="Start Treatment") %>% sum()

# No. completing		504
osNode.cost$Get('path_probs', filterFun = function(x) x$name=="Complete Treatment") %>% sum()

# Active TB		129.800625
0.05 * (3000 - osNode.cost$Get('path_probs', filterFun = function(x) x$name=="Effective") %>% sum())


