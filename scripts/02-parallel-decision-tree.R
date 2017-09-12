#****************************************************************
# LTBI screening model
# N Green
# Aug 2017
#
# the output is saved in Q:/R/cluster--LTBI-decision-tree
# because the alternative way of running is on the DIDE cluster
# so all of the results are in the same place
#
# https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/


library(parallel)

if (getwd() != "Q:/R/cluster--LTBI-decision-tree") {
  exit_wd <- getwd()}

source("scripts/cluster-prep-decisiontree-data.R")

setwd("Q:/R/cluster--LTBI-decision-tree")

sources <- list.files(pattern = "[.]R$")[!grepl(x = list.files(pattern = "[.]R$"), pattern = "^cluster-master")]

source("decision_tree_cluster.R")


# parallel config ---------------------------------------------------------

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

n.uk_tb <- unlist(n.uk_tb)
n.exit_tb <- unlist(n.exit_tb)

clusterExport(cl, "n.uk_tb")
clusterExport(cl, "n.exit_tb")
clusterExport(cl, "N.mc")

clusterEvalQ(cl, library(data.tree))
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(treeSimR))

set.seed(12345)

ptm <- proc.time()

dectree_res <- parLapply(cl, scenario_parameters, decision_tree_cluster,
                         N.mc = N.mc,
                         n.uk_tb = n.uk_tb,
                         n.exit_tb = n.exit_tb)

proc.time() - ptm

stopCluster(cl)

saveRDS(dectree_res, file = cluster_output_filename)

setwd(exit_wd)


##to debug
# res <- lapply(scenario_parameters, decision_tree_cluster,
#               N.mc = N.mc,
#               n.uk_tb = n.uk_tb,
#               n.exit_tb = n.exit_tb)
