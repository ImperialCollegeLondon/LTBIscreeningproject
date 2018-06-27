#****************************************************************
# LTBI screening model
# N Green
# Aug 2017
#
# the output is also saved in Q:/R/cluster--LTBI-decision-tree
# because the alternative way of running is on the DIDE cluster
# so all of the results are in the same place
#
# https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/


# library(parallel)

## MAKE SURE THIS IS UPTO DATE WITH PACKAGE ##
cluster_folder <- "Q:/R/cluster--LTBI-decision-tree"

if (getwd() != cluster_folder) {

  exit_wd <- getwd()
  setwd(cluster_folder)
}

exclude_regex <- "^cluster-master"
dir_R_files <- list.files(pattern = "[.]R$")
sources <- dir_R_files[!grepl(x = dir_R_files, pattern = exclude_regex)]


# parallel config ---------------------------------------------------------

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)#, outfile = "temp_logfile.txt")

clusterEvalQ(cl, library(data.tree))
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(treeSimR))
clusterEvalQ(cl, library(purrr))
clusterEvalQ(cl, library(LTBIscreeningproject))

clusterEvalQ(cl, source("subset_pop_dectree.R"))
clusterEvalQ(cl, source("sample_subset_pop_dectree.R"))

set.seed(12345)

ptm <- proc.time()

dectree_res <- parLapplyLB(cl,
                           scenario_parameters,
                           fun = decision_tree_cluster,
                           N.mc = interv$N.mc)
(proc.time() - ptm)/60

stopCluster(cl)


# save --------------------------------------------------------------------

# saveRDS(dectree_res, file = pastef("output", cluster_output_filename))
save(dectree_res, file = pastef(exit_wd, diroutput, "dectree_res.RData"))

readr::write_csv(x = my_ToDataFrameTypeCol(dectree_res[[1]]$osNode.cost,
                                           "distn", "max", "min", "p", "pmin", "pmax", "path_probs", "payoff",
                                           "sampled", "weighted_sampled", "pmax", "pmin", "scale", "shape", "type"),
                 path = pastef(exit_wd, diroutput, "dectree_cost.csv"))

readr::write_csv(x = my_ToDataFrameTypeCol(dectree_res[[1]]$osNode.health,
                                           "distn", "max", "min", "p", "pmin", "pmax", "path_probs", "payoff",
                                           "sampled", "weighted_sampled", "pmax", "pmin", "scale", "shape", "type"),
                 path = pastef(exit_wd, diroutput, "dectree_health.csv"))

setwd(exit_wd)


# dectree_res <- lapply(scenario_parameters[1:3],
#                       decision_tree_cluster,
#                       N.mc = 3)


