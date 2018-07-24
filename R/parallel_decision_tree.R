
#' parallel_decision_tree
#'
#' the output is also saved in Q:/R/cluster--LTBI-decision-tree
#' because the alternative way of running is on the DIDE cluster
#' so all of the results are in the same place
#'
#' https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
#'
#' @param scenario_parameters
#' @param interv
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
parallel_decision_tree <- function(scenario_parameters,
                                   interv,
                                   folders) {

  ## MAKE SURE CLUSTER FOLDER IS UPTO DATE WITH PACKAGE ##
  wdir <- getwd()
  on.exit(setwd(wdir))
  setwd("Q:/R/cluster--LTBI-decision-tree")

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
  run_time <- (proc.time() - ptm)/60
  # print(run_time)

  stopCluster(cl)

  # saveRDS(dectree_res, file = pastef("output", cluster_output_filename))
  save(dectree_res, file = pastef(folders$output$scenario, "dectree_res.RData"))

  readr::write_csv(x = my_ToDataFrameTypeCol(dectree_res[[1]]$osNode.cost,
                                             "distn", "max", "min", "p", "pmin", "pmax", "path_probs", "payoff",
                                             "sampled", "weighted_sampled", "pmax", "pmin", "scale", "shape", "type"),
                   path = pastef(folders$output$scenario, "dectree_cost.csv"))

  readr::write_csv(x = my_ToDataFrameTypeCol(dectree_res[[1]]$osNode.health,
                                             "distn", "max", "min", "p", "pmin", "pmax", "path_probs", "payoff",
                                             "sampled", "weighted_sampled", "pmax", "pmin", "scale", "shape", "type"),
                   path = pastef(folders$output$scenario, "dectree_health.csv"))

  return(dectree_res)
}


