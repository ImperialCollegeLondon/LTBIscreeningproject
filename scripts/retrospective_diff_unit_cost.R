
#' ---
#' title: "LTBI screening model:
#' hack to use previous runs but changing a unit test cost"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

# we can force the QALYs and other costs to be
# exactly the same between different scenarios
# by recycling a single scenario and adjusting
# retrospectively for a different unit test cost
# i.e.
# c_new = c_old + p(accept) * (unit_cost1 - unit_cost2)

# runs 1.1
unit_cost <- c(25, 50, 100)
p_accept <- 0.72

load(here::here("ext-data", "runs_1.1", "18_to_35_in_2009", "policy_003", "dectree_res.RData"))

dectree_res$`2` <- dectree_res$`1`
dectree_res$`2`$mc_cost <- dectree_res$`1`$mc_cost + p_accept*(unit_cost[2] - unit_cost[1])
dectree_res$`3`$mc_cost <- dectree_res$`2`$mc_cost + p_accept*(unit_cost[3] - unit_cost[2])

load(here::here("ext-data", "runs_1.1", "18_to_35_in_2009", "policy_003", "popmod_res.RData"))

popmod_res$QALYgain_person[[2]] <- popmod_res$QALYgain_person[[1]]
popmod_res$QALYgain_person[[3]] <- popmod_res$QALYgain_person[[1]]

popmod_res$QALY.screened_person[[2]] <- popmod_res$QALY.screened_person[[1]]
popmod_res$QALY.screened_person[[3]] <- popmod_res$QALY.screened_person[[1]]


save(dectree_res, file = here::here("ext-data", "runs_1.1", "18_to_35_in_2009", "policy_003", "dectree_res_fixed.RData"))
save(popmod_res, file = here::here("ext-data", "runs_1.1", "18_to_35_in_2009", "policy_003", "popmod_res_fixed.RData"))

