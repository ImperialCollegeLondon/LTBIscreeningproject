
#' ---
#' title: "LTBI screening model:
#' hack to use previous runs but changing a unit cost"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

# we can force the QALYs to be exactly the same
# by recycling a single scenario and adjusting
# retrospectively for a different unit test cost
# c_new = c_old + p(accept)*diff_unit_cost

dectree_res2$`2` <- dectree_res2$`1`
dectree_res2$`2`$mc_cost <- dectree_res2$`1`$mc_cost + 0.72*25
dectree_res2$`3`$mc_cost <- dectree_res2$`2`$mc_cost + 0.72*50

popmod_res$QALYgain_person[[2]] <- popmod_res$QALYgain_person[[1]]
popmod_res$QALYgain_person[[3]] <- popmod_res$QALYgain_person[[1]]

popmod_res$QALY.screened_person[[2]] <- popmod_res$QALY.screened_person[[1]]
popmod_res$QALY.screened_person[[3]] <- popmod_res$QALY.screened_person[[1]]
