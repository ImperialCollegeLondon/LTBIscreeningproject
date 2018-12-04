
#' ---
#' title: "LTBI screening model:
#' CE boundary plot for a range of effectiveness"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

# the simulation model output are plotted
# as well as the meta-regression predictions


library(LTBIscreeningproject)
library(miscUtilities)
library(dplyr)
library(tibble)
library(arm)


wtp <- '20000'
# wtp <- '30000'

run_names <- c("runs_2_effic60","runs_2_effic65","runs_2_effic70","runs_2_effic75",
               "runs_2_effic80","runs_2_effic85","runs_2_effic90","runs_2_effic95","runs_2_effic100")

sim_INMB <- vector("list")
plot_data <- vector("list")

for (i in seq_along(run_names)) {


  fldr <- here::here("ext-data", run_names[i], "18_to_35_in_2009", "policy_003")
  file_names <- list.files(fldr, full.names = TRUE, pattern = ".RData")
  lapply(file_names, load, .GlobalEnv)

  ce_res <- combine_popmod_dectree_res(cohort, interv, popmod_res, dectree_res, folders)
  bcea_incr <- bcea_incremental(ce_res$ce_incr)

  # simulation output
  sim_INMB[[run_names[i]]] <- bcea_to_plotdata(bcea_incr, folders, wtp_threshold = wtp)
  # meta-regression
  pred_INMB <- nmb_predictions(ce_res, folders)

  plot_data[[run_names[i]]] <- pred_INMB[[wtp]]
}

merged_data <- plyr::join_all(plot_data, by = c("Start_Treatment_p", "Complete_Treatment_p"))
merged_sim <- plyr::join_all(sim_INMB, by = c("Start_Treatment_p", "Complete_Treatment_p"))

p <- ce_boundary_line_plot(merged_data,
                           folders = NA)
p

q <- ce_boundary_line_plot(merged_sim,
                           folders = NA)
q


## plot all together
# merged_all <- merge(merged_sim, merged_data, by = c("Start_Treatment_p", "Complete_Treatment_p"), all = FALSE)
# pq <- ce_boundary_line_plot(merged_all,
#                             folders = NA)
# pq

