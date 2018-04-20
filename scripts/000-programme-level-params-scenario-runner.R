# *******************************************
# LTBI screening
# N Green
#
# high-level global policy runner
# for a (deterministic) sensitivity analysis
# of screening programme scenarios
#
# *******************************************


rm(list = ls())

devtools::load_all(".")

library(parallel)
library(assertthat)
library(miscUtilities)
library(crayon)
library(tibble)
library(memoise)
library(QALY)

save_session_info("session_info.txt")

##########
# inputs #
##########

# source("scripts/create_LTBI_input_workspace.R")

data("intervention_constants")
data("cost_effectiveness_params")
data("scenario_parameters")
data("model_input_cohort")
data("global-parameters-scenarios")
data("global-parameters-scenarios_ls")

home_dir <- find.package("LTBIscreeningproject")

sources_correctly <- NULL

runtime <- proc.time()

Rout <- file("output/messages.Rout", open = "wt")
sink(Rout, type = "message")


#########
# model #
#########

# global_run <- 1
# for (global_run in c(1,3,5)) {
for (global_run in seq_along(global_params_scenarios_ls)) {

  try_out <- try(
    source("scripts/00-main.R")
  )

  if (inherits(try_out, "try-error")) {
    setwd(home_dir)
  }

  sources_correctly <- c(sources_correctly, !inherits(try_out, "try-error"))
}

source("scripts/combine-costeffectiveness-tables.R")

##TODO: policy-level CE planes


elapsed <- proc.time() - runtime

message("run time: ", elapsed['elapsed']/60)
message("scenarios source correctly: ", sources_correctly)

sink(type = "message")

file.copy(from = "session_info.txt",
          to = pastef(parent_folder, "session_info.txt"),
          overwrite = TRUE)
file.remove("session_info.txt")

