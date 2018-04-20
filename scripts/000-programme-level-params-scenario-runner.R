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


sink("session_info.txt")
  sessioninfo::session_info()
  git2r::repository()
sink()

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


# global_run <- 4
# for (global_run in c(38,44,39,45)) {
for (global_run in seq_along(global_params_scenarios_ls)) {

  message(sprintf("[ policy level parameters ]\n scenario: %s", green(global_run)))

  try_out <- try(source("scripts/00-main.R"))

  if (inherits(try_out, "try-error")) {
    setwd(home_dir)
  }

  sources_correctly <- c(sources_correctly, !inherits(try_out, "try-error"))
}

source("scripts/combine-costeffectiveness-tables.R")

elapsed <- proc.time() - runtime

message("run time: ", elapsed['elapsed']/60)
message("scenarios source correctly: ", sources_correctly)

sink(type = "message")

file.copy(from = "session_info.txt",
          to = pastef(parent_folder, "session_info.txt"),
          overwrite = TRUE)
file.remove("session_info.txt")

