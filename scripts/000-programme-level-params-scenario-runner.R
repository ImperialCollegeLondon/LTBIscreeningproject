# *****************************************
# LTBI screening
# N Green
#
# high-level global scenario runner
# for a deterministic sensitivity analysis
# of screening programme
#
# *****************************************


rm(list = ls())

devtools::load_all(".")

# source("scripts/create_LTBI_input_workspace.R")

data("global-parameters-scenarios")
data("global-parameters-scenarios_ls")

load(file = "ext-data/LTBI_input_workspace.RData")

home_dir <- find.package("LTBIscreeningproject")

sources_correctly <- NULL

# global_run <- 1
for (global_run in 1:6) {

  print(sprintf("[ programme level parameters ] scenario: %d", global_run))

  try_out <- try(source("scripts/00-main.R"))

  if (inherits(try_out, "try-error")) {
    setwd(home_dir)
  }

  sources_correctly <- c(sources_correctly, !inherits(try_out, "try-error"))
}
