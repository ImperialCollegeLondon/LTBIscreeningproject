# *******************************************
# LTBI screening
# N Green
#
# high-level policy runner
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

# source("scripts/create_LTBI_input_workspace.R")

run_model()
