
#' ---
#' title: "LTBI screening model:
#' high-level policy runner
#' for a (deterministic) sensitivity analysis
#' of screening programme scenarios"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


rm(list = ls())
devtools::load_all(".")

pkgs = c("parallel",
         "assertthat",
         "miscUtilities",
         "crayon",
         "tibble",
         "memoise",
         "treeSimR",
         "QALY",
         "here",
         "arm",
         "magrittr")
inst = lapply(pkgs, library, character.only = TRUE)

save_session_info("session_info.txt")

# source("scripts/create_input_workspace.R")

run_model()
