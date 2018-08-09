
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

library(parallel)
library(assertthat)
library(miscUtilities)
library(crayon)
library(tibble)
library(memoise)
library(QALY)
library(here)

save_session_info("session_info.txt")

# source("scripts/create_input_workspace.R")

run_model()
