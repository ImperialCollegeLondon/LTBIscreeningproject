
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


library(LTBIscreeningproject)
library(miscUtilities)
library(dplyr)
library(tibble)
library(arm)


wtp <- '20000'
# wtp <- 30000

fldr <- here::here("ext-data", "runs_2_effic60", "18_to_35_in_2009", "policy_003")
file_names <- list.files(fldr, full.names = TRUE, pattern = ".RData")
lapply(file_names, load, .GlobalEnv)

ce_res1 <- combine_popmod_dectree_res(cohort, interv, popmod_res, dectree_res, folders)

fldr <- here::here("ext-data", "runs_2_effic80", "18_to_35_in_2009", "policy_003")
file_names <- list.files(fldr, full.names = TRUE, pattern = ".RData")
lapply(file_names, load, .GlobalEnv)

ce_res2 <- combine_popmod_dectree_res(cohort, interv, popmod_res, dectree_res, folders)

fldr <- here::here("ext-data", "runs_2_effic100", "18_to_35_in_2009", "policy_003")
file_names <- list.files(fldr, full.names = TRUE, pattern = ".RData")
lapply(file_names, load, .GlobalEnv)

ce_res3 <- combine_popmod_dectree_res(cohort, interv, popmod_res, dectree_res, folders)

ce_res <- ce_res1
ce_res$ce_incr$e <- cbind(ce_res1$ce_incr$e[ ,c(1, 25, 50, 75, 100, 121)],
                          ce_res2$ce_incr$e[ ,c(25, 50, 75, 100, 121)],
                          ce_res3$ce_incr$e[ ,c(25, 50, 75, 100, 121)])
ce_res$ce_incr$c <- cbind(ce_res1$ce_incr$c[ ,c(1, 25, 50, 75, 100, 121)],
                          ce_res2$ce_incr$c[ ,c(25, 50, 75, 100, 121)],
                          ce_res3$ce_incr$c[ ,c(25, 50, 75, 100, 121)])

bcea_incr <- bcea_incremental(ce_res$ce_incr)

p <- boxplot_INMB(bcea_incr, folders, oneway = TRUE)

p + scale_x_discrete(breaks = 1:15,
                     labels = apply(expand.grid(c("0.6,0.6","0.75,0.7","0.9,0.8","0.5,0.95","1,1"), c("0.6","0.8","1")), 1, paste, collapse = ",")) +
  theme(axis.text.x = element_text(angle = 45))

