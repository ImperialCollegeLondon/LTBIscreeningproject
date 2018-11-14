
#' ---
#' title: "LTBI screening model:
#' create grid of parameter values
#  representing each scenario to Excel"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


library(xlsx)
library(dplyr)


parameter_p <-
  expand.grid(
    # 'Agree to Screen' = c(0.6, 0.8, 1),
    'Start Treatment' = seq(0.5, 1, by = 0.02),
    'Complete Treatment' = seq(0.5, 1, by = 0.02)
    # 'Effective' = c(0.6, 0.8, 1),
    # 'Sensitivity' = 0.84,
    # 'Specificity' = 0.99
  ) %>%
  mutate(
    'scenario' = seq_len(nrow(.)))

# costs
agreetoscreen <- data.frame('node' = 'Agree to Screen',
                            'min' = 20,
                            'max' = 20,
                            'distn' = "unif",
                            'scenario' = seq_len(nrow(parameter_p)))
completeTx <- data.frame('node' = 'Complete Treatment',
                         'min' = 531,
                         'max' = 531,
                         'distn' = "unif",
                         'scenario' = seq_len(nrow(parameter_p)))
notcompleteTx <- data.frame('node' = 'Not Complete Treatment',
                            'min' = 88.5,
                            'max' = 88.5,
                            'distn' = "unif",
                            'scenario' = seq_len(nrow(parameter_p)))

parameter_cost <- rbind(agreetoscreen, completeTx, notcompleteTx)

# save

# filename <- here::here("data", "scenario-parameter-values_fullfactorial.xlsx")
filename <- here::here("data", "scenario-parameter-values_startTx_completeTx_steps02")

write.xlsx(parameter_p, sheetName = "p",
           file = paste0(filename, ".xlsx"), row.names = FALSE)

write.xlsx(parameter_cost, sheetName = "cost",
           file = paste0(filename, ".xlsx"), row.names = FALSE, append = TRUE)

write.csv(parameter_p, file = paste0(filename, ".csv"))
