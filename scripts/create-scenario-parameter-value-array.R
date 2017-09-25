# ********************************************
# project: LTBI screening
# N Green
# Nov 2016
#
# create grid of parameter values
# representing each scenario
# full-factorial design


library(xlsx)


parameter_p <-
  expand.grid('Start Treatment' = c(0.6, 0.8, 1),
              'Complete Treatment' = c(0.6, 0.8, 1),
              'Agree to Screen' = c(0.6, 0.8, 1),
              'Effective' = c(0.6, 0.8, 1),
              'Sensitivity' = 0.84,
              'Specificity' = 0.99) %>%
  mutate('Not Start Treatment' = 1 - `Start Treatment`,
             'Not Complete Treatment' = 1 - `Complete Treatment`,
             'Not Agree to Screen' = 1 - `Agree to Screen`,
             'Not Effective' = 1 - `Effective`,
             '1 - Sensitivity' = 1 - `Sensitivity`,
             '1 - Specificity' = 1 - `Specificity`,
             'scenario' = seq_len(nrow(.)))

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


filename <- "data/scenario-parameter-values_fullfactorial.xlsx"

write.xlsx(parameter_p, sheetName = "p",
           file = filename, row.names = FALSE)

write.xlsx(parameter_cost, sheetName = "cost",
           file = filename, row.names = FALSE, append = TRUE)

