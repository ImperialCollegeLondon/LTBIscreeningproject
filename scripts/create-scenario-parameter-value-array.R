#
# project: LTBI screening
# N Green
# Nov 2016
#


library(xlsx)


scenario_parameter_p <- expand.grid('Start Treatment' = seq(0, 1, by = 0.05),
                                    'Complete Treatment' = seq(0, 1, by = 0.05))

scenario_parameter_p <- data.frame(scenario_parameter_p,
                                   'Not Start Treatment' = 1-scenario_parameter_p$`Start Treatment`,
                                   'Not Complete Treatment' = 1-scenario_parameter_p$`Complete Treatment`,
                                   'Agree to Screen' = 0.9,
                                   'Not Agree to Screen' = 0.1,
                                   'scenario' = seq_len(nrow(scenario_parameter_p)))

scenario_parameter_cost <- data.frame('Agree to Screen' = 50,
                                      'scenario' = seq_len(nrow(scenario_parameter_p)))

names(scenario_parameter_cost) <- gsub("\\.", " ", names(scenario_parameter_cost))
names(scenario_parameter_p) <- gsub("\\.", " ", names(scenario_parameter_p))


write.xlsx(scenario_parameter_p, sheetName = "p",
           file = "data/scenario-parameter-values_adherence_completion.xlsx", row.names = FALSE)

write.xlsx(scenario_parameter_cost, sheetName = "cost",
           file = "data/scenario-parameter-values_adherence_completion.xlsx", row.names = FALSE, append = TRUE)

