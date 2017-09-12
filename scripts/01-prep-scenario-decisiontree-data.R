#******************************************************
# LTBI screening model
# N Green
# Sept 2017
#
# prep scenario decision tree cost & QALY data


# load data ---------------------------------------------------------------

parameter_values_file <- system.file("data", sprintf("scenario-parameter-values%s.xlsx", scenario_file_tag),
                                     package = "LTBIscreeningproject")

scenario_parameter_cost <- readxl::read_excel(parameter_values_file,
                                              sheet = "cost")

scenario_parameter_p <- readxl::read_excel(parameter_values_file,
                                           sheet = "p")

# transform to long format

scenario_parameter_p.melt <-
  as.data.frame(scenario_parameter_p) %>%
  reshape2::melt(id.vars = "scenario") %>%
  plyr::rename(replace = c("variable" = "node",
                           "value" = "p"))

# combine probs and costs in to a single array

scenario_parameter_cost$val_type <- "cost"
scenario_parameter_p.melt$val_type <- "QALYloss"

scenario_parameters <- plyr::rbind.fill(scenario_parameter_cost,
                                        scenario_parameter_p.melt)

# split by scenario to lists
require(plyr)
scenario_parameters <- plyr::dlply(scenario_parameters, .(scenario))

save(scenario_parameters, file = "data/scenario_parameters.RData")
