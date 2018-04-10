#******************************************************
# LTBI screening model
# N Green
# Sept 2017
#
# prep scenario decision tree cost & QALY data


# deterministic sensitivity analysis --------------------------------------
# grid of input parameter values

# scenario_file_tag <- "_high-low"
# scenario_file_tag <- "_main" #paste0("_", study) #_oneway
# scenario_file_tag <- "_fullfactorial_QFT-GIT"
scenario_file_tag <- "_fullfactorial_QFT-GIT_3mo_RIFINH"
# scenario_file_tag <- "_fullfactorial_QFT-GIT_50testcost"
# scenario_file_tag <- "_fullfactorial_QFT-GIT_100testcost"
# scenario_file_tag <- "_fullfactorial_QFT-plus"
# scenario_file_tag <- "_fullfactorial_TSPOT"


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

scenario_parameters <- dplyr::bind_rows(scenario_parameter_cost,
                                        scenario_parameter_p.melt)

# split by scenario to lists
scenario_parameters <- split(x = scenario_parameters,
                             f = scenario_parameters$scenario)


save(scenario_parameters, file = "data/scenario_parameters.RData")
