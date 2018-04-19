#******************************************************
# LTBI screening model
# N Green
# Sept 2017
#
# prep scenario decision tree cost & QALY data


# deterministic sensitivity analysis --------------------------------------
# grid of policy input parameter values

# file_tag <- "_high-low"
file_tag <- "_baseline"
# file_tag <- "_oneway"
# file_tag <- "_fullfactorial"


# load data ---------------------------------------------------------------

parameter_values_file <- system.file("data", sprintf("scenario-parameter-values%s.xlsx", file_tag),
                                     package = "LTBIscreeningproject")

scenario_parameter_cost <- readxl::read_excel(parameter_values_file,
                                              sheet = "cost", na = c("", NA),
                                              col_types = c('text', 'numeric', 'numeric', 'text', 'numeric'))

scenario_parameter_p <- readxl::read_excel(parameter_values_file,
                                           sheet = "p", na = c("", NA),
                                           col_types = c('numeric', 'numeric', 'numeric', 'numeric'))

scenario_parameter_cost$val_type <- "cost"

if (nrow(scenario_parameter_p) > 0) {

  # transform to long format
  scenario_parameters <-
    as.data.frame(scenario_parameter_p) %>%
    reshape2::melt(id.vars = "scenario") %>%
    plyr::rename(replace = c("variable" = "node",
                             "value" = "p")) %>%
    mutate(val_type = "QALYloss") %>%
    dplyr::bind_rows(scenario_parameter_cost, .)

}else{
  scenario_parameters <- scenario_parameter_cost
}

# split by scenario to lists
scenario_parameters <- split(x = scenario_parameters,
                             f = scenario_parameters$scenario)


save(scenario_parameters, file = "data/scenario_parameters.RData")


# convert to percentages and simplify names
design_matrix <-
  scenario_parameter_p %>%
  mutate_at(vars(-scenario),
            funs(.*100)) %>%
  set_names(~sub(' Treatment', '', .x)) %>%
  set_names(~sub(' to Screen', '', .x))

save(design_matrix, file = "data/design_matrix.RData")

