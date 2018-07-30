
#' ---
#' title: "TBI screening model: prep scenario decision tree cost & QALY data"
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


# deterministic sensitivity analysis --------------------------------------
# grid of policy input parameter values

# file_tag <- "_high-low"
# file_tag <- "_baseline"
file_tag <- "_effective"
# file_tag <- "_test"
# file_tag <- "_oneway"
# file_tag <- "_fullfactorial"


# load data ---------------------------------------------------------------

parameter_values_file <- system.file("data", sprintf("scenario-parameter-values%s.xlsx", file_tag),
                                     package = "LTBIscreeningproject")

scenario_parameter_cost <- readxl::read_excel(parameter_values_file,
                                              sheet = "cost", na = c("", NA))#,
                                              # col_types = c('text', 'numeric', 'numeric', 'text', 'numeric'))

scenario_parameter_p <- readxl::read_excel(parameter_values_file,
                                           sheet = "p", na = c("", NA))#,
                                           # col_types = c('numeric', 'numeric', 'numeric', 'numeric'))

scenario_parameter_cost$val_type <- "cost"

if (nrow(scenario_parameter_p) > 0) {

  # transform to long format
  scenario_parameters_df <-
    as.data.frame(scenario_parameter_p) %>%
    reshape2::melt(id.vars = "scenario") %>%
    plyr::rename(replace = c("variable" = "node",
                             "value" = "p")) %>%
    mutate(val_type = "p") %>%
    dplyr::bind_rows(scenario_parameter_cost, .)

}else{
  scenario_parameters_df <- scenario_parameter_cost
}

# split by scenario to lists
scenario_parameters <- split(x = scenario_parameters_df,
                             f = scenario_parameters_df$scenario)

write.csv(scenario_parameters_df, file = "data/scenario_parameters_df.csv")
save(scenario_parameters, file = "data/scenario_parameters.RData")


# convert to percentages and simplify names
design_matrix <-
  scenario_parameter_p %>%
  mutate_at(vars(-scenario),
            funs(.*100)) %>%
  set_names(~sub(' Treatment', '', .x)) %>%
  set_names(~sub(' to Screen', '', .x))

save(design_matrix, file = "data/design_matrix.RData")

