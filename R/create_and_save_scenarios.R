
#' create_and_save_scenarios
#'
#' @param file_tag
#'
#' @return
#' @export
#'
#' @examples
#'
create_and_save_scenarios <- function(file_tag) {

  parameter_values_file <- system.file("data", sprintf("scenario-parameter-values%s.xlsx", file_tag),
                                       package = "LTBIscreeningproject")

  scenario_parameter_cost <- readxl::read_excel(parameter_values_file,
                                                sheet = "cost", na = c("", NA))#,
  # col_types = c('text', 'numeric', 'numeric', 'text', 'numeric'))

  scenario_parameter_p <- readxl::read_excel(parameter_values_file,
                                             sheet = "p", na = c("", NA))#,
  # col_types = c('numeric', 'numeric', 'numeric', 'numeric'))

  scenario_parameter_cost$val_type <- "cost"

  scenario_parameters_df <-
    combine_cost_and_p_xlsheets(scenario_parameter_p,
                                scenario_parameter_cost)

  # split by scenario to lists
  scenario_parameters <- split(x = scenario_parameters_df,
                               f = scenario_parameters_df$scenario)

  write.csv(scenario_parameters_df, file = here::here("data/scenario_parameters_df.csv"))
  save(scenario_parameters, file = here::here("data/scenario_parameters.RData"))
}
