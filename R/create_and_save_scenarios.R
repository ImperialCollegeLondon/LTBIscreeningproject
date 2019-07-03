
#' Create and save scenarios
#'
#' Read in an Excel workbook consisting of a cost and probability (p) sheet.
#' This is converted to a list of dataframes for model inputs
#' and a long flat array to easily inspect by eye.
#'
#' @param file_tag Trailing part of the Excel file name to identify specific sets of scenarios.
#'
#' @return empty (save to project data folder)
#' @export
#'
#' @examples
#'
create_and_save_scenarios <- function(file_tag) {

  params_file <- here("data", sprintf("scenario-parameter-values%s.xlsx", file_tag))
  tab_names <- readxl::excel_sheets(params_file)

  params <- list()

  for (i in tab_names) {

    params[[i]] <- readxl::read_excel(params_file,
                                      sheet = i, na = c("", NA))#,
    # col_types = c('text', 'numeric', 'numeric', 'text', 'numeric'))

    if ("node" %in% names(params_p)) {

      params[[i]]$val_type <- i
    }
  }

  scenario_params_df <-
    combine_cost_and_p_xlsheets(params_p,
                                params_cost)

  scenario_params_df <- drop_all_na_rows(scenario_params_df)

  scenario_params <- split(x = scenario_params_df,
                           f = scenario_params_df$scenario)

  write.csv(scenario_params_df, file = here("data", "scenario_params_df.csv"))
  save(scenario_params, file = here("data", "scenario_params.RData"))
}
