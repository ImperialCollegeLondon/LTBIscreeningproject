
#' Create and save scenarios
#'
#' @param file_tag Trailing part of the Excel file name
#'
#' @return none (save to project data folder)
#' @export
#'
#' @examples
#'
create_and_save_scenarios <- function(file_tag) {

  params_file <- here("data", sprintf("scenario-parameter-values%s.xlsx", file_tag))

  params_cost <- readxl::read_excel(params_file,
                                    sheet = "cost", na = c("", NA))#,
  # col_types = c('text', 'numeric', 'numeric', 'text', 'numeric'))

  params_p <- readxl::read_excel(params_file,
                                 sheet = "p", na = c("", NA))#,
  # col_types = c('numeric', 'numeric', 'numeric', 'numeric'))

  params_cost$val_type <- "cost"

  scenario_params_df <-
    combine_cost_and_p_xlsheets(params_p,
                                params_cost)

  scenario_params_df <- drop_all_na_rows(scenario_params_df)

  scenario_params <- split(x = scenario_params_df,
                           f = scenario_params_df$scenario)

  write.csv(scenario_params_df, file = here("data", "scenario_params_df.csv"))
  save(scenario_params, file = here("data", "scenario_params.RData"))
}


#
drop_all_na_rows <- function(df) {

  value_cols <-
    df %>% dplyr::select(-node,
                         -val_type,
                         -scenario)

  keep_rows <- apply(value_cols, 1,
                     function(x) !all(is.na(x)))
  df[keep_rows, ]
}
