
#' plots_and_tables_policies
#'
#' @return
#' @export
#'
#' @examples
#'
plots_and_tables_policies <- function() {

  ##TODO: dont want this hardcoded
  data_folder <- here::here("ext-data/18_to_35_in_2009")
  plots_folder <- here::here("output/plots")

  source("scripts/tables--combine_costeffectiveness_tables.R", local = TRUE)

  combine_freq_tables(data_folder, file_name = "all_subsets.csv")
  combine_freq_tables(data_folder, file_name = "prob_subset_dectree.csv")

  try(source("scripts/e_and_c_totals_by_scenario.R", local = TRUE))

  try(source("scripts/tables--wide_combined_ce.R", local = TRUE))

  # try(source("scripts/CE_plane_by_scenario.R", local = TRUE))

  care_cascade_num(data_folder, plots_folder)
  care_cascade_prob(data_folder, plots_folder)

  return()
}
