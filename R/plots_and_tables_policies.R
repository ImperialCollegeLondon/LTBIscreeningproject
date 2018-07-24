
#' plots_and_tables_policies
#'
#' @return
#' @export
#'
#' @examples
#'
plots_and_tables_policies <- function() {

  ##TODO: dont want this hardcoded
  data_folder <- "C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/ext-data/18_to_35_in_2009"
  plots_folder <- "C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/output/plots"

  source("scripts/tables--combine_costeffectiveness_tables.R", local = TRUE)

  combine_freq_tables(data_folder, file_name = "all_subsets.csv")
  combine_freq_tables(data_folder, file_name = "prob_subset_dectree.csv")

  try(source("scripts/e_and_c_totals_by_scenario.R", local = TRUE))

  try(source("scripts/tables--wide_combined_ce.R", local = TRUE))

  # try(source("scripts/CE_plane_by_scenario.R", local = TRUE))

  plot_care_cascade(data_folder, plots_folder, prob_or_num = "num")
  plot_care_cascade(data_folder, plots_folder, prob_or_num = "prob")

  return()
}
