
#' combine_freq_tables
#'
#' @param parent_folder
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
#'
#' combine_freq_tables(parent_folder,
#'                     file_name = "all_subsets.csv")
#'
#' combine_freq_tables(parent_folder,
#'                     file_name = "prob_subset_dectree.csv")
#'
combine_freq_tables <- function(folder,
                                file_name) {

  # initiate
  flder <-
    list.files(data_folder, pattern = 'policy_[0-9]*$') %>%
    sort()

  tab <-
    read.csv(pastef(flder[1], file_name)) %>%
    select(-X)

  for (i in seq_along(flder)[-1]) {

    tab <-
      read.csv(pastef(flder[i], file_name)) %>%
      select(-X) %>%
      merge(tab, .,
            by = c("scenario", "variable"),
            suffixes = c("", paste("_", i, sep = "")))
  }

  tab <-
    tab %>%
    mutate(scenario = as.numeric(scenario)) %>%
    arrange(scenario)

  write.csv(tab, file = paste0(folder, "/combined_", file_name))
}





# double header Excel file
##TODO: specify column names

# XLConnect::writeWorksheetToFile(file = paste0(parent_folder, "/combined_all_subsets.xlsx"),
#                                 data = header,
#                                 sheet = "test",
#                                 startRow = 1,
#                                 header = FALSE)
#
# XLConnect::writeWorksheetToFile(file = paste0(parent_folder, "/combined_all_subsets.xlsx"),
#                                 data = example,
#                                 sheet = "test",
#                                 startRow = 2)



