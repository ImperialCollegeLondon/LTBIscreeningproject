
#' cbind_all_subsets
#'
#' @param read_folder text string
#' @param write_folder text string
#'
#' @return
#' @export
#'
#' @examples
#'
cbind_all_subsets <- function(read_folder,
                              write_folder = read_folder) {

  # tb_avoid <- read.csv(file = paste(folder, "tb_avoided.csv", sep = "/"))
  tb_avoid <- read.csv(file = paste(read_folder, "num_subset_tb.csv", sep = "/"))
  num_subset_dectree <- read.csv(file = paste(read_folder, "num_subset_dectree.csv", sep = "/"))

  all_subset <-
    dplyr::bind_rows(tb_avoid,
                     num_subset_dectree) %>%
    arrange(scenario) %>%
    select(-X)

  write.csv(all_subset, file = paste(write_folder, "all_subsets.csv", sep = "/"))

  invisible(all_subset)
}
