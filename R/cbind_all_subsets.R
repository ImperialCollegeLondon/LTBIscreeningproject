
#' cbind_all_subsets
#'
#' @param diroutput
#'
#' @return
#' @export
#'
#' @examples
#'
cbind_all_subsets <- function(diroutput = NA) {

  # tb_avoid <- read.csv(file = paste(diroutput, "tb_avoided.csv", sep = "/"))
  tb_avoid <- read.csv(file = paste(diroutput, "num_subset_tb.csv", sep = "/"))
  num_subset_dectree <- read.csv(file = paste(diroutput, "num_subset_dectree.csv", sep = "/"))

  all_subset <-
    dplyr::bind_rows(tb_avoid,
          num_subset_dectree) %>%
    arrange(scenario) %>%
    select(-X)

  if (!is.na(diroutput)) {
    write.csv(all_subset, file = paste(diroutput, "all_subsets.csv", sep = "/"))
  }

  invisible(all_subset)
}
