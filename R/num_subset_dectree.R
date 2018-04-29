
#' num_subset_dectree
#'
#' @param cohort
#' @param dectree_res
#' @param diroutput
#' @param by_screen_year
#'
#' @return
#' @export
#'
#' @examples
#'
num_subset_dectree <- function(cohort,
                               dectree_res,
                               diroutput,
                               by_screen_year = FALSE) {

  if (by_screen_year) {num_screen_year <- table(ceiling(cohort$screen_year))
  } else {num_screen_year <- nrow(cohort)}

  num_subset_list <-
    map(dectree_res, "subset_pop") %>%
    lapply(function(x) cbind(total = 1, x)) %>%
    lapply(function(x) num_screen_year %o% t(x)) %>%
    map(round)

  num_subset_dectree <-
    plyr::ldply(num_subset_list,
                data.frame,
                .id = "scenario") %>%
    cbind(year = seq_along(num_screen_year), .)

  write.csv(num_subset_dectree,
            file = pastef(diroutput, "num_subset_dectree.csv"))

  invisible(num_subset_dectree)
}
