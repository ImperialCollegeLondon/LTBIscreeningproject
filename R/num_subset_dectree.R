
#' num_subset_dectree
#'
#' Counts or proportion frequency of subset sizes along
#' screening pathway.
#'
#' @param cohort individual level data
#' @param dectree_res output of decision_tree_cluster()
#' @param num_screen
#'
#' @return tibble
#' @export
#'
#' @examples
#'
subset_dectree <- function(cohort,
                           dectree_res,
                           num_screen = 1) {
  num_subset <-
    dectree_res %>%
    map("subset_pop") %>%
    map(reshape2::melt) %>%
    plyr::ldply(data.frame,
                .id = "scenario") %>%
    group_by(scenario, X2) %>%
    summarise(L95 = quantile(value, 0.025) * num_screen,
              L50 = quantile(value, 0.25) * num_screen,
              mean = mean(value) * num_screen,
              U50 = quantile(value, 0.75) * num_screen,
              U95 = quantile(value, 0.975) * num_screen)

  invisible(num_subset)
}

#' prob_subset_dectree
#'
#' @param cohort
#' @param dectree_res
#' @param diroutput
#'
#' @return
#' @export
#'
#' @examples
prob_subset_dectree <- function(cohort,
                                dectree_res,
                                diroutput = NA) {

  out <- subset_dectree(cohort,
                        dectree_res,
                        num_screen = 1)

  if (!is.na(diroutput)) {
    write.csv(out,
              file = pastef(diroutput, "prob_subset_dectree.csv"))
  }

  invisible(out)
}

#' num_subset_dectree
#'
#' @param cohort
#' @param dectree_res
#' @param diroutput
#'
#' @return
#' @export
#'
#' @examples
num_subset_dectree <- function(cohort,
                               dectree_res,
                               diroutput = NA) {

  out <- subset_dectree(cohort,
                        dectree_res,
                        num_screen = nrow(cohort))

  if (!is.na(diroutput)) {
    write.csv(out,
              file = pastef(diroutput, "num_subset_dectree.csv"))
  }

  invisible(out)
}



