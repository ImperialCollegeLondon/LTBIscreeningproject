
#' num_subset_dectree
#'
#' Counts frequency of subset sizes along
#' screening pathway.
#'
#' @param cohort individual level data
#' @param dectree_res output of decision_tree_cluster()
#' @param folder text string
#' @param by_screen_year TRUE or FALSE ##TODO:
#' @param probs Return probabilities or numbers
#'
#' @return tibble
#' @export
#'
#' @examples
#'
num_subset_dectree <- function(cohort,
                               dectree_res,
                               folder = NA,
                               by_screen_year = FALSE,
                               probs = TRUE) {

  num_screen_year <-
    if (probs) {
      1
    } else {
      if (by_screen_year) {
        table(ceiling(cohort$screen_year))
      } else {
        nrow(cohort)}
    }

  ##TODO: update for each screen year
  # num_subset_list <-
  #   dectree_res %>%
  #   map("subset_pop") %>%
  #   lapply(function(x) cbind(total = 1, x)) %>%
  #   lapply(function(x) num_screen_year %o% t(x)) %>%
  #   map(round) %>%
  #   plyr::ldply(data.frame,
  #               .id = "scenario") %>%
  #   cbind(year = seq_along(num_screen_year), .)

  num_subset <-
    dectree_res %>%
    map("subset_pop") %>%
    map(reshape2::melt) %>%
    plyr::ldply(data.frame,
                .id = "scenario") %>%
    group_by(scenario, X2) %>%
    summarise(L95 = quantile(value, 0.05) * num_screen_year,
              mean = mean(value) * num_screen_year,
              U95 = quantile(value, 0.95) * num_screen_year) #%>%
    # dplyr::filter(X2 != 'p_LTBI_to_cured')

  if (!is.na(folder)) {
    write.csv(num_subset,
              file = pastef(folder, "num_subset_dectree.csv"))
  }

  invisible(num_subset)
}

