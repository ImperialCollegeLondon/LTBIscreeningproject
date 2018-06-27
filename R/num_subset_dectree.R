
#' num_subset_dectree
#'
#' Counts or proportion frequency of subset sizes along
#' screening pathway.
#'
#' @param cohort individual level data
#' @param subset_pop part of output of decision_tree_cluster()
#' @param num_screen
#'
#' @return tibble
#' @export
#'
#' @examples
#'
subset_dectree <- function(cohort,
                           subset_pop,
                           num_screen = 1) {
  num_subset <-
    subset_pop %>%
    map(reshape2::melt) %>%
    plyr::ldply(data.frame,
                .id = "scenario") %>%
    group_by(scenario, variable) %>%
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
  subset_pop_tot <-
    dectree_res %>%
    map("subset_pop") %>%
    map(as.data.frame)

  out <- subset_dectree(cohort,
                        subset_pop_tot,
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

  subset_pop_tot <-
    dectree_res %>%
    map("subset_pop") %>%
    map(as.data.frame)

  # separate marginal and conditional probs
  # different denominators
  subset_LTBI <- map(subset_pop_tot, `[`, c("LTBI_tests", "LTBI_positive", "LTBI_startTx", "LTBI_completeTx", "p_LTBI_to_cured"))
  subset_all <- map(subset_pop_tot, `[`, c("LTBI_pre", "tests", "positive", "startTx", "completeTx", "cured", "LTBI_post"))

  out_LTBI <- subset_dectree(cohort,
                             subset_pop = subset_LTBI,
                             num_screen = nrow(cohort)*subset_all[[1]]$LTBI_pre[1])
  ##TODO: currently assume that all scenarios have equal LTBI prevalence
  ## generalise to take each scenarios own LTBI prob
  # num_screen = map_dfc(subset_all, .f = head, 1))

  out_all <- subset_dectree(cohort,
                            subset_all,
                            num_screen = nrow(cohort))

  out <-
    rbind.data.frame(out_LTBI, out_all) %>%
    arrange(scenario, variable)

  if (!is.na(diroutput)) {
    write.csv(out,
              file = pastef(diroutput, "num_subset_dectree.csv"))
  }

  invisible(out)
}



