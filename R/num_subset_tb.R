
#' num_subset_tb
#'
#' TODO: this uses dectree subset_pop output
#'
#' @param n.all_tb
#' @param n.exit_tb
#' @param dectree_res
#' @param diroutput folder text string
#'
#' @return tibble
#' @export
#'
#' @examples
#'
num_subset_tb <- function(n.all_tb,
                          n.exit_tb,
                          dectree_res,
                          diroutput) {
  num_subset_tb <-
    dectree_res %>%
    map("subset_pop") %>%
    map(select(p_LTBI_to_cured)) %>%
    map(reshape2::melt) %>%
    plyr::ldply(data.frame,
                .id = "scenario") %>%
    group_by(scenario, X2) %>%
    summarise(L95_EWNI = quantile(value, 0.05) * n.exit_tb,
              mean_EWNI = mean(value) * n.exit_tb,
              U95_EWNI = quantile(value, 0.95) * n.exit_tb,
              L95_all = quantile(value, 0.05) * n.all_tb,
              mean_all = mean(value) * n.all_tb,
              U95_all = quantile(value, 0.95) * n.all_tb)

  write.csv(num_subset_tb,
            file = pastef(diroutput, "num_subset_tb.csv"))

  invisible(num_subset_tb)
}
