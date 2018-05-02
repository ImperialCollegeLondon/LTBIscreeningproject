
#' num_subset_tb_wide
#'
#' Uses dectree subset_pop output
#' instead of separate montecarlo()
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
num_subset_tb_wide <- function(n.all_tb,
                               n.exit_tb,
                               dectree_res,
                               diroutput) {
  num_subset_tb <-
    dectree_res %>%
    map("subset_pop") %>%
    map(as.tibble) %>%
    map(~select(.x, "p_LTBI_to_cured")) %>%
    map(reshape2::melt) %>%
    plyr::ldply(data.frame,
                .id = "scenario") %>%
    group_by(scenario) %>%
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


#' num_subset_tb
#'
#' Uses dectree subset_pop output
#' instead of separate montecarlo()
#'
#' @param n.all_tb Number of TB cases in total
#' @param n.exit_tb Number of TB cases occuring in EWNI only
#' @param dectree_res
#' @param diroutput folder name text string
#'
#' @return
#' @export
#'
#' @examples
#'
num_subset_tb  <- function(n.all_tb,
                           n.exit_tb,
                           dectree_res,
                           diroutput = NA) {

  p_subset_tb <-
    dectree_res %>%
    map("subset_pop") %>%
    map(as.tibble) %>%
    map(~select(.x, "p_LTBI_to_cured")) %>%
    map(reshape2::melt) %>%
    plyr::ldply(data.frame,
                .id = "scenario") %>%
    group_by(scenario) %>%
    summarise(L95 = quantile(value, 0.05),
              mean = mean(value),
              U95 = quantile(value, 0.95))

  tb_uk <-
    p_subset_tb %>%
    mutate_at(vars(-scenario), `*`, n.exit_tb) %>%
    mutate(X2 = "tb_avoid_uk")

  tb_all <-
    p_subset_tb %>%
    mutate_at(vars(-scenario), `*`, n.all_tb) %>%
    mutate(X2 = "tb_avoid_all")

  num_subset_tb <-
    rbind(tb_all, tb_uk) %>%
    select(scenario, X2, everything())

  if (!is.na(diroutput)) {
    write.csv(num_subset_tb,
              file = pastef(diroutput, "num_subset_tb.csv"))
  }

  invisible(num_subset_tb)
}
