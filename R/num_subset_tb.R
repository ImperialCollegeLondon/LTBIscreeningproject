
#' num_subset_tb_wide
#'
#' Uses dectree subset_pop output
#' instead of separate montecarlo()
#'
#' @param cohort
#' @param dectree_res
#' @param folder text string
#'
#' @return tibble
#' @export
#'
#' @examples
#'
num_subset_tb_wide <- function(cohort,
                               dectree_res,
                               folder) {

  n.exit_tb <- sum(cohort$exituk_tb)
  n.all_tb <- sum(cohort$all_tb)

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
            file = pastef(folder, "num_subset_tb.csv"))

  invisible(num_subset_tb)
}


#' num_subset_tb
#'
#' Uses dectree subset_pop output
#' instead of separate montecarlo()
#'
#' @param cohort
#' @param dectree_res
#' @param folder name text string
#'
#' @return
#' @export
#'
#' @examples
#'
num_subset_tb  <- function(cohort,
                           dectree_res,
                           folder = NA) {

  n.exit_tb <- sum(cohort$exituk_tb)
  n.all_tb <- sum(cohort$all_tb)

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

  if (!is.na(folder)) {
    write.csv(num_subset_tb,
              file = pastef(folder, "num_subset_tb.csv"))
  }

  invisible(num_subset_tb)
}
