
#' table_tb_avoided
#'
#' @param dectree_res
#' @param folder text string
#'
#' @return EWNI and total 5%, 50% and 95% quantiles
#' @export
#'
#' @examples
#'
table_tb_avoided <- function(dectree_res,
                             folder = NA) {

  tb_all <-
    dectree_res %>%
    map("n_tb_screen_all") %>%
    plyr::ldply(data.frame,
                .id = "scenario") %>%
    group_by(scenario, status) %>%
    summarise(L95 = quantile(n, 0.05),
              mean = mean(n),
              U95 = quantile(n, 0.95)) %>%
    dplyr::filter(status == "disease-free") %>%
    mutate(X2 = "tb_all") %>%
    select(-status) %>%
    select(scenario, X2, everything())

  tb_uk <-
    dectree_res %>%
    map("n_tb_screen_uk") %>%
    plyr::ldply(data.frame,
                .id = "scenario") %>%
    group_by(scenario, status) %>%
    summarise(L95 = quantile(n, 0.05),
              mean = mean(n),
              U95 = quantile(n, 0.95)) %>%
    dplyr::filter(status == "disease-free") %>%
    mutate(X2 = "tb_uk") %>%
    select(-status) %>%
    select(scenario, X2, everything())

  out <- rbind(tb_all, tb_uk)

  if (!is.na(folder)) {
    write.csv(x = out,
              file = paste(folder, "tb_avoided.csv", sep = "/"))
  }

  invisible(out)
}
