
#' Linear multivariate regression varying willingness to pay
#'
#' @param nmb_formula
#' @param nmb_mat list by wtp
#' @param f_lm function type of regression; bayeslm_wtp, lm_wtp
#' @param wtp_seq
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
lm_multi_wtp <- function(nmb_formula,
                         nmb_mat,
                         # f_lm = c(bayeslm_wtp, lm_wtp),
                         f_lm = c(bayesglm, lm),
                         wtp_seq = seq(10000, 30000, by = 10000),
                         folders = NA) {

  ##TODO: closure error?
  lm_multi_fit <- map(nmb_mat, lm, formula = nmb_formula)
  # lm_multi_fit <- map(nmb_mat, f_lm, formula = nmb_formula)

  if (!is.na(folders)) {

    lm_multi_all <- lm_list_to_df(lm_multi_fit)

    # format values
    lm_multi_all[ ,-1] <- round(sapply(lm_multi_all[ ,-1], as.numeric), 4)

    # names_keep <- lm_multi_all[lm_multi_all$term == "wtp", ] %in% c("wtp", 10000, 20000, 30000)
    # lm_multi_save <- lm_multi_all[ ,names_keep]

    write.csv(lm_multi_all,
              file = pastef(folders$output$scenario, "lm_multi_all_table.csv"))
  }

  return(lm_multi_fit)
}


#' lm_list_to_df
#'
#' Create wide output table using broom.
#'
#' @param fit
#'
#' @return
#' @export
#'
#' @examples
lm_list_to_df <- function(fit) {

  lapply(fit,
         function(x) dplyr::select(tidy(x), -statistic)) %>%
  plyr::join_all(by = "term") %>%
  rbind(c("wtp", rep(names(fit), each = 3)))
}
