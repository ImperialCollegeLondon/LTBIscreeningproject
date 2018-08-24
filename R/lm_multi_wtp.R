
#' Linear multivariate regression varying willingness to pay
#'
#' @param nmb_formula
#' @param nmb_mat list by wtp
#' @param f_lm function type of regression; bayeslm_wtp, lm; default: lm
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
lm_multi_wtp <- function(nmb_formula,
                         nmb_mat,
                         f_lm = lm,
                         folders = NA) {

  lm_fit <- map(nmb_mat, bayesglm,
                formula = nmb_formula,
                family = gaussian,
                prior.mean = 0, prior.scale = 1000, prior.df = 1000)
  # lm_fit <- map(nmb_mat, f_lm, formula = nmb_formula)

  if (!is.na(folders)) {

      write.csv(lm_list_to_df(lm_fit),
                file = pastef(folders$output$scenario, "lm_multi_all_table.csv"))
  }

  return(lm_fit)
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

  out <-
    lapply(fit,
         function(x) dplyr::select(tidy(x), -statistic)) %>%
  plyr::join_all(by = "term") %>%
  rbind(c("wtp", rep(names(fit), each = 3)))

  # format values
  out[ ,-1] <- round(sapply(out[ ,-1], as.numeric), 4)

  # names_keep <- out[out$term == "wtp", ] %in% c("wtp", 10000, 20000, 30000)
  # out <- out[ ,names_keep]

  return(out)
}
