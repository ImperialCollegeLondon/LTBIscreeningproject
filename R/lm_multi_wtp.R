#' lm_multi_wtp
#'
#' @param nmb_formula
#' @param sim_matrix
#'
#' @return
#' @export
#'
#' @examples
lm_multi_wtp <- function(nmb_formula,
                         sim_matrix) {

  # expanded formula
  # terms = attr(terms.formula(nmb_formula), "term.labels")
  # f = as.formula(sprintf("y ~ %s", paste(terms, collapse="+")))

  # fit model

  # lm_multi_wtp <- lm_wtp(nmb_formula, sim_matrix)
  lm_multi_wtp <- bayeslm_wtp(nmb_formula, sim_matrix)

  lm_multi <-
    lapply(wtp_seq, lm_multi_wtp) %>%
    purrr::set_names(wtp_seq)

  # create wide output table
  lm_multi_all <-
    lapply(lm_multi,
           function(x) dplyr::select(tidy(x), -statistic)) %>%
    plyr::join_all(by = "term") %>%
    rbind(c("wtp", rep(wtp_seq, each = 3)))

  # format values
  lm_multi_all[ ,-1] <- round(sapply(lm_multi_all[ ,-1], as.numeric), 4)

  return(lm_multi_all)
}
