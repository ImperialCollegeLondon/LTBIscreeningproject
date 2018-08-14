
#' nmb_multi_regn
#'
#' @param nmb_mat list by wtp
#' @param folders
#' @param f_lm default: lm
#' @param interactions
#' @param centre default: 90
#'
#' @return
#' @export
#'
#' @examples
#'
nmb_multi_regn <- function(nmb_mat,
                          folders = NA,
                          f_lm = lm,
                          interactions = NA,
                          centre = 90) {

  if (is.na(interactions)) {
    interactions <-
      paste0("(((I(Agree - ", centre,
             ") + I(Start - ", centre,
             ") + I(Complete - ", centre,
             ") + I(Effective - ", centre, "))^2)")
  }

  nmb_formula <- as.formula(paste("NMB ~ type *", interactions))

  lm_multi_wtp <- lm_multi_wtp(nmb_formula, nmb_mat, f_lm)

  return(lm_multi_wtp)
}
