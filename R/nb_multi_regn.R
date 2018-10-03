
#' nmb_multi_regn
#'
#' Fit regression
#'
#' @param nmb_mat list by wtp
#' @param folders
#' @param f_lm lm or bayeslm_wtp; default: lm
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
                           centre_p = 90) {

  if (!all(is.na(folders))) {

    design_mat <-
      pastef(folders$output$parent,
             "scenario_params_df.csv") %>%
      read.csv() %>%
      design_matrix() %>%
      remove_cols_constant_vars()

    vars <- string_sum_covariates(design_mat)
  }

  if (is.na(interactions)) {
    ##TODO: as.formula returns an error
    # interactions <-
    #   paste0("((I(Agree_to_Screen_p - ", centre_p,
    #          ") + I(Start_Treatment_p - ", centre_p,
    #          ") + I(Complete_Treatment_p - ", centre_p,
    #          # ") + I(Effective - ", centre_p,
    #          "))^2")

    # pairs
    interactions <-
      paste0("(", vars, ")^2")
  }

  nmb_formula <- as.formula(paste("NMB ~ type *", interactions))

  fit_multi_wtp <- lm_multi_wtp(nmb_formula, nmb_mat, f_lm, folders)

  return(fit_multi_wtp)
}

#
string_sum_covariates <- function(design_mat) {

  names(design_mat)[names(design_mat) != "scenario"] %>%
    paste(collapse = " + ")
}
