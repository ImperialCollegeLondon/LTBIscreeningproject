
#' nmb_multi_regn
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

  if (!is.na(folders)) {

    design_mat <-
      pastef(folders$output$scenario,
             "scenario_params_df.csv") %>%
      read.csv() %>%
      design_matrix()

    vars <-
      names(design_mat)[names(design_mat) != "scenario"] %>%
      paste(collapse = " + ")
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

  lm_multi_wtp <- lm_multi_wtp(nmb_formula, nmb_mat, f_lm)

  return(lm_multi_wtp)
}
