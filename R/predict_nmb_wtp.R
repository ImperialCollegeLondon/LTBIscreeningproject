
#' Predict from a list of fitted regressions
#'
#' @param fits_list
#' @param newdata
#'
#' @return
#' @export
#'
#' @examples
#'
predict_nmb_wtp <- function(fits_list,
                            newdata = NA) {

  if (any(is.na(newdata))) {
    newdata <- read.csv(here::here("data", "predict_newdata.csv"),
                        stringsAsFactors = TRUE)
  }

  newdata <- rm_redundant_covariates(fits_list, newdata)

  pred_wtp <- map(fits_list,
                  predict,
                  newdata = newdata,
                  type = "response")

  pred_INMB <- map(pred_wtp,
                   make_wide_INMB,
                   newdata = newdata)

  return(pred_INMB)
}


#
rm_redundant_covariates <- function(fits_list,
                                    newdata) {

  lm_names <- get_covariate_names(fits_list[[1]])
  newdata <- newdata[names(newdata) %in% lm_names]
  newdata[!duplicated(newdata), ]
}


#
get_covariate_names <- function(fit)
  dimnames(attr(fit$terms, "factors"))[[1]][-1]


#' make wide INMB array from predictions
#'
#' reshape
#'
#' @param pred
#' @param newdata
#'
#' @return
#' @export
#'
#' @examples
#'
make_wide_INMB <- function(pred,
                           newdata) {

  newdata %>%
    cbind(pred = pred) %>%
    tidyr::spread(type, pred) %>%
    mutate(INMB = screened - statusquo,
           CE = INMB > 0)
}


#' Net monetary benefit regression predictions
#'
#' High-level create desing matrix, fit model & predict.
#'
#' @param ce_res from \code{combine_popmod_dectree_res()}
#' @param folders
#' @param use_newdata default TRUE
#'
#' @return Array of regression predictions.
#' @export
#'
nmb_predictions <- function(ce_res,
                            folders,
                            use_newdata = TRUE) {

  nmb_mat <- nmb_matrix(ce_res$ce1, ce_res$ce0, folders)

  fits_list <- nmb_multi_regn(nmb_mat, folders)

  if (use_newdata) newdata <- NA
  else newdata <- nmb_mat[[1]][nmb_mat[[1]]$runs == 1, ]

  pred_INMB <- predict_nmb_wtp(fits_list, newdata)

  return(pred_INMB)
}

