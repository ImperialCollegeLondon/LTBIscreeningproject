
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

  if (is.na(newdata)) {
    newdata <- read.csv(here::here("data", "predict_newdata.csv"),
                        stringsAsFactors = TRUE)
  }

  newdata <- rm_redundant_covariates(fits_list, newdata)

  pred_wtp <- map(fits_list,
                  predict,
                  newdata = newdata,
                  type = "response")

  pred_INMB <- map(pred_wtp,
                   wide_INMB,
                   newdata = newdata)

  return(pred_INMB)
}


#
rm_redundant_covariates <- function(fits_list,
                                    newdata) {

  lm_names <- dimnames(attr(fits_list[[1]]$terms, "factors"))[[1]]
  newdata <- newdata[names(newdata) %in% lm_names]
  newdata[!duplicated(newdata), ]
}


#' Wide INMB array from predictions
#'
#' reshape
#'
#' @param pred
#'
#' @return
#' @export
#'
#' @examples
#'
wide_INMB <- function(pred,
                      newdata) {

  newdata %>%
    cbind(pred = pred) %>%
    tidyr::spread(type, pred) %>%
    mutate(INMB = screened - statusquo,
           CE = INMB > 0)
}


#' Create prediction input data
#'
#' This can be a super set of values because the prediction
#' function just picks the ones that are in the fitted model.
#'
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#' create_pred_newdata()
#'
create_pred_newdata <- function() {

  # p_grid_vals <- seq(0.05, 1, by = 0.05)
  p_grid_vals <- seq(0.5, 1, by = 0.5)

  newdata <-
    expand.grid("Agree_to_Screen_p" = p_grid_vals,
                "Start_Treatment_p" = p_grid_vals,
                "Complete_Treatment_p" = p_grid_vals,
                "Effective_p" = p_grid_vals,
                "Agree_to_Screen_cost" = seq(50, 100, by = 50),
                "type" = c("screened", "statusquo"))

  write.csv(newdata, here::here("data", "predict_newdata.csv"))

  return()
}


#' Net monetary benefit regression predictions
#'
#' High-level
#'
#' @param dectree_res
#' @param popmod_res
#' @param folders
#'
#' @return
#' @export
#'
nmb_predictions <- function(dectree_res,
                            popmod_res,
                            folders,
                            cohort,
                            interv) {

  nmb_mat <- nmb_matrix_tb(dectree_res, popmod_res, folders, cohort, interv)

  fits_list <- nmb_multi_regn(nmb_mat, folders)

  pred_INMB <- predict_nmb_wtp(fits_list)

  return(pred_INMB)
}

