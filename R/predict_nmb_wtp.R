
#' predict_nmb_wtp
#'
#' @param lm_multi_wtp
#' @param newdata
#'
#' @return
#' @export
#'
#' @examples
predict_nmb_wtp <- function(lm_multi_wtp,
                            newdata = NA) {

  if (is.na(newdata)) {
    newdata <- read.csv(here::here("data", "predict_newdata.csv"))
  }

  pred_wtp <- map(lm_multi_wtp,
                  predict,
                  newdata = newdata,
                  type = "response")

  pred_INMB <- map(pred_wtp,
                   wide_INMB,
                   newdata = newdata)

  return(pred_INMB)
}


#' Wide INMB from predictions
#'
#' @param pred
#'
#' @return
#' @export
#'
#' @examples
wide_INMB <- function(pred,
                      newdata) {

  newdata %>%
    cbind(pred = pred) %>%
    tidyr::spread(type, pred) %>%
    mutate(INMB = screened - statusquo,
           CE = INMB > 0)
}


#' create_pred_newdata
#'
#' @return
#' @export
#'
#' @examples
#' create_pred_newdata()
#'
create_pred_newdata <- function() {

  grid_vals <- seq(5, 100, 5)

  newdata <-
    expand.grid("Agree_to_Screen_p" = grid_vals,
                "Start_Treatment_p" = grid_vals,
                "Start_Treatment_p" = grid_vals,
                "Effective_p" = grid_vals,
                "type" = c("screened", "statusquo"))

  write.csv(newdata, here::here("data", "predict_newdata.csv"))

  return()
}
