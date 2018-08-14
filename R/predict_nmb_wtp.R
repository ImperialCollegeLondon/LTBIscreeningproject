
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

  pred_wtp <- map(lm_multi_wtp, predict, newdata = newdata, type = "response")

  pred_INMB <- map(pred_wtp, wide_INMB, newdata = newdata)

  return(pred_INMB)
}


#' Wide INMB from predictions
#'
#' @param pred_wtp
#'
#' @return
#' @export
#'
#' @examples
wide_INMB <- function(pred_wtp,
                      newdata) {

  newdata %>%
    cbind(pred = pred_wtp[[1]]) %>%
    tidyr::spread(type, pred) %>%
    mutate(INMB = screened - statusquo,
           CE = INMB > 0)
}


##TODO:...
create_pred_newdata <- function() {

  # pred_grid_values <- seq(50, 100, 10)
  # plot_grid_values <- seq(50, 100, 10)

  pred_grid_values <- seq(5, 100, 5) #for regression
  plot_grid_values <- c(50, 100)      #for grid of plots

  ## full-factorial grid
  newdata <-
    expand.grid("Agree" = pred_grid_values,
                "Start" = pred_grid_values,
                "Complete" = pred_grid_values,
                "Effective" = pred_grid_values,
                "policy" = c("screened", "statusquo"))

  write.csv(newdata, here::here("data", "predict_newdata.csv"))
}
