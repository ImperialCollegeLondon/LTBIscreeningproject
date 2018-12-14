
#' Create prediction input data
#'
#' This can be a super set of values because the prediction
#' function just picks the ones that are in the fitted model.
#' Although for a fine grid on some parameter this may
#' result in a very large array.
#'
#' @param grid_min,grid_max upper and lower limits
#' @param step_size equally spaced intervals
#' @param agree,start,complete,effective,cost parameter names
#'
#' @return
#' @export
#'
#' @examples
#' create_pred_newdata(sens = 0.9,
#'                   spec = 0.85,
#'                   #start = c(0.5, 1),
#'                   #complete = c(0.5, 1),
#'                   cost = 50)
#'
create_pred_newdata <- function(grid_min = NA,
                                grid_max = NA,
                                step_size = NA,
                                agree = NA,
                                sens = NA,
                                spec = NA,
                                start = NA,
                                complete = NA,
                                effective = NA,
                                cost = NA) {

  p_grid_vals <-
    if (!is.na(grid_min) && !is.na(grid_max)) {

      seq(grid_min, grid_max, by = step_size)
    } else {
      NA
    }

  newdata <-
    expand.grid(
      "Agree_to_Screen_p" = if (anyNA(agree)) p_grid_vals else agree,
      "Sensitivity_p" = if (anyNA(sens)) p_grid_vals else sens,
      "Specificity_p" = if (anyNA(spec)) p_grid_vals else spec,
      "Start_Treatment_p" = if (anyNA(start)) p_grid_vals else start,
      "Complete_Treatment_p" = if (anyNA(complete)) p_grid_vals else complete,
      "Effective_p" = if (anyNA(effective)) p_grid_vals else effective,
      "Agree_to_Screen_cost" = cost,
      "type" = c("screened", "statusquo"))

  # remove empty columns
  newdata <- newdata[ ,complete.cases(t(newdata))]

  write.csv(newdata, here::here("data", "predict_newdata.csv"))

  invisible(newdata)
}
