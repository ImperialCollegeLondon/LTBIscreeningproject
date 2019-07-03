
#' combine_cost_and_p_xlsheets
#'
#' @param parameter_p point value or distribution format (hyper) parameter values
#' @param parameter_cost distribution format hyper parameter values
#'
#' @return
#' @export
#'
#' @examples
#'
combine_cost_and_p_xlsheets <- function(parameter_p,
                                        parameter_cost) {

  if (nrow(parameter_p) <= 0) {
    return(parameter_cost)
  }

  # if not distn format
  # transform to long format
  if (!"node" %in% names(parameter_p)) {

    parameter_p <-
      parameter_p %>%
      as.data.frame() %>%
      reshape2::melt(id.vars = "scenario") %>%
      plyr::rename(replace = c("variable" = "node",
                               "value" = "p")) %>%
      mutate(val_type = "p")
  }

  if (!"node" %in% names(parameter_cost)) {

    parameter_cost <-
      parameter_cost %>%
      as.data.frame() %>%
      reshape2::melt(id.vars = "scenario") %>%
      plyr::rename(replace = c("variable" = "node",
                               "value" = "cost")) %>%
      mutate(val_type = "cost")
  }

  return(
    dplyr::bind_rows(parameter_cost,
                     parameter_p))
}
