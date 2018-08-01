
#' combine_cost_and_p_xlsheets
#'
#' @param parameter_p
#' @param parameter_cost
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

  # transform to long format
  parameter_p %>%
    as.data.frame() %>%
    reshape2::melt(id.vars = "scenario") %>%
    plyr::rename(replace = c("variable" = "node",
                             "value" = "p")) %>%
    mutate(val_type = "p") %>%
    dplyr::bind_rows(parameter_cost, .)
}
