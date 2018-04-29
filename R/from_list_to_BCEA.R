

#' from_list_to_BCEA
#'
#' transform to BCEA package input format
#'
#' @param scenario_list
#' @param discount
#'
#' @return
#' @export
#'
#' @examples
#'
from_list_to_BCEA <- function(scenario_list,
                              discount = 1) {

  scenario_names <-
    c(0, seq_len(length(scenario_list))) %>%
    as.character(.)

  scenario_list %>%
    do.call(cbind.data.frame, .) %>%
    multiply_by(discount) %>%
    add_column('0' = 0, .before = 1) %>%
    set_names(nm = scenario_names)
}
