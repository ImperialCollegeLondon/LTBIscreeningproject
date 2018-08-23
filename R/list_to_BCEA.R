
#' list_to_BCEA
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
list_to_BCEA <- function(scenario_list,
                         discount = 1) {

  scenario_names <-
    seq_along(scenario_list) %>%
    as.character()

  scenario_list %>%
    cbind_list() %>%
    multiply_by(discount) %>%
    set_names(nm = scenario_names)
}


#' list_to_BCEA_incr
#'
#' @rdname list_to_BCEA
#'
#' @return
#' @export
#'
list_to_BCEA_incr <- function(scenario_list,
                              discount = 1) {

  list_to_BCEA(scenario_list,
               discount) %>%
  add_column('0' = 0, .before = 1)
}
