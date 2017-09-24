
#' cost_tb_notif
#'
#' @param num_sec_inf
#' @param unit_cost
#' @param secondary_inf_discounts
#' @param notif_discounts
#'
#' @return
#' @export
#'
#' @examples
#'
cost_tb_notif <- function(num_sec_inf,
                          unit_cost,
                          secondary_inf_discounts,
                          notif_discounts) {

  cost_secondary_inf <- num_sec_inf * unit_cost * secondary_inf_discounts
  total_cost <- (notif_discounts * unit_cost) + cost_secondary_inf
  return(total_cost)
}
