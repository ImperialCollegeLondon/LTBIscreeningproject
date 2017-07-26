#' table_costeffectiveness
#'
#' @param bcea_obj
#'
#' @return
#' @export
#'
#' @examples
table_costeffectiveness <- function(bcea_obj) {

  attach(bcea_obj)

  out <- do.call(data.frame,
                 list("percentile_95pc" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.95),
                      "percentile_5pc" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.05),
                      "EINB" = eib[k == wtp_threshold, ],
                      "pCE_WTP15000" = apply(ib[k == 15000, , ], 2, function(x) sum(x > 0)/n.sim),
                      "pCE_WTP20000" = apply(ib[k == 20000, , ], 2, function(x) sum(x > 0)/n.sim),
                      "pCE_WTP25000" = apply(ib[k == 25000, , ], 2, function(x) sum(x > 0)/n.sim),
                      "pCE_WTP30000" = apply(ib[k == 30000, , ], 2, function(x) sum(x > 0)/n.sim)))

  detach(bcea_obj)

  return(out)
}

