
#' Table of Cost-Effectiveness Statistics
#'
#' Take \code{BCEA} package oject as input and
#' creates a summary table.
#' 15000, 20000, 25000, 30000 willingness to pay
#'
#' @param bcea_obj Pre-calculated output from BCEA package (class(bcea))
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
table_costeffectiveness <- function(bcea_obj,
                                    wtp_threshold = 20000) {

  out <-
    if (bcea_obj$n.comparisons == 1) {
    with(bcea_obj,
         do.call(data.frame,
                 list("percentile_5th" = quantile(x = ib[k == wtp_threshold, ], probs = 0.05),
                      "EINB" = eib[k == wtp_threshold],
                      "percentile_95th" = quantile(x = ib[k == wtp_threshold, ], probs = 0.95),
                      "ceac_WTP15000" = ceac[k == 15000],
                      "ceac_WTP20000" = ceac[k == 20000],
                      "ceac_WTP25000" = ceac[k == 25000],
                      "ceac_WTP30000" = ceac[k == 30000])))
  }else{

    with(bcea_obj,
         do.call(data.frame,
                 list("percentile_5th" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.05),
                      "EINB" = eib[k == wtp_threshold, ],
                      "percentile_95th" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.95),
                      "ceac_WTP15000" = ceac[k == 15000, ],
                      "ceac_WTP20000" = ceac[k == 20000, ],
                      "ceac_WTP25000" = ceac[k == 25000, ],
                      "ceac_WTP30000" = ceac[k == 30000, ])))
  }

  return(out)
}


#' table_tb_avoided
#'
#' @param n_tb_screen_all
#' @param n_tb_screen_uk
#'
#' @return EWNI and total 5%, 50% and 95% quantiles
#' @export
#'
#' @examples
#'
table_tb_avoided <- function(n_tb_screen_all,
                             n_tb_screen_uk) {

  out <- NULL
  QUANTILES <- c(0.05, 0.5, 0.95)

  for (i in seq_along(n_tb_screen_all)) {

    diseasefree_all <- subset(n_tb_screen_all[[i]],
                              status == "disease-free",
                              select = "n") %>% unlist()

    diseasefree_uk <- subset(n_tb_screen_uk[[i]],
                             status == "disease-free",
                             select = "n") %>% unlist()

    out <- rbind(out, c(diseasefree_uk %>% quantile(probs = QUANTILES),
                        diseasefree_all %>% quantile(probs = QUANTILES)))

  }

  colnames(out) <- paste(c("EWNI","EWNI","EWNI",
                           "Total","Total","Total"), colnames(out))

  return(out)
}

