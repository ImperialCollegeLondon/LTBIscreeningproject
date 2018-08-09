
#' Net benefit
#'
#' @param e_list
#' @param c_list
#' @param wtp_threshold
#'
#' @return
#' @export
#'
#' @examples
net_benefit <- function(e_list,
                        c_list,
                        wtp_threshold) {

  mapply(FUN = function(e, c, wtp) (e * wtp) - c,
         e = e_list,
         c = c_list,
         MoreArgs = list(wtp = wtp_threshold),
         SIMPLIFY = FALSE)
}


#' Net monetary benefit
#'
#' Create long array over multiple wtp.
#'
#' @param e_statusquo
#' @param c_statusquo
#' @param e_screened
#' @param c_screened
#' @param wtp_threshold
#'
#' @return
#' @export
#'
#' @examples
nmb <- function(e_statusquo, c_statusquo,
                e_screened, c_screened,
                wtp_threshold) {

  list(statusquo = net_benefit(e_list = e_statusquo,
                               c_list = c_statusquo,
                               wtp_threshold),
       screened = net_benefit(e_list = e_screened,
                              c_list = c_screened,
                              wtp_threshold)) %>%
    reshape2::melt() %>%
    set_names(c("NMB", "scenario", "policy")) %>%
    mutate(wtp = wtp_threshold)
}

#' Partial linear regression function
#'
#' @param nmb_formula Regression formula
#' @param design_matrix Input data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
lm_wtp <- function(nmb_formula,
                   design_matrix, ...) {
  function(threshold){

    lm(nmb_formula,
       data = subset(design_matrix, wtp == threshold))
  }
}

#' Partial Bayesian linear regression function
#'
#' @param nmb_formula
#' @param design_matrix
#' @param ...
#'
#' @return
#' @export
bayeslm_wtp <- function(nmb_formula,
                        design_matrix, ...) {
  function(threshold){
    arm::bayesglm(formula = nmb_formula,
                  family = gaussian,
                  data = subset(design_matrix, wtp == threshold),
                  prior.mean = 0, prior.scale = Inf, prior.df = Inf)
  }
}

#' Find optimal thresholds
#'
#' @param lm_multi
#' @param covar
#' @param centre
#'
#' @return
#' @export
#'
#' @examples
optimal_thresholds <- function(lm_multi,
                               covar,
                               centre) {

  opt <-
    sapply(lm_multi,
           function(x) centre - x$coefficients["policyscreened"]/x$coefficients[sprintf("policyscreened:I(%s - %s)", covar, centre)])

  # remove out-of-bounds
  opt <-
    opt %>%
    set_names(wtp_seq) %>%
    ifelse(test = . < 100 & . > 0,
           yes =  .,
           no =  NA)

  return(opt)
}

