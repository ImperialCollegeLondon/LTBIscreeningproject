
#' Net benefit on c and e lists
#'
#' @param e_list
#' @param c_list
#' @param wtp_threshold
#'
#' @return
#' @export
#'
#' @examples
net_benefit_list <- function(e_list,
                             c_list,
                             wtp_threshold) {

  mapply(FUN = netbenefit(e, c, wtp),
         e = e_list,
         c = c_list,
         MoreArgs = list(wtp = wtp_threshold),
         SIMPLIFY = FALSE)
}


netbenefit <- function(e, c, wtp) {
  (e * wtp) - c
}


#' Net monetary benefit over scenarios
#'
#' Create long array over multiple wtp.
#'
#' @param e0
#' @param c0
#' @param e1
#' @param c1
#' @param wtp
#'
#' @return
#' @export
#'
nmb_scenarios <- function(e0, c0,
                          e1, c1,
                          wtp) {

  seq_runs <- seq.int(nrow(e0))

  nb0 <-
    netbenefit(e0, c0, wtp) %>%
    cbind(runs = seq_runs, .)

  nb1 <-
    netbenefit(e1, c1, wtp) %>%
    cbind(runs = seq_runs, .)

  list(statusquo = nb0,
       screened  = nb1) %>%
    reshape2::melt(id.var = "runs") %>%
    dplyr::rename(scenario = variable,
                  NMB = value,
                  type = L1) %>%
        mutate(wtp = wtp)
}

#' Partial linear regression function
#'
#' @param formula Regression formula
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
lm_wtp <- function(formula, ...) {

  function(nmb_mat){
    lm(formula,
       data = nmb_mat)
  }
}

#' Partial Bayesian linear regression function
#'
#' @param nmb_form
#' @param ...
#'
#' @return
#' @export
bayeslm_wtp <- function(nmb_form, ...) {

  function(nmb_mat){
    arm::bayesglm(formula = nmb_form,
                  family = gaussian,
                  data = nmb_mat,
                  prior.mean = 0,
                  prior.scale = Inf,
                  prior.df = Inf)
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

