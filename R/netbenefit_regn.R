net_benefit <- function(e_list, c_list, wtp_threshold) {

  mapply(FUN = function(e, c, wtp) (e * wtp) - c,
         e = e_list,
         c = c_list,
         MoreArgs = list(wtp = wtp_threshold),
         SIMPLIFY = FALSE)
}

# create long array over multiple wtp
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

# partial linear regression function
lm_wtp <- function(nmb_formula,
                   design_matrix, ...) {
  function(threshold){

    lm(nmb_formula,
       data = subset(design_matrix, wtp == threshold))
  }
}

# partial bayesian linear regression function
bayeslm_wtp <- function(nmb_formula,
                        design_matrix, ...) {
  function(threshold){
    arm::bayesglm(formula = nmb_formula,
                  family = gaussian,
                  data = subset(design_matrix, wtp == threshold),
                  prior.mean = 0, prior.scale = Inf, prior.df = Inf)
  }
}

optimal_thresholds <- function(lm_multi, covar, centre) {

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

