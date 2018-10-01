
#' bayes_predict
#'
#' @param model_fit
#' @param n_sim
#' @param newdata
#'
#' @return
#' @export
#'
#' @examples
bayes_predict <- function(model_fit,
                          newdata,
                          n_sim = 100) {

  # simulate posterior coefficients from fitted model
  lm.sim <- arm::sim(model_fit, n = n_sim)
  coef.sim <- coef(lm.sim)

  pred_inmb <- matrix(NA,
                      ncol = nrow(coef.sim),
                      nrow = num_scenarios)

  pred_inmb_wide <- newdata

  for (i in seq_len(n_sim)) {

    # replace with posterior sample
    model_fit$coefficients <- coef.sim[i, ]

    random_effect <- rnorm(n = nrow(newdata),
                           mean = 0,
                           sd = lm.sim@sigma[i])

    newdata$pred_nb <- predict(model_fit, newdata = newdata, type = "response") + random_effect

    pred_inmb_wide[ ,paste0("pred_nb", i)] <- newdata$pred_nb

    pred_inmb[ ,i] <-
      tidyr::spread(newdata, type, pred_nb) %>%
      # arrange(Effective, Agree, Complete, Start) %>%
      transmute(screened - statusquo) %>% unlist()
  }


  # probability cost-effective
  out <-
    data.frame(newdata[1:num_scenarios, ],
               prob_CE = apply(pred_inmb, 1,
                               function(x) sum(x > 0)/ncol(pred_inmb)),
               pc_5 = apply(pred_inmb, 1,
                            function(x) quantile(x, probs = 0.05)),
               pc_50 = apply(pred_inmb, 1,
                             function(x) quantile(x, probs = 0.5)),
               pc_95 = apply(pred_inmb, 1,
                             function(x) quantile(x, probs = 0.95)))

  out
}
