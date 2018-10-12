
#' stan_predict
#'
#' @param stan_fit
#' @param newdata
#' @param n_draws
#'
#' @return
#' @export
#'
#' @examples
stan_predict <- function(stan_fit,
                         newdata,
                         n_draws) {

  dm0 <- dplyr::filter(newdata, type == "statusquo")
  y0_rep <- posterior_predict(stan_fit,
                              newdata = dm0,
                              draws = n_draws) %>% t()

  dm1 <- dplyr::filter(newdata, type == "screened")
  y1_rep <- posterior_predict(stan_fit,
                              newdata = dm1,
                              draws = n_draws) %>% t()

  INMB_sim <- data.frame(y1_rep - y0_rep)


  out_sim <-
    data.frame(dm0,
               prob_CE = apply(INMB_sim, 1,
                               function(x) sum(x > 0)/ncol(INMB_sim)),
               EINMB = apply(INMB_sim, 1, mean),
               pc_5 = apply(INMB_sim, 1,
                            function(x) quantile(x, probs = 0.05)),
               pc_50 = apply(INMB_sim, 1,
                             function(x) quantile(x, probs = 0.5)),
               pc_95 = apply(INMB_sim, 1,
                             function(x) quantile(x, probs = 0.95)))

  out_sim
}


##TODO:
# stan_plots <- function(out_sim) {
#
#
#   qplot(x = c(y1_rep - y0_rep), geom = "histogram",
#         xlab = "Estimated average treatment effect")
#
#
#   # df <- data.frame(X = c(y1_rep - y0_rep))
#   # ggplot(df, aes(x = X)) +
#   #   geom_histogram(binwidth = 10, fill = "white", color = "black") +
#   #   geom_histogram(binwidth = 10, data = subset(df, X >= 0),
#   #                  colour = "black", fill = "grey") + theme_bw()
# }
