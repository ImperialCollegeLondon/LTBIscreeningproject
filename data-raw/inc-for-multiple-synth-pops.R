
#' ---
#' title: "LTBI screening model:
#' plot of Incidence over time for
#' multiple realisations of synthetic population"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


par(mar = c(5.1,5.1,4.1,2.1))
plot(dens$x, nrow(IMPUTED_sample)*dens$y, type = "l",
     xlab = "Years since entry", ylab = "Number of TB cases", cex.lab = 2, col = alpha("grey", 0.4))

for (i in 1:20) {

  IMPUTED_sample <-
    IMPUTED_sample %>%
    dplyr::mutate(
      all_tb_issdt = sim_tb_times(data = .,
                                  prob = p_tb_given_LTBI_year))

  dens <- density(IMPUTED_sample$all_tb_issdt, from = 1, bw = 1)
  lines(dens$x[-1], nrow(IMPUTED_sample)*dens$y[-1], type = "l", col = alpha("grey", 0.4))
}
