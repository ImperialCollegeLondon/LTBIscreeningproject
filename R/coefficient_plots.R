
#' coefficient_plots
#'
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
coefficient_plots <- function(folders) {

  var_names <- c("policyscreened:I(Agree - 90)",
                 "policyscreened:I(Start - 90)",
                 "policyscreened:I(Complete - 90)",
                 "policyscreened:I(Effective - 90)")#,
  # "policyscreened:I(Start - 90):I(Effective - 90)",
  # "policyscreened:I(Start - 90):I(Complete - 90)",
  # "policyscreened:I(Agree - 90):I(Effective - 90)",
  # "policyscreened:I(Agree - 90):I(Complete - 90)",
  # "policyscreened:I(Agree - 90):I(Start - 90)",
  # "policyscreened:I(Complete - 90):I(Effective - 90)")

  var_labels <- c("Agree", "Start", "Complete", "Effective")


  filename <- paste(folders$plots$scenario, "coef_plot.png", sep = "/")

  png(filename, width = 600, height = 600)#, res = 45)

  print(
    arm::coefplot(summary(lm_multi$`30000`)$coefficients[var_names, "Estimate"],
                  summary(lm_multi$`30000`)$coefficients[var_names, "Std. Error"],
                  mar = c(1,15,5.1,2), varnames = var_labels, main = "",
                  xlim = c(-1,7), cex.var = 1.2))
  print(
    arm::coefplot(summary(lm_multi$`20000`)$coefficients[var_names, "Estimate"],
                  summary(lm_multi$`20000`)$coefficients[var_names, "Std. Error"],
                  mar = c(1,15,5.1,2), varnames = var_labels, main = "", add = TRUE, col = "red"))
  print(
    arm::coefplot(summary(lm_multi$`10000`)$coefficients[var_names, "Estimate"],
                  summary(lm_multi$`10000`)$coefficients[var_names, "Std. Error"],
                  mar = c(1,15,5.1,2), varnames = var_labels, main = "", add = TRUE, col = "blue"))

  legend("bottomright",
         legend = c("£10k","£20k","£30k"),
         col = c("blue", "red", "black"), lty = 1, horiz = TRUE)

  dev.off()
}
