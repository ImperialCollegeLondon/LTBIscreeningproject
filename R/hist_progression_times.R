
#' hist_progression_times
#'
#' @param dat Individual level cohort data
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' hist_progression_times(cohort)
#'
hist_progression_times <- function(dat) {

  # catch error when no TB activation outside of EWNI
  try_out <- try({
    hist(ceiling(dat$exituk_tb.years), col = "blue",
         breaks = 150,
         xlab = "Time (year)", main = "");

    hist(ceiling(dat$rNotificationDate_issdt.years), add = T, col = "green",
         breaks = 150);

    hist(ceiling(dat$rNotificationDate_issdt.years[dat$uk_tb_orig == 1]), add = T,
         breaks = 12, col = "red")
  } , silent = T)


  if (inherits(try_out, "try-error")) {

    hist(ceiling(dat$rNotificationDate_issdt.years), col = "green",
         breaks = 150,
         xlab = "Time (year)", main = "")

    hist(ceiling(dat$rNotificationDate_issdt.years[dat$uk_tb_orig == 1]), add = T,
         breaks = 12, col = "red")
  }

  legend('topright',
         c('Observed', 'EWNI', 'Outside EWNI'),
         col = c('red', 'green', 'blue'), lty = 1)
}
