
#' Plot a Cost-Effectiveness Acceptability Curve
#'
#' The function that comes with teh BCEA package
#' is a bit basic.
#'
#' TODO: ggplot version
#'
#' @param screen.bcea A BCEA object from the BCEA package
#' @param new_window
#'
#' @return
#' @export
#'
#' @examples
#'
my_ceac.plot <- function(screen.bcea, new_window = FALSE) {

  require(RColorBrewer)

  NUM_SCENARIOS <- ncol(screen.bcea$ceac)

  rainbow_cols <-  colorRampPalette(c('red','blue','green'))(NUM_SCENARIOS)

  if (new_window) windows(width = 100, height = 50)

  plot(x = seq(0, 50000, by = 100), y = screen.bcea$ceac[,1],
       ylim = c(0, 1), xlim = c(10000, 30000),
       type = 'l',
       xlab = "Willingness to pay (£)",
       ylab = "Probability cost-effective")

  for (i in 1:NUM_SCENARIOS) {
    # for (i in 9:16) {

    lines(x = seq(0, 50000, by = 100), y = screen.bcea$ceac[,i],
          col = rainbow_cols[i], lty = i)
  }

  legend('topleft',
         legend = 1:NUM_SCENARIOS,
         col = rainbow_cols,
         lty = 1:NUM_SCENARIOS)
  abline(v = 20000, col = "grey")
}
