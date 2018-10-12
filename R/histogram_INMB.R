
#' histogram_INMB
#'
#' @param bcea
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
histogram_INMB <- function(bcea, ...) {

  UseMethod("histogram_INMB", bcea)
}


#' @rdname histogram_INMB
#'
histogram_INMB.bcea <- function(bcea,
                                folders = NA,
                                wtp_threshold = 20000) {

  ##TODO: oneway/twoway conditions

  if (!all(is.na(folders))) {

    filename <- pastef(folders$plots$scenario, "histogram_INMB.png")
  }

  dat <-
    bcea$ib[bcea$k == wtp_threshold, , ] %>%
    melt() %>%
    mutate(X2 = factor(X2))

  print(
    out <-
      ggplot(dat, aes(x = value, color = X2, group = X2)) +
      # geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) +
      geom_density(alpha = 0.1) + theme_bw() +
      xlab("INB"))

  ggplot2::ggsave(file = filename,
                  plot = out,
                  width = 30, height = 20, units = "cm")
}
