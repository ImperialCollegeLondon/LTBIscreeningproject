
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
    INMB_hist <-
      ggplot(dat, aes(x = value, color = X2, group = X2)) +
      # geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) +
      geom_density(alpha = 0.1, show.legend = FALSE) +
      theme_bw() +
      theme(text = element_text(size = 30)) +
      xlab(paste0("INB (", intToUtf8(163), ")")) +
      geom_vline(xintercept = 0, linetype = "dashed")
  )

  ggplot2::ggsave(file = filename,
                  plot = INMB_hist,
                  width = 30, height = 20, units = "cm")

  save(INMB_hist, file = paste(folders$plots$scenario, "INMB_hist.RData", sep = "/"))

  invisible(INMB_hist)
}
