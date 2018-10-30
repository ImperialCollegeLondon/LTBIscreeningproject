
##TODO: finish updating, S3

#' CE_plane_trajectories
#'
#' @param bcea
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
CE_plane_trajectories <- function(bcea,
                                  folders) {

  design_mat <-
    pastef(folders$output$parent,
           "scenario_params_df.csv") %>%
    read.csv() %>%
    design_matrix()

  dat <- data.frame(
    design_mat,
    mean_e = colMeans(bcea$delta.e),
    mean_c = colMeans(bcea$delta.c)
  )

dat <- rbind(c(0,0,0,0), dat)

  filename <-
    paste(folders$plots$scenario, "ce_plane_trajectories.png", sep = "/")

  plot(
    NULL,
    xlab = "Health gained (QALYs)",
    ylab = "Cost incurred (£)",
    xlim = c(0, 0.005),
    ylim = c(0, 110)
  )

  for (i in 2:nrow(dat)) {

  arrows(x0 = dat$mean_e[i - 1],
         y0 = dat$mean_c[i - 1],
         x1 = dat$mean_e[i],
         y1 = dat$mean_c[i],
         col = "black")

    text(x = dat$mean_e[i] + 0.0001,
         y = dat$mean_c[i],
         labels = dat$scenario[i])
  }

  # wtp
  abline(a = 0, b = 20000, lty = 2)

  # legend(
  #   "topright",
  #   legend = c("Agree", "Start", "Complete", "Effective"),
  #   col = c("green", "red", "blue", "black"),
  #   lty = 1
  # )

}
