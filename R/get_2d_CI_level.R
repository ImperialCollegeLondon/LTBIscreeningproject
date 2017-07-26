
# https://stackoverflow.com/questions/23437000/how-to-plot-a-contour-line-showing-where-95-of-values-fall-within-in-r-and-in
#
# set.seed(1001)
# d <- data.frame(x = rnorm(1000), y = rnorm(1000))
# L95 <- get_2d_CI_level(d$x, d$y)
# L05 <- get_2d_CI_level(d$x, d$y, prob = 0.05)
#
# kk <- with(d, MASS::kde2d(x,y))
#
# dimnames(kk$z) <- list(kk$x,kk$y)
#
# dc <- melt(kk$z)
#
# ggplot(dc, aes(x = X1, y = X2)) +
#   geom_contour(aes(z = value), breaks = c(L05, L95))

get_2d_CI_level <- function(x,
                            y,
                            prob = 0.95) {
  kk <- MASS::kde2d(x,y)
  dx <- diff(kk$x[1:2])
  dy <- diff(kk$y[1:2])
  sz <- sort(kk$z)
  c1 <- cumsum(sz) * dx * dy
  approx(c1, sz, xout = 1 - prob)$y
}


## different colour contours

# kd <- ks::kde(d, compute.cont = TRUE)
#
# contour_95 <-
#   with(kd, contourLines(x = eval.points[[1]], y = eval.points[[2]],
#                         z = estimate, levels = cont["5%"])[[1]]) %>%
#   data.frame(prob = "95")
#
# contour_05 <-
#   with(kd, contourLines(x = eval.points[[1]], y = eval.points[[2]],
#                         z = estimate, levels = cont["95%"])[[1]]) %>%
#   data.frame(prob = "05")
#
# contours <- rbind(contour_95, contour_05)
#
# ggplot(data = d, aes(x, y)) +
#   # geom_point() +
#   geom_path(aes(x, y, colour = prob), data = contours) +
#   theme_bw()
