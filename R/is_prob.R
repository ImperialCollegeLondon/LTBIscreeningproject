
#
is_prob <- function(prob) {

  each_gt_zero <- all(prob >= 0)
  each_lt_one <- all(prob <= 1)
  total_lt_one <- sum(prob) <= 1

  each_gt_zero && each_lt_one
}
