#
assert_all_equal <- function(x) {

  abs(max(x) - min(x)) == 0
}
