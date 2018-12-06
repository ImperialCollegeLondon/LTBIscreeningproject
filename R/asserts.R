
#
assert_all_equal <- function(x) {

  abs(max(x) - min(x)) == 0
}


# otherwise silently skips and keeps default
assert_names_in_tree <- function(osNode,
                                 names) {

  all(names %in% osNode$Get('name'))
}
