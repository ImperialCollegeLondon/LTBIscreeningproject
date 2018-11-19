
#
assert_all_equal <- function(x) {

  abs(max(x) - min(x)) == 0
}


#
assert_names_in_tree <- function(names,
                                 osNode) {

  all(names %in% osNode$Get('name'))
}
