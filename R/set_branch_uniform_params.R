
#' set_branch_uniform_params
#'
#' @param vals
#' @param osNode
#'
#' @return
#' @export
#'
#' @examples
#'
set_branch_uniform_params <- function(vals,
                                      osNode) {

  UseMethod("set_branch_uniform_params")
}


#' @rdname set_branch_uniform_params
#' @export
#'
set_branch_uniform_params.default <- function(vals,
                                              osNode){

  stop("Error: Not a permitted class input object.")
}


#' @rdname set_branch_uniform_params
#' @export
#'
set_branch_uniform_params.branch_unif_params <- function(vals,
                                                         osNode) {

  stopifnot(assert_names_in_tree(osNode, vals$name))

  osNode$Set(pmin = vals$pmin,
             pmax = vals$pmax,
             filterFun = function(x) x$name == vals$name)
}


#' @rdname set_branch_uniform_params
#' @export
#'
set_branch_uniform_params.test <- function(vals,
                                           osNode) {

  set_branch_uniform_params(vals$sens, osNode)
  set_branch_uniform_params(vals$spec, osNode)
}
