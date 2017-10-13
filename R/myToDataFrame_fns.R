
#' my_ToDataFrameTable
#'
#' @param x
#' @param ...
#' @param pruneFun
#'
#' @return
#' @export
#'
#' @examples
my_ToDataFrameTable <- function(x, ..., pruneFun = NULL) {

  df <- as.data.frame(x, row.names = NULL, optional = FALSE,
                      ..., filterFun = NULL, pruneFun = pruneFun, inheritFromAncestors = TRUE)
  df[, -1]
}


#' my_ToDataFrameTypeCol
#'
#' @param x
#' @param ...
#' @param type
#' @param prefix
#' @param pruneFun
#'
#' @return
#' @export
#'
#' @examples
my_ToDataFrameTypeCol <- function(x, ..., type = "level", prefix = type, pruneFun = NULL){

  cols <- unique(c(x$Get(type, filterFun = isNotLeaf), x$Get(type)))

  pathArgs <- data.tree:::GetPathArgV(cols, type)

  if (is.null(prefix))
    names(pathArgs) <- as.character(cols)
  else names(pathArgs) <- paste0(prefix, "_", cols)

  do.call(my_ToDataFrameTable, c(x, pathArgs, ...))
}
