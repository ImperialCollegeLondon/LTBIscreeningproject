
# change data.tree:: to output _all_ nodes

my_ToDataFrameTable <- function(x, ..., pruneFun = NULL) {

  df <- as.data.frame(x, row.names = NULL, optional = FALSE,
                      ..., filterFun = NULL, pruneFun = pruneFun, inheritFromAncestors = TRUE)
  df[, -1]
}

my_ToDataFrameTypeCol <- function(x, ..., type = "level", prefix = type, pruneFun = NULL){

  cols <- unique(c(x$Get(type, filterFun = isNotLeaf), x$Get(type)))
  pathArgs <- data.tree:::GetPathArgV(cols, type)
  if (is.null(prefix))
    names(pathArgs) <- as.character(cols)
  else names(pathArgs) <- paste0(prefix, "_", cols)
  do.call(my_ToDataFrameTable, c(x, pathArgs, ...))
}



dtr_clone <- Clone(osNode.cost$`(50,150]`)

dtr_clone$Set(weighted_cost = dtr_clone$Get('path_probs') * dtr_clone$Get('sampled'))


readr::write_csv(x = my_ToDataFrameTable(dtr_clone, "pathString", "path_probs", "sampled", "weighted_cost", "payoff", "p"),
                 path = "data/dtr_DataFrameTable.csv")

readr::write_csv(x = ToDataFrameNetwork(dtr_clone, "path_probs", "sampled", "weighted_cost", "payoff", "p"),
                 path = "data/dtr_ToDataFrameNetwork.csv")

readr::write_csv(x = my_ToDataFrameTypeCol(dtr_clone, "path_probs", "sampled", "weighted_cost", "payoff", "p"),
                 path = "data/dtr_ToDataFrameTypeCol.csv")

