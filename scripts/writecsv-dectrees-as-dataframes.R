#
# writecsv-dectree-as-dataframe.R
# N Green
#

##TODO: change data.tree:: to output _all_ nodes



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


path_probs.screen <- calc_pathway_probs(osNode.cost)
osNode.cost$Set(path_probs = path_probs.screen)

# dtr_clone <- Clone(osNode.cost$`(350,1e+05]`)
dtr_clone <- Clone(osNode.cost)

dtr_clone$Set(weighted_cost = dtr_clone$Get('path_probs') * dtr_clone$Get('sampled'))

##TODO: where is the $Set(sampled) code??


# save --------------------------------------------------------------------

readr::write_csv(x = my_ToDataFrameTable(dtr_clone, "pathString", "path_probs", "sampled", "weighted_cost", "payoff", "p"),
                 path = "data/dtr_DataFrameTable.csv")

readr::write_csv(x = data.tree::ToDataFrameNetwork(dtr_clone, "path_probs", "sampled", "weighted_cost", "payoff", "p"),
                 path = "data/dtr_ToDataFrameNetwork.csv")

readr::write_csv(x = my_ToDataFrameTypeCol(dtr_clone, "path_probs", "sampled", "weighted_cost", "payoff", "p"),
                 path = "data/dtr_ToDataFrameTypeCol.csv")


# absolute numbers --------------------------------------------------------

num_screen_year <- table(ceiling(IMPUTED_sample_year_cohort$screen_year))

nums_dectree <- function(dtr_clone, n_pop) {

  dtr_clone$Set(E_n = round(dtr_clone$Get('path_probs') * n_pop, 2))
  tab <- my_ToDataFrameTypeCol(dtr_clone, "path_probs", "p", "E_n")


  num_LTBI_pre <- dplyr::filter(tab, level_3 == "LTBI" & is.na(level_4))
  num_tests <- dplyr::filter(tab, level_4 == "Agree to Screen", is.na(level_5))
  num_positive <-
    dplyr::filter(tab,
                  (level_3 == "LTBI" & level_5 == "Sensitivity" & is.na(level_6)) |
                    (level_3 == "non-LTBI" & level_5 == "1-Specificity" & is.na(level_6)))
  num_startTx <- dplyr::filter(tab, level_6 == "Start Treatment", is.na(level_7))
  num_completeTx <- dplyr::filter(tab, level_9 == "Complete Treatment", is.na(level_10))
  num_cured <- dplyr::filter(tab, level_10 == "Effective")

  data.frame(num_LTBI_pre = sum(num_LTBI_pre$E_n),
             num_tests = sum(num_tests$E_n),
             num_positive = sum(num_positive$E_n),
             num_startTx = sum(num_startTx$E_n),
             num_completeTx = sum(num_completeTx$E_n),
             num_cured = sum(num_cured$E_n),
             num_LTBI_post = sum(num_LTBI_pre$E_n) - sum(num_cured$E_n))
}

map(num_screen_year,
    function(x) nums_dectree(dtr_clone, n_pop = x))
