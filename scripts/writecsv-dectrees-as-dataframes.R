# ********************************************
# LTBI screening
# N Green
# 2017
#
# writecsv-dectree-as-dataframe.R

##TODO: change data.tree:: to output _all_ nodes


# dtr_clone <- Clone(osNode.cost$`(350,1e+05]`)
dtr_clone <- Clone(dectree_res$`1`$osNode.cost)
dtr_clone$Set(weighted_cost = dtr_clone$Get('path_probs') * dtr_clone$Get('sampled'))


# save --------------------------------------------------------------------

# readr::write_csv(x = my_ToDataFrameTable(dtr_clone, "pathString", "path_probs", "sampled", "weighted_cost", "payoff", "p"),
#                  path = "data/dtr_DataFrameTable.csv")
#
# readr::write_csv(x = data.tree::ToDataFrameNetwork(dtr_clone, "path_probs", "sampled", "weighted_cost", "payoff", "p"),
#                  path = "data/dtr_ToDataFrameNetwork.csv")

readr::write_csv(x = my_ToDataFrameTypeCol(dtr_clone, "path_probs", "sampled", "weighted_cost", "payoff", "p"),
                 path = "data/dtr_ToDataFrameTypeCol.csv")

