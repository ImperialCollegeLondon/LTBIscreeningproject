
#
is.na_OR_is.inf <- function(x)
  is.na(x) | is.infinite(x)


#
cbind_list <- function(df_list)
  do.call(cbind.data.frame, df_list)
