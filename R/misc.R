
#
is.na_OR_is.inf <- function(x)
  is.na(x) | is.infinite(x)


#
cbind_list <- function(df_list)
  do.call(cbind.data.frame, df_list)


#
drop_all_na_rows <- function(df) {

  value_cols <-
    df %>% dplyr::select(-node,
                         -val_type,
                         -scenario)

  keep_rows <- apply(value_cols, 1,
                     function(x) !all(is.na(x)))

  return(df[keep_rows, ])
}
