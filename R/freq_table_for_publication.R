#' freq_table_for_publication
#'
#' https://stackoverflow.com/questions/34587317/using-dplyr-to-create-summary-proportion-table-with-several-
#' categorical-factor-v?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
#'
#' @param wide_df
#' @param row_vars
#' @param col_var
#'
#' @return
#' @export
#'
#' @examples
#'
freq_table_for_publication <- function(wide_df,
                                       row_vars,
                                       col_var) {

  long_df <- melt(wide_df,
                  measure.vars = row_vars)
  res <-
    long_df %>%
    group_by_(col_var, quote(variable), quote(value)) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))

  #make an 'export' variable
  res$export <- with(res, sprintf("%i (%.1f%%)", n, freq*100))

  #reshape again
  output <- dcast(variable + value ~ who_inc_Pareek2011,
                  value.var = "export",
                  data = res,
                  fill = "missing") #use drop=F to prevent silent missings
  #'silent missings'
  output$variable <- as.character(output$variable)
  #make 'empty lines'
  empties <- data.frame(variable = unique(output$variable), stringsAsFactors = FALSE)
  empties[ ,colnames(output)[-1]] <- ""

  #bind them together
  output2 <- rbind(empties,output)
  output2 <- output2[order(output2$variable,output2$value), ]

  #optional: 'remove' variable if value present

  output2$variable[output2$value != ""] <- ""
  rownames(output2) <- NULL

  return(output2)
}

