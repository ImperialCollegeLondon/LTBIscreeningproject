# write.excel(my.df)
write.excel <- function(x,
                        row.names = FALSE,
                        col.names = TRUE,
                        ...) {

  write.table(x, "clipboard", sep = "\t", row.names = row.names, col.names = col.names, ...)
}

