
# https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/
write_clipboard <- function(x,
                        row.names = FALSE,
                        col.names = TRUE, ...) {

  write.table(x, "clipboard",
              sep = "\t",
              row.names = row.names,
              col.names = col.names,...)
}

