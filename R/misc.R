
days_to_years <- function(days)
  ceiling(days/365)



remove_duplicates <- function(dat)
  dat[!duplicated(dat)]


delete_text <- purrr::partial(gsub, replacement = "")

