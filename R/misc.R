
days_to_years <- function(days)
  ceiling(days/365)



remove_duplicates <- function(dat)
  dat[!duplicated(dat)]


delete_text <- purrr::partial(gsub, replacement = "")

pastef <- purrr::partial(...f = paste, sep = "/")

rm_last <- function(prob)
  prob[2:length(prob) - 1]

rm_na <- function(dat)
  dat[!is.na(dat)]
