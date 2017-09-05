
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


syncr_fixed_paths <- function(from_folder_path, to_folder_path){

  function(filename){
    syncr::syncr(src = paste(from_folder_path, filename, sep = ""),
                 dest = to_folder_path)
  }
}
