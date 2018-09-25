
#' Setup folders
#'
#' @param policy_name String
#' @param interv List of model run constants
#'
#' @return List of folder locations
#' @export
#'
#' @examples
#'
setup_folders <- function(policy_name,
                          interv) {

  root_wd <- here::here()
  on.exit(setwd(root_wd))
  folders <- list()

  folders$output$scenario <- diroutput(policy_name, interv)

  dir.create(folders$output$scenario, showWarnings = FALSE, recursive = TRUE)

  folders$output$parent <- system.file("ext-data", "18_to_35_in_2009",
                                      package = "LTBIscreeningproject")

  folders$plots$parent <- system.file("output", "plots",
                                      package = "LTBIscreeningproject")

  folders$plots$scenario <- sprintf("%s/%s", folders$plots$parent, policy_name)
  dir.create(folders$plots$scenario, showWarnings = FALSE)

  cp_in_data_to_out_dir(file_names = c("scenario_params_df.csv",
                                       "policies-inputs.csv"),
                        to_dir = folders$output$scenario)

  return(folders)
}


#' Copy input data to output folder
#'
#' @param file_names
#' @param to_dir
#'
#' @return
#' @export
cp_in_data_to_out_dir <- function(file_names,
                                  to_dir) {

  home_dir <- here::here()
  setwd(to_dir)

  for (i in seq_along(file_names)) {

    file.copy(from = paste(home_dir, "data", file_names[i], sep = "/"),
              to = paste("..", file_names[i], sep = "/"))
  }

  return()
}
