#
# row bind all CE output tables into single table
#


parent_folder <- "C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/ext-data/18_to_45_in_2009"
out_folders <- list.dirs(parent_folder, recursive = FALSE) %>% sort()

out <- data.frame()

for (programme_scenario in seq_along(out_folders)) {

  out <-
    rbind(out,
          cbind(programme_scenario,
                read.csv(paste0(out_folders[programme_scenario], "/costeffectiveness_table.csv"))))
}

write.csv(out, file = "ext-data/18_to_45_in_2009/scenarios_combined_table.csv")

