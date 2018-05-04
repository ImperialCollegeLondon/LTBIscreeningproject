#
# LTBI screening
# N Green
# combine_num_screen_tables.R


flder <-
  list.dirs(parent_folder)[-1] %>%
  sort()

tab <-
  read.csv(paste0(flder[1], "/all_subsets.csv")) %>%
  select(-X)

for (i in seq_along(flder)[-1]) {

  tab <-
    read.csv(paste0(flder[i], "/all_subsets.csv")) %>%
    select(-X) %>%
    merge(tab, .,
          by = c("scenario", "X2"),
          suffixes = c("", paste("_", i, sep = "")))
}


tab <-
  tab %>%
  mutate(scenario = as.numeric(scenario)) %>%
  arrange(scenario)

write.csv(tab, file = paste0(parent_folder, "/combined_all_subsets.csv"))


# double header Excel file
##TODO: specify column names

# XLConnect::writeWorksheetToFile(file = paste0(parent_folder, "/combined_all_subsets.xlsx"),
#                                 data = header,
#                                 sheet = "test",
#                                 startRow = 1,
#                                 header = FALSE)
#
# XLConnect::writeWorksheetToFile(file = paste0(parent_folder, "/combined_all_subsets.xlsx"),
#                                 data = example,
#                                 sheet = "test",
#                                 startRow = 2)



