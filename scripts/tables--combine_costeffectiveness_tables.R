#
# LTBI screening
# N Green
# combine-costeffectiveness-tables.R
#
# combine all CE output in to single table


##TODO:test
flder <-
  list.files(data_folder, pattern = 'policy_[0-9]*$', full.names = TRUE) %>%
  sort()

tab <- NULL

for (i in seq_along(flder)) {

  tab <-
    read.csv(paste0(flder[i], "/costeffectiveness_table.csv")) %>%
    add_column('policy' = i) %>%
    rbind(tab, .)
}

policy_desc <- read.csv(paste0(data_folder, "/policies-inputs.csv"))

tab <- merge(policy_desc, tab,
             by = 'policy',
             all.y = TRUE, all.x = FALSE)

write.csv(tab, file = paste0(data_folder, "/combined_costeffectiveness_tables.csv"))
