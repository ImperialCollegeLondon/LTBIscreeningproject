#
# LTBI screening
# N Green
# combine-num_screen-tables.R


flder <- list.dirs(parent_folder)[-1]

tab <- NULL

for (i in seq_along(flder)) {

  tab <-
    read.csv(paste0(flder[i], "/num_subset_dectree.csv")) %>%
    add_column('policy' = i) %>%
    rbind(tab, .)
}

policy_desc <- read.csv("ext-data/programme-level-scenarios-inputs.csv")

tab <- merge(policy_desc, tab,
             by = 'policy',
             all.y = TRUE, all.x = FALSE)

write.csv(tab, file = "ext-data/combined_num_screen_tables.csv")




