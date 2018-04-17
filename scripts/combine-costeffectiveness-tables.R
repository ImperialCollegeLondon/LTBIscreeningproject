#
# LTBI screening
# N Green
#
# combine all CE output in to single table


# flder <- list.dirs(parent_folder)[-1]
flder <- list.dirs("ext-data/18_to_35_in_2009")[-1]

tab <- NULL

for (i in seq_along(flder)) {

  tab <-
    read.csv(paste0(flder[i], "/costeffectiveness_table.csv")) %>%
    add_column('policy' = i) %>%
    rbind(tab, .)
}

policy_desc <- read.csv("ext-data/programme-level-scenarios-inputs.csv")

tab <- merge(policy_desc, tab, by = 'policy', all.y = TRUE, all.x = FALSE)

write.csv(tab, file = "ext-data/combined_costeffectiveness_tables.csv")




