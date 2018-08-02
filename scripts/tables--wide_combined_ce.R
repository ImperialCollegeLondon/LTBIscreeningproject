#
# LTBI screening
# N Green
# combine-costeffectiveness-tables.R
#
# combine all CE output in to single wide table
#

##TODO: this is just a cbind version of long combined_costeffectiveness_table


flder <-
  list.files(data_folder, pattern = 'policy_[0-9]*$', full.names = TRUE) %>%
  sort()

tab <- matrix(seq_len(n.scenarios), ncol = 1)

for (i in seq_along(flder)) {

  tab <-
    read.csv(paste0(flder[i], "/costeffectiveness_table.csv")) %>%
    cbind(tab, .)
}

tab <-  tab[ ,!names(tab) == 'X']

policy_desc <- read.csv(paste0(data_folder, "/policies-inputs.csv"))

write.csv(tab, file = paste0(data_folder, "/wide_combined_costeffectiveness_tables.csv"))



