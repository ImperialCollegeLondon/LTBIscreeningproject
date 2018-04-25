#
# LTBI screening
# N Green
# combine-costeffectiveness-tables.R
#
# combine all CE output in to single wide table


flder <- list.dirs(parent_folder)[-1]

tab <- list()


# test cost & baseline/perfect

# for (i in seq_along(flder)) {
#
#   tab[[i]] <-
#     read.csv(paste0(flder[i], "/costeffectiveness_table.csv")) %>%
#     mutate(cascade = rep(c("baseline", "perfect"), 3),
#            test_cost = c(100,100,50,50,25,25)) %>%
#     select(cascade, everything()) %>%
#     split(f = .$test_cost) %>%
#     plyr::join_all(by = "cascade")
# }
#
# res <- do.call(cbind, tab, quote = TRUE)
# res <- res[, !grepl(x = names(res), pattern = "X|test_cost|cascade\\.")]


# include drug effectiveness

for (i in seq_along(flder)) {

  tab[[i]] <-
    read.csv(paste0(flder[i], "/costeffectiveness_table.csv")) %>%
    mutate(cascade = rep(seq(1, 0.3, by = -0.1), 3),
           test_cost = rep(c(25,50,100), each = 8)) %>%
    select(cascade, everything()) %>%
    split(f = .$test_cost) %>%
    plyr::join_all(by = "cascade")
}


res <- do.call(cbind, tab, quote = TRUE)
res <- res[, !grepl(x = names(res), pattern = "X|test_cost|cascade\\.")]

write.csv(res, file = "ext-data/wide_combined_costeffectiveness_tables.csv")

#
# for (i in seq_along(flder)) {
#
#   ce_tab <- read.csv(paste0(flder[i], "/costeffectiveness_table.csv"))
#   empty_tab <- matrix(NA, nrow = nrow(ce_tab), ncol = ncol(ce_tab) - 1, dimnames = NULL)
#
#   tab[[i]] <-
#     data.frame(ce_tab, empty_tab, empty_tab)
# }
#
#
# res <-
#   do.call(cbind, tab, quote = TRUE) %>%
#   add_column(cascade = seq(1, 0.3, by = -0.1), .before = 1)
#
# res <- res[, !grepl(x = names(res), pattern = "X$")]
#
# write.csv(res, file = "ext-data/wide_combined_costeffectiveness_tables.csv")
