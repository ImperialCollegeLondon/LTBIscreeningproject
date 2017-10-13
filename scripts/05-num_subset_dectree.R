#
# 05-num_subset_dectree
#


num_subset_list <- lapply(map(dectree_res, "subset_pop"),
                             # function(x) num_screen_year %o% t(round(x, 2)))
                             function(x) num_screen_year %o% t(x))

num_subset_list <- map(num_subset_list, round)

num_subset_dectree <- plyr::ldply(num_subset_list,
                                  data.frame,
                                  .id = "scenario") %>% cbind(year = 1:5, .)

write.csv(num_subset_dectree, file = pastef(diroutput, "num_subset_dectree.csv"))
