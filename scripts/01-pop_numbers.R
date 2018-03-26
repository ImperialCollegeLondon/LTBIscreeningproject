
# total sample size
n.pop_screen <- nrow(IMPUTED_sample)

# total sample sizes for each yearly cohort
n.popyear_screen <-
  aggregate(x = rep(1, n.pop_screen),
            by = list(IMPUTED_sample$issdt_year),
            sum) %>%
  purrr::set_names(c("year", "pop"))


# expected LTBI prob
# used in sampling tb times
# prop_table(IMPUTED_sample$who_prev_cat_Pareek2011) %*% pLatentTB.who %>% c()
