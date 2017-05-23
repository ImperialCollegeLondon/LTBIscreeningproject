#
# project: LTBI screening
# N Green
# Oct 2016
#
# sample how many LTBI become active cases
# with potential screening for each scenario
# separate for uk only and all tb cases


# if using cluster LTBI screening output separately
N.mc <- 1000


p_complete_screen_lookup <-
  read.csv(file = pastef(diroutput,
                        "prob_complete_Tx_given_LTBI_by_who.csv"),
           header = FALSE) %>%
  set_names(who_levels) %>%
  mutate(scenario = rownames(.)) %>%
  gather(key = "who_prev_cat_Pareek2011",
         value = "prob",
         -scenario)

n.tb_screen.all_tb <- vector(mode = 'list')
n.tb_screen.uk_tb  <- vector(mode = 'list')


# binom sample cases avoided ----------------------------------------------

who_prev_cat.all_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(all_tb == TRUE) %>%
  select(who_prev_cat_Pareek2011) %>%
  unlist() %>%
  as.character()

who_prev_cat.uk_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(as.logical(uk_tb) == TRUE) %>%
  select(who_prev_cat_Pareek2011) %>%
  unlist() %>%
  as.character()

for (i in seq_len(n.scenarios)) {

  print(sprintf("scenario: %d", i))

  tb_scenarios.all_tb <- c(NULL, NULL)
  tb_scenarios.uk_tb  <- c(NULL, NULL)

  p.completeTx <-
    p_complete_screen_lookup %>%
    dplyr::filter(scenario == i) %>%
    as.data.table()

  setkey(x = p.completeTx, "who_prev_cat_Pareek2011")

  p.complete_treat.all_tb <- p.completeTx[who_prev_cat.all_tb, prob]
  p.complete_treat.uk_tb  <- p.completeTx[who_prev_cat.uk_tb, prob]


  for (tbsample in seq_len(N.mc)) {

    after_screen.all_tb <- sample_tb(prob = p.complete_treat.all_tb)
    after_screen.uk_tb  <- sample_tb(prob = p.complete_treat.uk_tb)

    tb_scenarios.all_tb <- rbind(tb_scenarios.all_tb,
                                 data.frame(sim = tbsample,
                                            status = after_screen.all_tb))
    tb_scenarios.uk_tb <- rbind(tb_scenarios.uk_tb,
                                data.frame(sim = tbsample,
                                           status = after_screen.uk_tb))
  }

  # counts of non/active TB cases _after_ screening
  n.tb_screen.all_tb[[i]] <-
    tb_scenarios.all_tb %>%
    group_by(sim, status) %>%
    tally()

  n.tb_screen.uk_tb[[i]] <-
    tb_scenarios.uk_tb %>%
    group_by(sim, status) %>%
    tally()

  # rename tb status
  n.tb_screen.all_tb[[i]]$status <-
    n.tb_screen.all_tb[[i]]$status %>%
    plyr::revalue(x = as.factor(.),
                  replace = c("0" = "disease-free",
                              "1" = "tb"))

  n.tb_screen.uk_tb[[i]]$status <-
    n.tb_screen.uk_tb[[i]]$status %>%
    plyr::revalue(x = as.factor(.),
                  replace = c("0" = "disease-free",
                              "1" = "tb"))


  # save --------------------------------------------------------------------

  # filename <- sprintf("tb_scenarios_uk_tb - scenario_%d.RData", i)
  # save(tb_scenarios.uk_tb, file = pastef(diroutput, filename))

  filename <- sprintf("n.tb_screen_uk_tb - scenario_%d.RData", i)
  dat <- n.tb_screen.uk_tb[[i]]
  save(dat, file = pastef(diroutput, filename))

  filename <- sprintf("n.tb_screen_all_tb - scenario_%d.RData", i)
  dat <- n.tb_screen.all_tb[[i]]
  save(dat, file = pastef(diroutput, filename))
}
