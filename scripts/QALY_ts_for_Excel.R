#
# QALY_ts_for_Excel
#
#
#



cohort <-
  IMPUTED_sample %>%
  subset(all_tb == TRUE,
         screen_year = 2009)

cohort <- cohort[1:200, ]

cohort <-
  cohort %>%
  dplyr::mutate(tb_fatality = ifelse(test = is.na(cfr),
                                     yes = NA,
                                     no = runif(n = n()) < cfr))


# main --------------------------------------------------------------------

interv$discount_rate <- 0#0.035

attach(cohort)

all_death_notif[all_death_notif < 0] <- 0

diseasefree <- calc_QALY_population(age = age_all_notification,
                                    start_delay = 0,
                                    discount_rate = interv$discount_rate,
                                    utility = utility$disease_free,
                                    time_horizons = all_death_notif,
                                    sum_res = FALSE,
                                    # utility_method = "prod")
                                    utility_method = "add")

fatality <- calc_QALY_population(age = age_all_notification,
                                 start_delay = 0,
                                 discount_rate = interv$discount_rate,
                                 utility = utility$activeTB,
                                 time_horizons = pmin(all_death_notif, 0.5),
                                 sum_res = FALSE,
                                 # utility_method = "prod")
                                 utility_method = "add")

cured <- calc_QALY_population(age = age_all_notification,
                              start_delay = 0,
                              discount_rate = interv$discount_rate,
                              utility = c(utility$activeTB, utility$postTx),
                              time_horizons = all_death_notif,
                              sum_res = FALSE,
                              # utility_method = "prod")
                              utility_method = "add")

detach(cohort)

QALY_all_tb <- calc_QALY_tb(
  timetoevent = cohort$all_death_notif,
  utility = utility,
  age = cohort$age_all_notification,
  # age = NA, #commentout
  start_delay = 0,
  discount_rate = interv$discount_rate,
  utility_method = "add"
)

cohort <-
  cohort %>%
  dplyr::mutate(QALY_fatality = ifelse(test = all_tb,
                                       yes = QALY_all_tb$fatality,
                                       no = NA),
                QALY_diseasefree = ifelse(test = all_tb,
                                          yes = QALY_all_tb$diseasefree,
                                          no = NA),
                QALY_cured = ifelse(test = all_tb,
                                    yes = QALY_all_tb$cured,
                                    no = NA),
                QALY_statusquo = ifelse(test = tb_fatality,
                                        yes = QALY_fatality,
                                        no = QALY_cured))


# wide format; equal lengths
indx <- sapply(diseasefree, length)
res_free <- as.data.frame(do.call(rbind,
                                  lapply(diseasefree, `length<-`,
                                         max(indx))))
indx <- sapply(cured, length)
res_cure <- as.data.frame(do.call(rbind,
                                  lapply(cured, `length<-`,
                                         max(indx))))
indx <- sapply(fatality, length)
res_fat <- as.data.frame(do.call(rbind,
                                  lapply(fatality, `length<-`,
                                         max(indx))))

res_sq <- res_cure
res_sq[cohort$tb_fatality, ] <- NA #clear contents
res_sq[cohort$tb_fatality, 1] <- res_fat[cohort$tb_fatality,]


write.csv(res_free, file = "ext-data/QALY_diseasefree_ts.csv")
write.csv(res_sq, file = "ext-data/QALY_statusquo_ts.csv")


# p_LTBI_to_cured sample to disease-free in status-quo

p_LTBI_to_cured <- 0.9
who_avoid <- 1:(nrow(res_sq)*p_LTBI_to_cured)
# who_avoid <- sample(x = seq_len(nrow(res_sq)),
#                     size = nrow(res_sq)*p_LTBI_to_cured)

res_screen <- res_sq
res_screen[who_avoid, ] <- res_free[who_avoid, ]

write.csv(res_screen, file = "ext-data/QALY_screen_ts.csv")





###############################

scenario_res$subset_pop[[1]] <- matrix(c(NA, p_LTBI_to_cured), nrow = 1,
                                       dimnames = list(NULL, c('dummy', 'p_LTBI_to_cured')))

n.scenarios <- 1
interv$N.mc <- 1
interv$ENDPOINT_cost <- "death"
interv$ENDPOINT_QALY <- "death"

cohort$id_avoided_tb <- 1:nrow(cohort)









###################
# spaghetti plots #
###################

# long format
yy <-
  res_sq %>%
  t() %>%
  as.data.frame() %>%
  mutate(year = seq_len(nrow(.))) %>%
  melt.data.frame(id.vars = 'year')

yy$value <- yy$value + rnorm(n = nrow(yy), mean = 0, sd = 0.005)
# yy$year <- yy$year + rnorm(n = nrow(yy), mean = 0, sd = 1)
yy$value[is.na(yy$value)] <- 0

rsample <- runif(n = length(unique(yy$variable)))

yy <-
  yy %>%
  group_by(year) %>%
  dplyr::mutate(alpha = ifelse(rsample > 0.99, 1, 0.01))

tspag <- ggplot(yy, aes(x = year, y = value)) +
  geom_line() +
  coord_cartesian(ylim = c(0.7, 1)) +
  guides(colour = FALSE) +
  xlab("Year") +
  ylab("Utility")

# all cases
spag = tspag + aes(colour = factor(variable))
spag

# highlight subset of individuals
tspag + aes(alpha = alpha,
            group = factor(variable)) +
  guides(alpha = FALSE) +
  theme_bw()
