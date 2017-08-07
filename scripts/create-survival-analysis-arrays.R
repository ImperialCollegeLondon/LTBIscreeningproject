# create survival analysis arrays -------------------------------------------------
# just use one imputation sample
# different functions/packages require different formats



# create final state vectors full sample
event <- rep(0, nrow(IMPUTED_sample))                          #event-free i.e. censored at followup
event[IMPUTED_sample$death1] <- 3
event[IMPUTED_sample$exit_uk1] <- 2
event[IMPUTED_sample$uk_tb_orig == "1"] <- 1



# naive case
# assume a.s. all LTBI -> active TB
# i.e. non active TB event censoring times
cens <- event
cens[cens != 1] <- 0
times <- fup_issdt[ ,"fup1_issdt"]
times_years <- days_to_years(fup_issdt[ ,"fup1_issdt"])

dat_surv_naive <-
  data.frame(cens = cens,
             times = times,
             times_years = times_years) %>%
  dplyr::filter(IMPUTED_sample$LTBI_or_activeTB == TRUE)


# impute progression time after follow-up in uk
# from exponential distn

n.LTBI <- sum(IMPUTED_sample$LTBI_or_activeTB)
imputed_uk_tb_excess <- rexp(n = n.LTBI, rate = 1e-5)  #days

dat_surv_etm_imputed_uk_tb <-
  IMPUTED_sample %>%
  dplyr::filter(LTBI_or_activeTB == TRUE) %>%
  transmute(imputed_uk_tb_times = fup1_issdt + imputed_uk_tb_excess,
            date_death1_issdt = date_death1_issdt,
            min_uk_tb_death_impute = pmin(imputed_uk_tb_times,
                                          date_death1_issdt),
            time_days = ifelse(cens1 == TRUE, min_uk_tb_death_impute, fup1_issdt),
            time = days_to_years(time_days),
            to = ifelse(death1 == TRUE, 3,
                        ifelse(exit_uk1 == TRUE, 2,
                               ifelse(uk_tb_orig == "1", 1,
                                      ifelse(imputed_uk_tb_times <= date_death1_issdt, 1, 3)))),
            cens = ifelse(to == 1, 1, 0),
            from = 9,
            id = rownames(.))



# etm:: format
# id from to time_days time
# 4   9   2  1436.080    4
# 14  9   0  1961.000    6

data_etm <-
  data.frame(id = seq_len(nrow(IMPUTED_sample)),
             from = 9,
             to = event,
             time_days = fup_issdt[ ,"fup1_issdt"],
             time = days_to_years(fup_issdt[ ,"fup1_issdt"]),
             LTBI_or_activeTB = IMPUTED_sample$LTBI_or_activeTB) %>%
  dplyr::filter(LTBI_or_activeTB == TRUE,
                time > 0) %>%
  dplyr::select(-LTBI_or_activeTB)

data_etm_cens_exituk <-
  data_etm %>%
  mutate(to = ifelse(to == 2, 0, to))


# flexsurv:: format
# fup1_issdt fup1_issdt_year age_at_entry event3 event2 event1
# 1436.080               4           18      0      1      0
# 1961.000               6           18      0      0      0

dat_surv <-
  fup_issdt %>%
  data.frame() %>%
  transmute(time_days = fup1_issdt,
            time = days_to_years(fup1_issdt),
            age_at_entry = IMPUTED_sample$age_at_entry,
            event3 = as.numeric(event == 3), #death
            event2 = as.numeric(event == 2), #exit_uk
            event1 = as.numeric(event == 1), #uk_tb
            LTBI_or_activeTB = IMPUTED_sample$LTBI_or_activeTB) %>%
  dplyr::filter(LTBI_or_activeTB == TRUE) %>%
  dplyr::select(-LTBI_or_activeTB)


# fup1_issdt fup1_issdt_year age_at_entry id         from     to status trans
# 1436.08               4           18     1 disease-free event3      0     3
# 1436.08               4           18     1 disease-free event2      1     2

dat_surv_long <-
  dat_surv %>%
  mutate(id = rownames(dat_surv),
         from = "disease-free") %>%
  reshape2::melt(measure.vars = c("event3", "event2", "event1")) %>%
  dplyr::rename(to = variable,
                status = value) %>%
  mutate(trans = as.factor(delete_text(pattern = "event",
                                       x = to))) %>%
  arrange(id)


dat_surv_long_cens_exit_uk <-
  dat_surv_long %>%
  dplyr::filter(to != "exit_uk1")


## use imputed exit_uk & death times after followup

#   id death1 exit_uk1 uk_tb_orig date_death1_issdt date_exit_uk1_issdt fup_limit_issdt fup1_issdt tb_issdt
# 1  1      0        1          0     14407.55 days       1436.080 days       2036 days   1436.080     2036
# 2  2      0        0          0     25126.57 days      36525.000 days       1961 days   1961.000     1961

dat_surv_impute <-
  IMPUTED_sample %>%
  dplyr::filter(LTBI_or_activeTB == TRUE) %>%
  transmute(id = rownames(.),
            death1 = as.numeric(death1),
            exit_uk1 = as.numeric(exit_uk1),
            uk_tb_orig = uk_tb_orig,
            date_death1_issdt = date_death1_issdt,
            date_exit_uk1_issdt = date_exit_uk1_issdt,
            fup_limit_issdt = fup_limit_issdt,
            tb_issdt = fup1_issdt)


#  melt times and status separately then combine array

times_temp <-
  dat_surv_impute %>%
  reshape2:::melt.data.frame(measure.vars = c("date_death1_issdt",
                                              "date_exit_uk1_issdt",
                                              "tb_issdt"),
                             id.vars = c("id")) %>%
  dplyr::rename(to = variable,
                time = value) %>%
  arrange(id)


status_temp <-
  dat_surv_impute %>%
  reshape2:::melt.data.frame(measure.vars = c("death1",
                                              "exit_uk1",
                                              "uk_tb_orig"),
                             id.vars = c("id")) %>%
  dplyr::rename(to = variable,
                status = value) %>%
  mutate(from = "disease-free",
         trans = as.factor(ifelse(to == "death1", 3,
                                  ifelse(to == "exit_uk1", 2, 1)))) %>%
  arrange(id)


# id     time status         from     to trans
#  1 14407.55      0 disease-free death1     3
#  2 25126.57      0 disease-free death1     3

dat_surv_impute_long <- data.frame(times_temp[ ,c("id", "time")],
                                   status_temp[ ,c("status", "from", "to", "trans")])

dat_surv_impute_long_cens_exituk <-
  dat_surv_impute_long %>%
  dplyr::filter(to != "exit_uk1")

