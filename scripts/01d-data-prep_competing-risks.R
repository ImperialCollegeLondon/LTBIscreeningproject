#
# project: LTBI screening
# N Green
# Oct 2016
#
# create competing risk modelling data
# [fit competing risk models to imputed complete dataset]
# _without_ screening

##TODO: use flexsurv, msm, mstate for data prep
##TODO: could use imputed exit uk and death times instead of followup censoring?



# create uk_entry -> follow-up times ---------------------------------------------
#
# fup: Minimum of time to active TB, leaving the UK, death


# find all columns with follow-up time imputations
cols_fup <- grepl(pattern = "fup",
                  x = names(IMPUTED_sample))

# find all columns with either exit uk or death event time imputations
cols_eventdate <- grepl(pattern = "date_exit_uk|date_death",
                        x = names(IMPUTED_sample))


# days to arrival in uk from time origin
issdt.asnumeric <- IMPUTED_sample$issdt - as.Date("1960-01-01")

# days from arrival in uk to end of follow-up
fup_issdt <- apply(IMPUTED_sample[ ,cols_fup], 2,
                   FUN = function(x) x - issdt.asnumeric)

colnames(fup_issdt) <- paste0(colnames(fup_issdt), "_issdt")


# days from uk arrival to death & uk exit
event_issdt <- apply(IMPUTED_sample[ ,cols_eventdate], 2,
                     FUN = function(y) as.Date(y, "%Y-%m-%d") - IMPUTED_sample$issdt)

colnames(event_issdt) <- paste0(colnames(event_issdt), "_issdt")

IMPUTED_sample <- data.frame(IMPUTED_sample,
                             event_issdt,
                             fup_issdt)


# as.Date(19723, origin = "1960-01-01")

fup_limit <- 19723  #days from 1960-01-01 to 2013-12-31

IMPUTED_sample$fup_limit_issdt <- fup_limit - issdt.asnumeric


# first event indicator (T/F) for each imputation -------------------------------

IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(cens1  = fup1 == fup_limit,
         cens2  = fup2 == fup_limit,
         cens3  = fup3 == fup_limit,
         cens4  = fup4 == fup_limit,
         cens5  = fup5 == fup_limit,
         cens6  = fup6 == fup_limit,
         cens7  = fup7 == fup_limit,
         cens8  = fup8 == fup_limit,
         cens9  = fup9 == fup_limit,
         cens10 = fup10 == fup_limit,

         death1  = date_death1_issdt == fup1_issdt,
         death2  = date_death2_issdt == fup2_issdt,
         death3  = date_death3_issdt == fup3_issdt,
         death4  = date_death4_issdt == fup4_issdt,
         death5  = date_death5_issdt == fup5_issdt,
         death6  = date_death6_issdt == fup6_issdt,
         death7  = date_death7_issdt == fup7_issdt,
         death8  = date_death8_issdt == fup8_issdt,
         death9  = date_death9_issdt == fup9_issdt,
         death10 = date_death10_issdt == fup10_issdt,

         exit_uk1  = date_exit_uk1_issdt == fup1_issdt,
         exit_uk2  = date_exit_uk2_issdt == fup2_issdt,
         exit_uk3  = date_exit_uk3_issdt == fup3_issdt,
         exit_uk4  = date_exit_uk4_issdt == fup4_issdt,
         exit_uk5  = date_exit_uk5_issdt == fup5_issdt,
         exit_uk6  = date_exit_uk6_issdt == fup6_issdt,
         exit_uk7  = date_exit_uk7_issdt == fup7_issdt,
         exit_uk8  = date_exit_uk8_issdt == fup8_issdt,
         exit_uk9  = date_exit_uk9_issdt == fup9_issdt,
         exit_uk10 = date_exit_uk10_issdt == fup10_issdt)


IMPUTED_sample_year_cohort <-
  IMPUTED_sample %>%
  dplyr::filter(issdt_year == year_cohort) %>%
  transmute(ref_id = ref_id,
            cens1  = fup1 == fup_limit,
            cens2  = fup2 == fup_limit,
            cens3  = fup3 == fup_limit,
            cens4  = fup4 == fup_limit,
            cens5  = fup5 == fup_limit,
            cens6  = fup6 == fup_limit,
            cens7  = fup7 == fup_limit,
            cens8  = fup8 == fup_limit,
            cens9  = fup9 == fup_limit,
            cens10 = fup10 == fup_limit,

            death1  = date_death1_issdt == fup1_issdt,
            death2  = date_death2_issdt == fup2_issdt,
            death3  = date_death3_issdt == fup3_issdt,
            death4  = date_death4_issdt == fup4_issdt,
            death5  = date_death5_issdt == fup5_issdt,
            death6  = date_death6_issdt == fup6_issdt,
            death7  = date_death7_issdt == fup7_issdt,
            death8  = date_death8_issdt == fup8_issdt,
            death9  = date_death9_issdt == fup9_issdt,
            death10 = date_death10_issdt == fup10_issdt,

            exit_uk1  = date_exit_uk1_issdt == fup1_issdt,
            exit_uk2  = date_exit_uk2_issdt == fup2_issdt,
            exit_uk3  = date_exit_uk3_issdt == fup3_issdt,
            exit_uk4  = date_exit_uk4_issdt == fup4_issdt,
            exit_uk5  = date_exit_uk5_issdt == fup5_issdt,
            exit_uk6  = date_exit_uk6_issdt == fup6_issdt,
            exit_uk7  = date_exit_uk7_issdt == fup7_issdt,
            exit_uk8  = date_exit_uk8_issdt == fup8_issdt,
            exit_uk9  = date_exit_uk9_issdt == fup9_issdt,
            exit_uk10 = date_exit_uk10_issdt == fup10_issdt) %>%
  merge(IMPUTED_sample_year_cohort, by = "ref_id", sort = FALSE)



IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(LTBI_or_activeTB = LTBI == 1 | uk_tb_orig == 1)


# create survival analysis arrays -------------------------------------------------
# just use one imputation sample
# different functions/packages require different formats



# create final state vectors full sample
event <- rep(0, n.pop)                          #event-free i.e. censored at followup
event[IMPUTED_sample$death1] <- 3
event[IMPUTED_sample$exit_uk1] <- 2
event[IMPUTED_sample$uk_tb_orig == "1"] <- 1



# naive case
# assume a.s. all LTBI -> active TB
# i.e. non active TB event censoring times
cens <- event
cens[cens != 1] <- 0
times <- fup_issdt[ ,"fup1_issdt"]
times_years = days_to_years(fup_issdt[ ,"fup1_issdt"])

dat_surv_naive <-
  data.frame(cens = cens,
             times = times,
             times_years = times_years) %>%
  dplyr::filter(IMPUTED_sample$LTBI_or_activeTB == TRUE)


# impute progression time after follow-up

n.LTBI <- sum(IMPUTED_sample$LTBI_or_activeTB)
imputed_uk_tb_excess <- rexp(n = n.LTBI, rate = 1e-5)  #days

dat_surv_imputed_uk_tb <-
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
  data.frame(id = seq_len(n.pop),
             from = 9,
             to = event,
             time_days = fup_issdt[ ,"fup1_issdt"],
             time = days_to_years(fup_issdt[ ,"fup1_issdt"]),
             LTBI_or_activeTB = IMPUTED_sample$LTBI_or_activeTB) %>%
  dplyr::filter(LTBI_or_activeTB == TRUE,
         time > 0) %>%
  select(-LTBI_or_activeTB)

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
  select(-LTBI_or_activeTB)


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
  reshape2::melt(measure.vars = c("date_death1_issdt",
                                  "date_exit_uk1_issdt",
                                  "tb_issdt"),
                 id.vars = c("id")) %>%
  dplyr::rename(to = variable,
                time = value) %>%
  arrange(id)


status_temp <-
  dat_surv_impute %>%
  reshape2::melt(measure.vars = c("death1",
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

