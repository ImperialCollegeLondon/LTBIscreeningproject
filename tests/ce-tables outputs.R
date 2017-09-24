##scenario 6

tb_fatality %>% table
# .
# FALSE  TRUE
# 508    22

cfr %>% table()
#.
# 0.012 0.048 0.176
# 316   142    72

uk_notif_dates %>% table
# .
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 42 43 44 45 46 47 48 49 50 51 52 54 56 58 62 64 66
# 4  2 11 11  4 21 15 19  3 10  7  5 15  3 11  2  9 12  4  8  9  5  6  6  5  9  1  5  1  5  4  1  4  5  1  1  4  3  3  3  6  2  1  5  2  2  1  1  1  3  1  2  1  1  2  1  1

all_notif_dates %>% table
# .
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 54 55 56 58 60 61 62 64 66
# 4  9 25 36 15 37 25 28 18 24 11 12 19  8 18  4 12 18  6 13 10  7 10 10 11 13  3  5  2  5  7  6  6  5  4  1  6  4  4  4  5  8  4  2  7  2  5  1  3  2  5  3  4  1  1  3  1  2  3  2  1

xx <- IMPUTED_sample_year_cohort %>%
  subset(all_tb == TRUE) %>%
  dplyr::transmute(fatality = QALY_fatality,
                   cured = QALY_cured,
                   diseasefree = QALY_diseasefree)
xx[xx$cured > xx$diseasefree, ]

IMPUTED_sample[IMPUTED_sample$QALY_cured > IMPUTED_sample$QALY_diseasefree & !is.na(IMPUTED_sample$QALY_diseasefree),
               c("QALY_cured", "QALY_diseasefree", "ref_id", "all_death_rNotificationDate")]


IMPUTED_sample %>%
  subset(ref_id == "PKISB010002125118346") %$%
  calc_QALY_tb(timetoevent = all_death_rNotificationDate,
               utility.disease_free = utility$disease_free,
               utility.case = utility$activeTB,
               age = age_all_notification)
