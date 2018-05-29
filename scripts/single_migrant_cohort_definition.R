

# no LTBI - exit
cohort <-
  data.frame(
    age_at_entry                  = 18              ,
    who_inc_Pareek2011            = "(150,250]"     ,
    issdt                         = "2009-01-01"    ,
    uk_tb                         = 0               ,
    all_tb                        = 0               ,
    LTBI                          = 0               ,
    screen_year                   = 1               ,
    date_death1                   = "2040-01-01"    ,
    date_death1_issdt             = 31              ,
    date_exit_uk1                 = "2011-01-01"    ,
    date_exit_uk1_issdt           = 2               ,
    rNotificationDate_issdt.years = NA              ,
    all_tb_issdt                  = NA              ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 1               ,
    QALY_diseasefree              = 1               ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = NA
  )

# tb uk
cohort <-
  data.frame(
    age_at_entry                  = "18"            ,
    who_inc_Pareek2011            = "(150,250]"     ,
    issdt                         = "2009-01-01"    ,
    uk_tb                         = 1               ,
    exituk_tb                     = 0               ,
    all_tb                        = 1               ,
    LTBI                          = "1"             ,
    screen_year                   = 1               ,
    date_death1                   = "2040-01-01"    ,
    date_death1_issdt             = 31              ,
    date_exit_uk1                 = "2050-01-01"    ,
    date_exit_uk1_issdt           = 41              ,
    rNotificationDate_issdt.years = 2               ,
    all_tb_issdt                  = 2               ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 1               ,
    QALY_diseasefree              = 1               ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = 1
  )

# tb exit
cohort <-
  data.frame(
    age_at_entry                  = "18"            ,
    who_inc_Pareek2011            = "(150,250]"     ,
    issdt                         = "2009-01-01"    ,
    uk_tb                         = "0"             ,
    all_tb                        = "1"             ,
    LTBI                          = "1"             ,
    screen_year                   = 1               ,
    date_death1                   = "2040-01-01"    ,
    date_death1_issdt             = 31              ,
    date_exit_uk1                 = "2011-01-01"    ,
    date_exit_uk1_issdt           = 2               ,
    rNotificationDate_issdt.years = NA              ,
    all_tb_issdt                  = 3               ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 1               ,
    QALY_diseasefree              = 1               ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = 1
  )

# # LTBI - exit
# cohort <-
#   data.frame(
#     age_at_entry                  = "18"            ,
#     who_inc_Pareek2011            = "(150,250]"     ,
#     issdt                         = "2009-01-01"    ,
#     uk_tb                         = "0"             ,
#     all_tb                        = "0"             ,
#     LTBI                          = "1"             ,
#     screen_year                   = "1"             ,
#     date_death1                   = "2040-01-01"    ,
#     date_exit_uk1                 = "2011-01-01"    ,
#     rNotificationDate             = NA
#   )
#
# # no LTBI - death
# cohort <-
#   data.frame(
#     age_at_entry                  = "18"            ,
#     who_inc_Pareek2011            = "(150,250]"     ,
#     issdt                         = "2009-01-01"    ,
#     uk_tb                         = "0"             ,
#     all_tb                        = "0"             ,
#     LTBI                          = "0"             ,
#     screen_year                   = "1"             ,
#     date_death1                   = "2011-01-01"    ,
#     date_exit_uk1                 = "2040-01-01"    ,
#     rNotificationDate             = NA
#   )
#
# # LTBI - death
# cohort <-
#   data.frame(
#     age_at_entry                  = "18"            ,
#     who_inc_Pareek2011            = "(150,250]"     ,
#     issdt                         = "2009-01-01"    ,
#     uk_tb                         = "0"             ,
#     all_tb                        = "0"             ,
#     LTBI                          = "1"             ,
#     screen_year                   = "1"             ,
#     date_death1                   = "2011-01-01"    ,
#     date_exit_uk1                 = "2040-01-01"    ,
#     rNotificationDate             = NA
#   )
#
