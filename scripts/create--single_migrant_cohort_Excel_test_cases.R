# create--single_migrant_cohort_Excel_test_cases.R
# matched with model runs in
# C:\Users\ngreen1\Google Drive\LTBI-screening-cost-effectiveness\spreadsheet model


# scenario 1 --------------------------------------------------------------
# tb in EWNI
# have LTBI, complete treatment, case averted
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
    notif_issdt.years = 2               ,
    all_tb_issdt                  = 2               ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = 1               ,
    num_2nd_inf                   = 0
  )


# scenario 2 --------------------------------------------------------------
# remain EWNI
# No LTBI, but tested, no treatment
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
    date_exit_uk1                 = "2050-01-01"    ,
    date_exit_uk1_issdt           = 41              ,
    notif_issdt.years = NA              ,
    all_tb_issdt                  = NA              ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = NA              ,
    num_2nd_inf                   = 0
  )


# scenario 3 --------------------------------------------------------------
# remain EWNI
# LTBI, no case averted, but complete treatment
cohort <-
  data.frame(
    age_at_entry                  = 18              ,
    who_inc_Pareek2011            = "(150,250]"     ,
    issdt                         = "2009-01-01"    ,
    uk_tb                         = 0               ,
    all_tb                        = 0               ,
    LTBI                          = 1               ,
    screen_year                   = 1               ,
    date_death1                   = "2040-01-01"    ,
    date_death1_issdt             = 31              ,
    date_exit_uk1                 = "2050-01-01"    ,
    date_exit_uk1_issdt           = 41              ,
    notif_issdt.years = NA              ,
    all_tb_issdt                  = NA              ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = NA              ,
    num_2nd_inf                   = 0
  )



# scenario 4 --------------------------------------------------------------
# tb exit EWNI
# LTBI, case averted, complete treatment, exit
cohort <-
  data.frame(
    age_at_entry                  = "18"            ,
    who_inc_Pareek2011            = "(150,250]"     ,
    issdt                         = "2009-01-01"    ,
    uk_tb                         = "0"             ,
    all_tb                        = "1"             ,
    LTBI                          = 1               ,
    screen_year                   = 1               ,
    date_death1                   = "2040-01-01"    ,
    date_death1_issdt             = 31              ,
    date_exit_uk1                 = "2011-01-01"    ,
    date_exit_uk1_issdt           = 2               ,
    notif_issdt.years = NA              ,
    all_tb_issdt                  = 3               ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = 1               ,
    num_2nd_inf                   = 0
  )


# scenario 5 --------------------------------------------------------------
# no LTBI - exit EWNI
# LTBI, no case averted, complete treatment, exit
cohort <-
  data.frame(
    age_at_entry                  = 18              ,
    who_inc_Pareek2011            = "(150,250]"     ,
    issdt                         = "2009-01-01"    ,
    uk_tb                         = 0               ,
    all_tb                        = 0               ,
    LTBI                          = 1               ,
    screen_year                   = 1               ,
    date_death1                   = "2040-01-01"    ,
    date_death1_issdt             = 31              ,
    date_exit_uk1                 = "2011-01-01"    ,
    date_exit_uk1_issdt           = 2               ,
    notif_issdt.years = NA              ,
    all_tb_issdt                  = NA              ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = NA              ,
    num_2nd_inf                   = 0
  )


# scenario 6 --------------------------------------------------------------
# No LTBI, but false positive, so complete treatment, no exit
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
    date_exit_uk1                 = "2050-01-01"    ,
    date_exit_uk1_issdt           = 41              ,
    notif_issdt.years = NA              ,
    all_tb_issdt                  = NA              ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = NA              ,
    num_2nd_inf                   = 0
  )

# scenario 7 --------------------------------------------------------------
# LTBI, progress to TB 100%, don't complete treatment, EWNI
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
    notif_issdt.years = 2               ,
    all_tb_issdt                  = 2               ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = 1               ,
    num_2nd_inf                   = 0
  )

# scenario 8 --------------------------------------------------------------
# LTBI, progress to TB 100%, don't complete treatment, EWNI
cohort <-
  data.frame(
    age_at_entry                  = "18"            ,
    who_inc_Pareek2011            = "(150,250]"     ,
    issdt                         = "2009-01-01"    ,
    uk_tb                         = 0               ,
    exituk_tb                     = 0               ,
    all_tb                        = 0               ,
    LTBI                          = "1"             ,
    screen_year                   = 1               ,
    date_death1                   = "2040-01-01"    ,
    date_death1_issdt             = 31              ,
    date_exit_uk1                 = "2050-01-01"    ,
    date_exit_uk1_issdt           = 41              ,
    notif_issdt.years = NA              ,
    all_tb_issdt                  = NA              ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = 1               ,
    num_2nd_inf                   = 0
  )


# scenario 9 --------------------------------------------------------------
# LTBI, progression risk 100%, doesn't accept testing
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
    notif_issdt.years = 2               ,
    all_tb_issdt                  = 2               ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = 1               ,
    num_2nd_inf                   = 0
  )

# scenario 10 --------------------------------------------------------------
# LTBI, progression 100%, accepts testing, doesn't start treatment
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
    notif_issdt.years = 2               ,
    all_tb_issdt                  = 2               ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = 1               ,
    num_2nd_inf                   = 0
  )

# scenario 11 --------------------------------------------------------------
# no LTBI, screened, treated, exit
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
    notif_issdt.years = NA              ,
    all_tb_issdt                  = NA              ,
    cfr                           = 0               ,
    tb_fatality                   = NA              ,
    QALY_statusquo                = 11              ,
    QALY_diseasefree              = 13              ,
    QALY_cured                    = 1               ,
    QALY_fatality                 = 0               ,
    uk_notif_discounts            = 1               ,
    all_notif_discounts           = 1               ,
    uk_secondary_inf_discounts    = 1               ,
    all_secondary_inf_discounts   = 1               ,
    id_avoided_tb                 = NA              ,
    num_2nd_inf                   = 0
  )
