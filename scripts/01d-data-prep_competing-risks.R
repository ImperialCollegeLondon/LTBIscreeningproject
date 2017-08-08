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




## orginally determine for all 10 imputation samples
## but now just do for first

# create uk_entry -> follow-up times ---------------------------------------------
# fup: Minimum of time to active TB, leaving the UK, death
#
# # find all columns with follow-up time imputations
# cols_fup <- grepl(pattern = "fup[0-9]+",
#                   x = names(IMPUTED_sample))
#
# # find all columns with either exit uk or death event time imputations
# cols_eventdate <- grepl(pattern = "date_exit_uk|date_death",
#                         x = names(IMPUTED_sample))
#
#
# # days from arrival in uk to end of follow-up
# fup_issdt <- apply(IMPUTED_sample[ ,cols_fup], 2,
#                    FUN = function(x) x - issdt.asnumeric)
#
# colnames(fup_issdt) <- paste0(colnames(fup_issdt), "_issdt")
#
#
# # days from uk arrival to death & uk exit
# event_issdt <- apply(IMPUTED_sample[ ,cols_eventdate], 2,
#                      FUN = function(y) as.Date(y, "%Y-%m-%d") - IMPUTED_sample$issdt)
#
# colnames(event_issdt) <- paste0(colnames(event_issdt), "_issdt")
#
# IMPUTED_sample <- data.frame(IMPUTED_sample,
#                              event_issdt,
#                              fup_issdt)
#
# as.Date(19723, origin = "1960-01-01")



# first event indicator (T/F) for each imputation -------------------------------
#
# IMPUTED_sample <-
#   IMPUTED_sample %>%
#   mutate(cens1  = fup1 == fup_limit,
         # cens2  = fup2 == fup_limit,
         # cens3  = fup3 == fup_limit,
         # cens4  = fup4 == fup_limit,
         # cens5  = fup5 == fup_limit,
         # cens6  = fup6 == fup_limit,
         # cens7  = fup7 == fup_limit,
         # cens8  = fup8 == fup_limit,
         # cens9  = fup9 == fup_limit,
         # cens10 = fup10 == fup_limit,

         # death1  = date_death1_issdt == fup1_issdt,
         # death2  = date_death2_issdt == fup2_issdt,
         # death3  = date_death3_issdt == fup3_issdt,
         # death4  = date_death4_issdt == fup4_issdt,
         # death5  = date_death5_issdt == fup5_issdt,
         # death6  = date_death6_issdt == fup6_issdt,
         # death7  = date_death7_issdt == fup7_issdt,
         # death8  = date_death8_issdt == fup8_issdt,
         # death9  = date_death9_issdt == fup9_issdt,
         # death10 = date_death10_issdt == fup10_issdt,

         # exit_uk1  = date_exit_uk1_issdt == fup1_issdt,
         # exit_uk2  = date_exit_uk2_issdt == fup2_issdt,
         # exit_uk3  = date_exit_uk3_issdt == fup3_issdt,
         # exit_uk4  = date_exit_uk4_issdt == fup4_issdt,
         # exit_uk5  = date_exit_uk5_issdt == fup5_issdt,
         # exit_uk6  = date_exit_uk6_issdt == fup6_issdt,
         # exit_uk7  = date_exit_uk7_issdt == fup7_issdt,
         # exit_uk8  = date_exit_uk8_issdt == fup8_issdt,
         # exit_uk9  = date_exit_uk9_issdt == fup9_issdt,
         # exit_uk10 = date_exit_uk10_issdt == fup10_issdt
  # )





