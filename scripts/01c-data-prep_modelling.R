#***********************************************************************
# project: LTBI screening
# N Green
# Oct 2016
# 01c-data-prep_modelling.R
#
# pre-process imputed dataset, from Aldridge (2016) Lancet


set.seed(23456)

IMPUTED_sample <- IMPUTED_IOM_ETS_WHO_merged_15_2_9


# remove duplicate and not needed ---------------------------------

rm_variables_names <-
  c("firstcdt","exdt", "sex","dob","cxr","cxrdt","cxrint","cxrintdt","ssdnd","dta1","dta2","dta3",
    "dtb1","dtb2","cresb2","dtb3","stres","iomgdln","ssspunob","ssnormcxr","sscxntb","ssappdec",
    "famcont","cert","expdt","birthyr","ager", "cxrndr","cxrukres","examyr","exammth","dst2", "ssndr","consp","nocert",
    "active","lab","clinical","ssres","scres","protocol","rclientsourceid","weight","SourceID","threshold",
    "uk_caserepdate","uk_sitepulm","uk_microscopy","uk_pcr","uk_etsculture","uk_culture",
    "uk_sputsmear","uk_origsputsmear","uk_anyres","uk_allfirstres","uk_secondres",
    "uk_prevdiag","uk_bcgvacc","uk_anyriskfactor","uk_typed_loci","uk_atleast23",
    "uk_clusteranalysis","uk_clusterno1_analysis","uk_clustered","uk_cluster_size","uk_clustersize_n",
    "uk_clusterno1_analysis_24","male","active_pulm","any_culture","culture_pos","smear_pos",
    "TB_IOM","lab_new","sputum_taken","time_to_cert","xray_new","clustered2", "first_screen",
    "last_screen","time_first_last_screen","first_screen_TB","last_screen_TB",
    "max_weight","keep_ETS_dup", "min_exdt", "next_screen","days_between_screens","cohort_end_dups",
    "uk_pulm_culture","uk_pulm_smear","uk_pulm_resist","uk_extpulm_culture","uk_extrapulm_smear",
    "uk_extrapulm_resist","cohort_end","cohort_end_visa","cohort_end_1yr","uk_anyriskfactor_risk",
    "uk_bcgvacc_risk","uk_prevdiag_risk","uk_delayindiag_cat","uk_clustersize_cat","uk_clustersize_n_cat",
    "uk_clusterno1_analysis_24_unique", "uk_max_loci_pre","uk_cluster_size_pre","uk_clustered_pre","uk_clustered_mi",
    "uk_cluster_trans","uk_cluster_react","uk_first_in_cluster", "prop",
    "dup_less365_10n", "dup_ref_id", "duplicates_refid", "duplicates_refid_dob", "ref_id_orig",
    "prevalent_120", "prevalent_150", "prevalent_180", "prevalent_30", "prevalent_365", "prevalent_60", "prevalent_90")

IMPUTED_sample <- IMPUTED_sample[ ,!names(IMPUTED_sample) %in% rm_variables_names]

# remove duplicate pre-entry screened
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                dup_ref_id_orig == 0)

# remove duplicate notification dates
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                is.na(rNotificationDate) | uk_tb == 1)

##TODO: why are there missing issdt uk entry dates?
# for now just remove them...
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                !is.na(issdt))

# remove death before entry to UK
##TODO: are deaths before entry the same for all imputation samples?
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                date_death1 >= issdt)

if (interv$force_everyone_stays) {
    IMPUTED_sample$date_exit_uk1 <- max(IMPUTED_sample$date_death1, na.rm = TRUE) + 100
}


# screening delay ---------------------------------------------------------

##TODO: could use a more realistic distn
IMPUTED_sample$screen_year <- runif(n = nrow(IMPUTED_sample))*MAX_SCREEN_DELAY

IMPUTED_sample$age_at_screen <- IMPUTED_sample$age_at_entry + round(IMPUTED_sample$screen_year)

# remove
# eligible screening age range only
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                age_at_screen %in% interv$screen_age_range)

# extract uk entry year only
IMPUTED_sample$issdt_year <- format(IMPUTED_sample$issdt, '%Y')
##TODO: why are these are different to $year??


# LTBI probs by WHO active TB group ---------------------------------

who_levels <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]")
who_level_breaks <- c(0, 50, 150, 250, 350, 100000)

# match active TB prevalence groups in dataset to Pareek (2011)
IMPUTED_sample$who_prev_cat_Pareek2011 <- cut(IMPUTED_sample$who_prevalence,
                                              breaks = who_level_breaks)

IMPUTED_sample$who_prev_cat_Aldridge2016 <- cut(IMPUTED_sample$who_prevalence,
                                                breaks = c(0, 39, 149, 349, 100000))

data("TB_burden_countries")

IMPUTED_sample <-
  merge(x = IMPUTED_sample,
        y = TB_burden_countries,
        by.x = c('iso_a3_country', 'year'),#issdt_year'),
        by.y = c('iso3', 'year'))


IMPUTED_sample$who_inc_Pareek2011 <- cut(IMPUTED_sample$e_inc_100k,
                                         breaks = who_level_breaks)

### assume >35 == 35 year olds ###
# i.e. age independent
##TODO: can be age-dependent

pLatentTB.who_age <-
  matrix(data = pLatentTB.who,
         ncol = length(interv$screen_age_range),
         nrow = length(pLatentTB.who)) %>%
  data.frame(who_levels, .) %>%
  purrr::set_names("who_inc_Pareek2011",
                   as.character(interv$screen_age_range))

# join with main data set
pLatentTB.who_age.long <-
  reshape2:::melt.data.frame(data = pLatentTB.who_age,
                             id.vars = "who_inc_Pareek2011",
                             value.name = "pLTBI",
                             variable.name = "age_at_screen")

IMPUTED_sample <- merge(x = IMPUTED_sample,
                        y = pLatentTB.who_age.long,
                        by = c("age_at_screen",
                               "who_inc_Pareek2011"))

##TODO:
## why are there multiple prevalence values for each country/year group??
# table(IMPUTED_sample$who_prevalence[IMPUTED_sample$year == 2009],
#       IMPUTED_sample$iso_a3_country[IMPUTED_sample$year == 2009])


##############################################################
## TODO:
## testing code...
## over-write sample with same fixed value for everyone
## do this to control for the effect of time in EWNI vs pLTBI
## run model targetting different who groups
#
# IMPUTED_sample$pLTBI <- 0.3
##############################################################


IMPUTED_sample$LTBI <- sample_tb(prob = 1 - IMPUTED_sample$pLTBI)
IMPUTED_sample$LTBI[as.logical(IMPUTED_sample$uk_tb)] <- TRUE

##TODO: this over-samples LTBI
# either remove observed tb beforehand or adjust downwards

IMPUTED_sample$uk_tb_orig <- IMPUTED_sample$uk_tb


# create time-to-events -------------------------
# from uk entry to event dates

IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(date_death1_issdt = date_death1 - issdt,
         date_exit_uk1_issdt = date_exit_uk1 - issdt,
         date_death1_issdt.years = as.numeric(date_death1_issdt)/365.25,
         date_exit_uk1_issdt.years = as.numeric(date_exit_uk1_issdt)/365.25,
         date_exit_uk1_issdt = ifelse(date_exit_uk1_issdt.years == 100,
                                      yes = Inf,
                                      no = date_exit_uk1_issdt),
         date_exit_uk1_issdt.years = ifelse(date_exit_uk1_issdt.years == 100,
                                            yes = Inf,
                                            no = date_exit_uk1_issdt.years))


# uk entry to follow-up days -------------------------------------------------

FUP_DATE <- as.Date("2013-12-31")

# days from 1960-01-01 to 2013-12-31
date_origin <- as.Date("1960-01-01") #from STATA
fup_limit <- FUP_DATE - date_origin

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(fup_limit_issdt_days = FUP_DATE - issdt,
                fup_limit_issdt = fup_limit_issdt_days/365.25,
                fup_date = as.Date(fup1, origin = date_origin),
                fup_issdt_days = fup_date - issdt,
                fup_issdt = fup_issdt_days/365.25,
                cens1 = fup_date == FUP_DATE,
                death1 = fup_date == date_death1,
                exit_uk1 = fup_date == date_exit_uk1)

# remove indiv follow-up date before entry
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                fup_date > issdt)

# remove tb before entry
IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(rNotificationDate_issdt = rNotificationDate - issdt,
                uk_tb = ifelse(rNotificationDate_issdt < 0 | is.na(rNotificationDate_issdt),
                               yes = 0,
                               no = 1),
                rNotificationDate_issdt = ifelse(uk_tb,
                                                 yes = rNotificationDate_issdt,
                                                 no = NA),
                rNotificationDate_issdt.years = as.numeric(rNotificationDate_issdt)/365.25)


save(IMPUTED_sample, file = "data/sample_cleaned.RData")

