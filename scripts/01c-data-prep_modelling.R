#***********************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# pre-process imputed dataset, from Aldridge (2016) Lancet


set.seed(23456)

IMPUTED_sample <- IMPUTED_IOM_ETS_WHO_merged_15_2_9
rm(IMPUTED_IOM_ETS_WHO_merged_15_2_9)


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
    "uk_cluster_trans","uk_cluster_react","uk_first_in_cluster", "prop")

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

# eligible screening age range only
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                age_at_entry %in% screen_age_range)


if (force_everyone_stays) {
##TODO: is this doing what we want?
    IMPUTED_sample$date_exit_uk1 <- max(IMPUTED_sample$date_death1, na.rm = TRUE) + 100
}


# create LTBI probs by WHO active TB group ---------------------------------

who_levels <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]")

# match active TB prevalence groups in dataset to Pareek (2011)
IMPUTED_sample$who_prev_cat_Pareek2011 <- cut(IMPUTED_sample$who_prevalence,
                                              breaks = c(0, 50, 150, 250, 350, 100000))

# ref. Pareek M et al. Lancet Infect Dis. Elsevier Ltd; 2011;11(6)
# ages 18-35 pooled
pLatentTB.who <- c(0.03, 0.13, 0.2, 0.3, 0.3) %>% setNames(who_levels)

### assume >35 == 35 year olds ###
# i.e. age independent

pLatentTB.who_age <-
  matrix(data = pLatentTB.who,
         ncol = 28,
         nrow = length(pLatentTB.who)) %>%
  data.frame(who_levels, .) %>%
  set_names("who_prev_cat_Pareek2011", as.character(screen_age_range))

# join with main data set
pLatentTB.who_age.long <- reshape2:::melt.data.frame(data = pLatentTB.who_age,
                                                     id.vars = "who_prev_cat_Pareek2011",
                                                     value.name = "pLTBI",
                                                     variable.name = "age_at_entry")

IMPUTED_sample <- merge(x = IMPUTED_sample,
                        y = pLatentTB.who_age.long,
                        by = c("age_at_entry",
                               "who_prev_cat_Pareek2011"))

IMPUTED_sample$LTBI <- sample_tb(prob = 1 - IMPUTED_sample$pLTBI)

##TODO: could use a more realistic distn
IMPUTED_sample$screen_year <- runif(n = nrow(IMPUTED_sample))*MAX_SCREEN_DELAY

IMPUTED_sample$uk_tb_orig <- IMPUTED_sample$uk_tb

# extract uk entry year only
IMPUTED_sample$issdt_year <- format(IMPUTED_sample$issdt, '%Y')


# create time-to-events -------------------------
# from uk entry to event dates

IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(date_death1_issdt = date_death1 - issdt,
         date_exit_uk1_issdt = date_exit_uk1 - issdt,
         date_death1_issdt.years = as.numeric(date_death1_issdt)/365.25,
         date_exit_uk1_issdt.years = as.numeric(date_exit_uk1_issdt)/365.25,
         date_exit_uk1_issdt = ifelse(date_exit_uk1_issdt.years == 100, Inf, date_exit_uk1_issdt),
         date_exit_uk1_issdt.years = ifelse(date_exit_uk1_issdt.years == 100, Inf, date_exit_uk1_issdt.years))


# uk entry to follow-up days -------------------------------------------------

FUP_DATE <- as.Date("2013-12-31")

# days from 1960-01-01 to 2013-12-31
date_origin <- as.Date("1960-01-01") #STATA
fup_limit <- FUP_DATE - date_origin

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(fup_limit_issdt_days = FUP_DATE - issdt,
                fup_limit_issdt = fup_limit_issdt_days/365.25,
                fup1_date = as.Date(fup1, origin = date_origin),
                fup_issdt_days = fup1_date - issdt,
                fup_issdt = fup_issdt_days/365.25,
                LTBI_or_activeTB = LTBI == 1 | uk_tb_orig == 1,
                cens1 = fup1_date == FUP_DATE,
                death1 = fup1_date == date_death1,
                exit_uk1 = fup1_date == date_exit_uk1)

# remove indiv follow-up date before entry
IMPUTED_sample <- dplyr::filter(IMPUTED_sample,
                                fup1_date > issdt)

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


# mdr ---------------------------------------------------------------------

# MDR_burden <- readr::read_csv("C:/Users/ngreen1/Dropbox/TB/LTBI/data/WHO/MDR_RR_TB_burden_estimates_2017-05-30.csv")
#
# sample_mdr <- left_join(IMPUTED_sample_year_cohort[,c("iso_a3_nat","iso_a3_country")],
#                 MDR_burden[,c("iso3","e_rr_pct_new")],
#                 by = c("iso_a3_country" = "iso3"))
#
# mdr_pct <- mean(sample_mdr$e_rr_pct_new)

# total sample size
n.pop_screen <- nrow(IMPUTED_sample)

# total sample sizes for each yearly cohort
n.popyear_screen <-
  aggregate(x = rep(1, n.pop_screen),
            by = list(IMPUTED_sample$issdt_year),
            sum) %>%
  set_names(c("year", "pop"))

