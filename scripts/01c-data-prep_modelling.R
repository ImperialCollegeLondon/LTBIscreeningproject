#***********************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# pre-process imputed dataset, from Aldridge (2016) Lancet


set.seed(23456)

IMPUTED_sample <- IMPUTED_IOM_ETS_WHO_merged_15_2_9
rm(IMPUTED_IOM_ETS_WHO_merged_15_2_9)


# remove duplicate and not needed records ---------------------------------


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



# assume that no-one leaves EWNI ------------------------------------------
## exit date after latest death

if (force_everyone_stays) {
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

pLatentTB.who_18to45 <- matrix(data = pLatentTB.who,
                               ncol = 28,
                               nrow = length(pLatentTB.who))

pLatentTB.who_age <- data.frame(levels(IMPUTED_sample$who_prev_cat_Pareek2011),
                                pLatentTB.who_18to45)


colnames(pLatentTB.who_age) <- c("who_prev_cat_Pareek2011", as.character(screen_age_range))


withr::with_options(list(warn = -1),
                    rm(pLatentTB.who_18to35,
                       pLatentTB.who_36to45,
                       pLatentTB.who_18to45))

# join with main data set

pLatentTB.who_age.long <- reshape2:::melt.data.frame(data = pLatentTB.who_age,
                                                     id.vars = "who_prev_cat_Pareek2011",
                                                     value.name = "pLTBI",
                                                     variable.name = "age_at_entry")

IMPUTED_sample <- merge(x = IMPUTED_sample,
                        y = pLatentTB.who_age.long,
                        by = c("age_at_entry",
                               "who_prev_cat_Pareek2011"))

# sample LTBI status
IMPUTED_sample$LTBI <- sample_tb(prob = 1 - IMPUTED_sample$pLTBI)


# create time-to-events in days --------------------------
# from uk entry to event dates

date_origin <- as.Date("1960-01-01")

IMPUTED_sample$issdt <- as.Date(IMPUTED_sample$issdt, '%Y-%m-%d')

# days to arrival in uk from time origin
issdt.asnumeric <- IMPUTED_sample$issdt - date_origin


# days from uk entry to active tb
rNotificationDate.asnumeric <- as.Date(IMPUTED_sample$rNotificationDate) - date_origin
rNotificationDate_issdt <- rNotificationDate.asnumeric - issdt.asnumeric
rNotificationDate_issdt.years <- as.numeric(rNotificationDate_issdt)/365


# days from uk entry to all-cause death
date_death1.asnumeric <- as.Date(IMPUTED_sample$date_death1) - date_origin
date_death1_issdt <- date_death1.asnumeric - issdt.asnumeric
date_death1_issdt.years <- as.numeric(date_death1_issdt)/365


# days from uk entry to exit uk
date_exit_uk1.asnumeric <- as.Date(IMPUTED_sample$date_exit_uk1) - date_origin
date_exit_uk1_issdt <- date_exit_uk1.asnumeric - issdt.asnumeric

date_exit_uk1_issdt[date_exit_uk1_issdt == 36525] <- Inf  #never exit imputed 100 years

date_exit_uk1_issdt.years <- as.numeric(date_exit_uk1_issdt)/365


IMPUTED_sample <- data.frame(IMPUTED_sample,
                             rNotificationDate_issdt,
                             rNotificationDate_issdt.years,
                             date_death1_issdt,
                             date_death1_issdt.years,
                             date_exit_uk1_issdt,
                             date_exit_uk1_issdt.years)

rm(issdt.asnumeric,
   rNotificationDate.asnumeric,
   rNotificationDate_issdt,
   date_death1_issdt,
   date_death1.asnumeric,
   date_death1_issdt.years,
   date_exit_uk1_issdt,
   date_exit_uk1.asnumeric,
   date_exit_uk1_issdt.years)


##TODO: could use a more realistic distn
IMPUTED_sample$screen_year <- runif(n = nrow(IMPUTED_sample))*MAX_SCREEN_DELAY


# create misc variables ---------------------------------------------------

# keep original TB status
IMPUTED_sample$uk_tb_orig <- IMPUTED_sample$uk_tb

# active TB case fatality rate age groups

IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(age_uk_notification = age_at_entry + rNotificationDate_issdt.years,
         agegroup_uk_notification = cut(age_uk_notification,
                                        breaks = cfr_age_breaks,
                                        right = FALSE))

# extract uk entry year only
IMPUTED_sample$issdt_year <- format(IMPUTED_sample$issdt, '%Y')


# uk entry to follow-up days -------------------------------------------------

fup_limit <- 19723  #days from 1960-01-01 to 2013-12-31

IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(issdt.asnumeric = issdt - date_origin,
         fup_limit_issdt = fup_limit - issdt.asnumeric,
         fup_issdt_days = fup1 - issdt.asnumeric,
         fup_issdt = days_to_years(fup_issdt_days),
         LTBI_or_activeTB = LTBI == 1 | uk_tb_orig == 1,
         cens1 = fup1 == fup_limit,
         death1 = date_death1_issdt == fup_issdt_days,
         exit_uk1 = date_exit_uk1_issdt == fup_issdt_days)


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


