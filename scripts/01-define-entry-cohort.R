#
# project: LTBI screening
# N Green
# Oct 2016
#
# impute LTBI status given country of origin
# define entry cohort in terms of LTBI prevalence and size by year



# global constants --------------------------------------------------------

screen_age_range <- 18:35
year_cohort <- '2012'

# eligible screening age range only
IMPUTED_sample <- subset(IMPUTED_sample, age_at_entry%in%screen_age_range)


# LTBI probability from country of origin  --------------------------------

# match prevalence groups in dataset to paper
IMPUTED_sample$who_prev_cat_Pareek2011 <- cut(IMPUTED_sample$who_prevalence,
                                              breaks = c(0, 50, 150, 250, 350, 100000))

# Pareek M, Watson JP, Ormerod LP, Kon OM, Woltmann G, White PJ, et al. Lancet Infect Dis. Elsevier Ltd; 2011;11(6)
# ages 18-35
# <50/100,000: 3% LTBI
# 51-150/100,000: 13% LTBI
pLatentTB.who <- c(0.03, 0.13, 0.2, 0.3, 0.3)

# prevalance group frequencies in data
tab.who <- table(IMPUTED_sample$who_prev_cat_Pareek2011)
tab.who_uktb <- table(IMPUTED_sample$who_prev_cat_Pareek2011, IMPUTED_sample$uk_tb)

# adjusted probability of LTBI for non-active TB given observed active TB cases
# p(LTBI|not active TB) = (N * p(LTBI) - #(active TB))/#(not active TB)
pLatentTB.who_adjusted <- (tab.who * pLatentTB.who - tab.who_uktb[ ,"1"])/tab.who_uktb[ ,"0"]

# probability of LTBI for each non-active TB case
prob <- with(IMPUTED_sample, pLatentTB.who_adjusted[who_prev_cat_Pareek2011])

# sample LTBI status
IMPUTED_sample$LTBI <- (runif(n = length(prob)) < prob)

# if active TB then assume LTBI
IMPUTED_sample$LTBI[IMPUTED_sample$uk_tb=="1"] <- TRUE

rm(pLatentTB.who, pLatentTB.who_adjusted, prob)



# convert follow-up columns to date format ---------------------------------

cols_fup <- grepl(pattern = "_fup", x = names(IMPUTED_sample))

# date of arrival to uk
issdt.asnumeric <- as.Date(IMPUTED_sample$issdt) - as.Date("1960-01-01")

x <- apply(IMPUTED_sample[ ,cols_fup], 2, FUN = function(x) x - issdt.asnumeric)
colnames(x) <- paste(colnames(x), "_issdt", sep="")


# time from uk entry to active tb
rNotificationDate.asnumeric <- as.Date(IMPUTED_sample$rNotificationDate) - as.Date("1960-01-01")
rNotificationDate_issdt <- rNotificationDate.asnumeric - issdt.asnumeric


IMPUTED_sample <- data.frame(IMPUTED_sample, x, rNotificationDate.asnumeric, rNotificationDate_issdt)


##TODO##
# use these times from modified STATA code...
# cr.colnames <- c("date_exit_uk", "date_death", "rNotificationDate", "issdt", "age_at_entry", "LTBI")

cr.colnames <- c("X_9_fup", "length_uk_stay", "time_screen_case", "uk_tb", "rNotificationDate", "issdt", "age_at_entry", "LTBI")

IMPUTED_LTBI <- IMPUTED_sample[IMPUTED_sample$LTBI, cr.colnames]



# yearly entry cohort size by age and prevalence ---------------------------

# extract year only
tmp <- as.Date(IMPUTED_sample$issdt, '%Y-%m-%d')
IMPUTED_sample$issdt_year <- format(tmp, '%Y')
rm(tmp)

# age and active TB prevalence WHO classification tables split for each year cohort
entryCohort_age <- lapply(IMPUTED_sample_splityear, function(x) table(x$age_at_entry))
entryCohort_who <- lapply(IMPUTED_sample_splityear, function(x) table(x$who_prev_cat_Pareek2011))
entryCohort_who_prop <- lapply(IMPUTED_sample_splityear, function(x) prop.table(table(x$who_prev_cat_Pareek2011)))
entryCohort_age_who  <- lapply(IMPUTED_sample_splityear, function(x) table(x$who_prev_cat_Pareek2011, x$age_at_entry))

# total sample sizes for each yearly cohort
entryCohort_poptotal <- aggregate(rep(1, nrow(IMPUTED_sample)), by = list(IMPUTED_sample$issdt_year), sum)
names(entryCohort_poptotal) <- c("year", "pop")

