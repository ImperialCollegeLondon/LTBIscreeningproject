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

##TODO##
# why are there missing issdt uk entry dates?
# for now just remove them...
IMPUTED_sample <- subset(IMPUTED_sample, !is.na(issdt))

# eligible screening age range only
IMPUTED_sample <- subset(IMPUTED_sample, age_at_entry%in%screen_age_range)



# impute LTBI probability from country of origin  --------------------------

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



# from event dates create time-to-events in days --------------------------

# find all columns with follow-up time imputations
cols_fup <- grepl(pattern = "fup", x = names(IMPUTED_sample))

# find all columns with either exit uk or death event time imputations
cols_eventdate <- grepl(pattern = "date_exit_uk|date_death", x = names(IMPUTED_sample))

IMPUTED_sample$issdt <- as.Date(IMPUTED_sample$issdt, '%Y-%m-%d')

# days to arrival in uk from time origin
issdt.asnumeric <- IMPUTED_sample$issdt - as.Date("1960-01-01")

# days from arrival in uk to end of follow-up
issdt_fup <- apply(IMPUTED_sample[ ,cols_fup], 2, FUN = function(x) x - issdt.asnumeric)
colnames(issdt_fup) <- paste(colnames(issdt_fup), "_issdt", sep = "")

# days from uk arrival to death & uk exit
issdt_event <- apply(IMPUTED_sample[ ,cols_eventdate], 2, FUN = function(y) as.Date(y, "%Y-%m-%d") - IMPUTED_sample$issdt)
colnames(issdt_event) <- paste(colnames(issdt_event), "_issdt", sep = "")

# days from uk entry to active tb
rNotificationDate.asnumeric <- as.Date(IMPUTED_sample$rNotificationDate) - as.Date("1960-01-01")
rNotificationDate_issdt <- rNotificationDate.asnumeric - issdt.asnumeric


IMPUTED_sample <- data.frame(IMPUTED_sample,
                             issdt_fup, issdt_event,
                             rNotificationDate_issdt)


cr.colnames <- c(colnames(issdt_fup), colnames(issdt_event),
                 "rNotificationDate_issdt", "uk_tb", "issdt", "age_at_entry", "LTBI")

# LTBI cases only
IMPUTED_LTBI <- IMPUTED_sample[IMPUTED_sample$LTBI, cr.colnames]

rm(cols_eventdate, cols_fup, issdt.asnumeric, issdt_fup, issdt_event,
   rNotificationDate.asnumeric, rNotificationDate_issdt, cr.colnames,
   screen_age_range)


# create event-type indicators --------------------------------------------

##TODO##
# tidy this up! prone to typos

IMPUTED_sample <- transform(IMPUTED_sample,
                            death1 = (date_death1<=date_exit_uk1 & uk_tb==0),
                            death2 = (date_death2<=date_exit_uk2 & uk_tb==0),
                            death3 = (date_death3<=date_exit_uk3 & uk_tb==0),
                            death4 = (date_death4<=date_exit_uk4 & uk_tb==0),
                            death5 = (date_death5<=date_exit_uk5 & uk_tb==0),
                            death6 = (date_death6<=date_exit_uk6 & uk_tb==0),
                            death7 = (date_death7<=date_exit_uk7 & uk_tb==0),
                            death8 = (date_death8<=date_exit_uk8 & uk_tb==0),
                            death9 = (date_death9<=date_exit_uk9 & uk_tb==0),
                            death10 = (date_death10<=date_exit_uk10 & uk_tb==0),
                            exit_uk1 = (date_death1>date_exit_uk1 & uk_tb==0),
                            exit_uk2 = (date_death2>date_exit_uk2 & uk_tb==0),
                            exit_uk3 = (date_death3>date_exit_uk3 & uk_tb==0),
                            exit_uk4 = (date_death4>date_exit_uk4 & uk_tb==0),
                            exit_uk5 = (date_death5>date_exit_uk5 & uk_tb==0),
                            exit_uk6 = (date_death6>date_exit_uk6 & uk_tb==0),
                            exit_uk7 = (date_death7>date_exit_uk7 & uk_tb==0),
                            exit_uk8 = (date_death8>date_exit_uk8 & uk_tb==0),
                            exit_uk9 = (date_death9>date_exit_uk9 & uk_tb==0),
                            exit_uk10 = (date_death10>date_exit_uk10 & uk_tb==0))


# yearly entry cohort size by age and prevalence ---------------------------

# extract year only
IMPUTED_sample$issdt_year <- format(IMPUTED_sample$issdt, '%Y')

IMPUTED_sample_splityear <- split(IMPUTED_sample, IMPUTED_sample$issdt_year)

# age and active TB prevalence WHO classification tables split for each year cohort
entryCohort_age <- lapply(IMPUTED_sample_splityear, function(x) table(x$age_at_entry))
entryCohort_who <- lapply(IMPUTED_sample_splityear, function(x) table(x$who_prev_cat_Pareek2011))
entryCohort_who_prop <- lapply(IMPUTED_sample_splityear, function(x) prop.table(table(x$who_prev_cat_Pareek2011)))
entryCohort_age_who  <- lapply(IMPUTED_sample_splityear, function(x) table(x$who_prev_cat_Pareek2011, x$age_at_entry))

# total sample sizes for each yearly cohort
entryCohort_poptotal <- aggregate(rep(1, nrow(IMPUTED_sample)), by = list(IMPUTED_sample$issdt_year), sum)
names(entryCohort_poptotal) <- c("year", "pop")

