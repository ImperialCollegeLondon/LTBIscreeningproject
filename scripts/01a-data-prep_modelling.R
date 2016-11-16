#
# project: LTBI screening
# N Green
# Oct 2016
#
# impute LTBI status given country of origin
# define entry cohort in terms of LTBI prevalence and size by year



# global constants --------------------------------------------------------

screen_age_range <- 18:35
# screen_age_range <- 18:45

# year_cohort <- '2012' #latest year
year_cohort <- '2012' #largest population

# Pareek M, Watson JP, Ormerod LP, Kon OM, Woltmann G, White PJ, et al. Lancet Infect Dis. Elsevier Ltd; 2011;11(6)
# ages 18-35
# <50/100,000: 3% LTBI
# 51-150/100,000: 13% LTBI
pLatentTB.who <- c(0.03, 0.13, 0.2, 0.3, 0.3)


###############
# pre-process #
###############

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



# create time-to-events in days --------------------------
# from uk entry to event dates

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
issdt_event <- apply(IMPUTED_sample[ ,cols_eventdate], 2,
                     FUN = function(y) as.Date(y, "%Y-%m-%d") - IMPUTED_sample$issdt)
colnames(issdt_event) <- paste(colnames(issdt_event), "_issdt", sep = "")

# days from uk entry to active tb
rNotificationDate.asnumeric <- as.Date(IMPUTED_sample$rNotificationDate) - as.Date("1960-01-01")
rNotificationDate_issdt <- rNotificationDate.asnumeric - issdt.asnumeric
rNotificationDate_issdt.years <- as.numeric(IMPUTED_sample$rNotificationDate_issdt)/365


IMPUTED_sample <- data.frame(IMPUTED_sample,
                             issdt_fup, issdt_event,
                             rNotificationDate_issdt)


rm(cols_eventdate, cols_fup,
   issdt.asnumeric, issdt_fup, issdt_event,
   rNotificationDate.asnumeric, rNotificationDate_issdt,
   screen_age_range)


# create event-type indicators --------------------------------------------

##TODO##
is.death <- function(imputation_num, data,
                     fup_limit = 19723){

  date_deathX <- paste("date_death", imputation_num, sep="")
  date_exit_ukX <- paste("date_exit_uk", imputation_num, sep="")
  fupX <- paste("fup", imputation_num, sep="")

  return(data[ ,date_deathX] <= data[ ,date_exit_ukX] &
         data$uk_tb==0 &
         data[ ,fupX]!=fup_limit)
}

# is.exit_uk
# is.fup_limit


fup_limit <- 19723  #days from 1960-01-01

##TODO##
# tidy this up! prone to typos
IMPUTED_sample <- transform(IMPUTED_sample,

                            cens1  = fup1==fup_limit,
                            cens2  = fup2==fup_limit,
                            cens3  = fup3==fup_limit,
                            cens4  = fup4==fup_limit,
                            cens5  = fup5==fup_limit,
                            cens6  = fup6==fup_limit,
                            cens7  = fup7==fup_limit,
                            cens8  = fup8==fup_limit,
                            cens9  = fup9==fup_limit,
                            cens10 = fup10==fup_limit,

                            death1  = (date_death1<=date_exit_uk1 & uk_tb==0 & fup1!=fup_limit), #is.death(1, IMPUTED_sample)
                            death2  = (date_death2<=date_exit_uk2 & uk_tb==0 & fup2!=fup_limit), #is.death(2, IMPUTED_sample)
                            death3  = (date_death3<=date_exit_uk3 & uk_tb==0 & fup3!=fup_limit),
                            death4  = (date_death4<=date_exit_uk4 & uk_tb==0 & fup4!=fup_limit),
                            death5  = (date_death5<=date_exit_uk5 & uk_tb==0 & fup5!=fup_limit),
                            death6  = (date_death6<=date_exit_uk6 & uk_tb==0 & fup6!=fup_limit),
                            death7  = (date_death7<=date_exit_uk7 & uk_tb==0 & fup7!=fup_limit),
                            death8  = (date_death8<=date_exit_uk8 & uk_tb==0 & fup8!=fup_limit),
                            death9  = (date_death9<=date_exit_uk9 & uk_tb==0 & fup9!=fup_limit),
                            death10 = (date_death10<=date_exit_uk10 & uk_tb==0 & fup10!=fup_limit),

                            exit_uk1  = (date_death1>date_exit_uk1 & uk_tb==0 & fup1!=fup_limit),
                            exit_uk2  = (date_death2>date_exit_uk2 & uk_tb==0 & fup2!=fup_limit),
                            exit_uk3  = (date_death3>date_exit_uk3 & uk_tb==0 & fup3!=fup_limit),
                            exit_uk4  = (date_death4>date_exit_uk4 & uk_tb==0 & fup4!=fup_limit),
                            exit_uk5  = (date_death5>date_exit_uk5 & uk_tb==0 & fup5!=fup_limit),
                            exit_uk6  = (date_death6>date_exit_uk6 & uk_tb==0 & fup6!=fup_limit),
                            exit_uk7  = (date_death7>date_exit_uk7 & uk_tb==0 & fup7!=fup_limit),
                            exit_uk8  = (date_death8>date_exit_uk8 & uk_tb==0 & fup8!=fup_limit),
                            exit_uk9  = (date_death9>date_exit_uk9 & uk_tb==0 & fup9!=fup_limit),
                            exit_uk10 = (date_death10>date_exit_uk10 & uk_tb==0 & fup10!=fup_limit))


# yearly entry cohort size by age and prevalence ---------------------------

# extract year only
IMPUTED_sample$issdt_year <- format(IMPUTED_sample$issdt, '%Y')

IMPUTED_sample_splityear <- split(IMPUTED_sample, IMPUTED_sample$issdt_year)

# single year sample
IMPUTED_sample_year_cohort <- IMPUTED_sample_splityear[[year_cohort]]

# age and active TB prevalence WHO classification tables split for each year cohort
entryCohort_age <- lapply(IMPUTED_sample_splityear, function(x) table(x$age_at_entry))
entryCohort_who <- lapply(IMPUTED_sample_splityear, function(x) table(x$who_prev_cat_Pareek2011))
entryCohort_who_prop <- lapply(IMPUTED_sample_splityear, function(x) prop.table(table(x$who_prev_cat_Pareek2011)))
entryCohort_age_who  <- lapply(IMPUTED_sample_splityear, function(x) table(x$who_prev_cat_Pareek2011, x$age_at_entry))

# total sample size
n.pop <- nrow(IMPUTED_sample)

# total sample sizes for each yearly cohort
entryCohort_poptotal <- aggregate(rep(1, n.pop),
                                  by = list(IMPUTED_sample$issdt_year), sum)
names(entryCohort_poptotal) <- c("year", "pop")


# keep pre-screened status
IMPUTED_sample$uk_tb_orig <- IMPUTED_sample$uk_tb

# active TB case fatality rate age groups
IMPUTED_sample$cfr_age_groups <- cut(IMPUTED_sample$age_at_entry + rNotificationDate_issdt.years,
                                     breaks = c(15, 45, 65, 200),
                                     right = FALSE)

age_at_fup <- IMPUTED_sample$age_at_entry + floor(IMPUTED_sample$fup1_issdt/365)

# logical active TB status of original data
uk_tb_TRUE <- IMPUTED_sample$uk_tb==1
sample.uk_tb_only <- IMPUTED_sample[uk_tb_TRUE, ]


uk_tb_only.notification_to_allcause_death <- with(sample.uk_tb_only,
                                                  floor((date_death1_issdt - rNotificationDate_issdt)/365))

##TODO##
# why are some of these -ve?

# for purpose of calculating QALYs
uk_tb_only.notification_to_allcause_death[uk_tb_only.notification_to_allcause_death<0] <- 0


# summary statistics ------------------------------------------------------

# cohort size at arrival to uk
n.pop <- sum(entryCohort_poptotal$pop)
pop_year <- with(entryCohort_poptotal, pop[year==year_cohort])

# number of active TB cases _before_ screening
n.tb <- sum(IMPUTED_sample$uk_tb)
n.tb_year <- sum(IMPUTED_sample_year_cohort$uk_tb)

p.who <- entryCohort_who_prop[[year_cohort]]
who_levels <- names(p.who)


