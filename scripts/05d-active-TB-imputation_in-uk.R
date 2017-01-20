#
# project: LTBI screening
# N Green
# Dec 2016
#
# active TB imputation in the UK


library(reshape2)


###################################
## create (in uk) times to event ##
###################################

cmprsk <- data.frame(dis = 1,
                     ftime = times,
                     status = event) # without age

# not all individuals are in the risk set of progressing to active TB
# we asssume that those without LTBI will never do so
# remove non-LTBI individuals

cmprsk <- cmprsk[LTBI_status==1 | IMPUTED_sample$uk_tb_orig==1, ]


# calculate and plot
# competing risk of active TB and all-cause death
# cumulative probability
# use total cohort

attach(cmprsk)
fit = CumIncidence(ftime, status, dis, cencode = 0,
                   t = seq(0, 3650, 365), #discrete yearly values
                   xlab = "Days since arrival to UK", col = 2:4)

legend("topright", legend = c("Active TB", "Leave UK", "Death"),
       col = 2:4, lty = 1, bg = "white")
detach(cmprsk)


# include year 0
cumprob.activetb <- c(0, fit$est[1, ])
names(cumprob.activetb) <- as.character(seq_along(cumprob.activetb) - 1)


pop_year_LTBI <- sum(LTBI_status==1 & whoin_year_cohort)


##################################################
## calculate absolute number of cases each year ##
##################################################

# no-event risk set at each year

issdt_exit_year <- ceiling(IMPUTED_sample$date_exit_uk1_issdt/365)[LTBI_status==1 & whoin_year_cohort]
issdt_death_year <- ceiling(IMPUTED_sample$date_death1_issdt/365)[LTBI_status==1 & whoin_year_cohort]
issdt_uktb_year <- ceiling(IMPUTED_sample$rNotificationDate_issdt.years)[LTBI_status==1 & whoin_year_cohort]

issdt_exit_year.tab <- table(issdt_exit_year) %>% melt(varnames = "year")
issdt_death_year.tab <- table(issdt_death_year) %>% melt(varnames = "year")
issdt_uktb_year.tab <- table(issdt_uktb_year) %>% melt(varnames = "year")

subpop <- merge(issdt_exit_year.tab, issdt_death_year.tab,
                by = "year", suffixes = c(".exit", ".death"))


subpop <- mutate(subpop, event_pop = value.exit + value.death)
subpop$cumevent_pop <- cumsum(subpop$event_pop)
subpop <- mutate(subpop, atrisk_pop = pop_year_LTBI - cumevent_pop)


max_year <- 10
year_seq <- seq_len(max_year)


activetb_pop <- 0
atrisk_pop_total <- subpop$atrisk_pop

for (i in year_seq){

  # scaled-up CIF by year populations
  activetb_pop[i] <- atrisk_pop_total[i] * diff(cumprob.activetb)[i]

  # remove active tb cases from next year risk set
  atrisk_pop_total[i+1] <- atrisk_pop_total[i+1] - activetb_pop[i]

}

