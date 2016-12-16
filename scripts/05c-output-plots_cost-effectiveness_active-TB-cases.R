#
# project: LTBI screening
# N Green
# Dec 2016
#
# proportion of LTBI become active cases
# with screening for each scenario
# bar plots & box plots


library(ggplot2)


if(!exists("p.complete_treat_scenarios")){

  p.complete_treat_scenarios <- read.csv(file = file.choose(), header = FALSE)
  # p.complete_treat_scenarios <- read.csv(file = paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"), header = FALSE)
  names(p.complete_treat_scenarios) <- who_levels
  p.complete_treat_scenarios$scenario <- rownames(p.complete_treat_scenarios)
}


LTBI_prob_lookup <-
  p.complete_treat_scenarios %>%
  gather("who_prev_cat_Pareek2011", "value", - scenario)


######################
## before screening ##
######################

rNotificationDate_issdt.years <- IMPUTED_sample_year_cohort$rNotificationDate_issdt.years
rNotificationDate_issdt.years <- ceiling(rNotificationDate_issdt.years[rNotificationDate_issdt.years>=0])

activeTBcases <- table(rNotificationDate_issdt.years)


# fit exponential to (latest) observed active TB
# yearly frequencies to predict missing years

activeTBcases.log <- model.frame(formula = logy.Freq ~ year,
                                 data = data.frame(logy = log(activeTBcases)[4:5], year = 4:5))
fit <- lm(activeTBcases.log)
years <- 1:8
uktb_estimated <- exp(years*fit$coefficients["year"] + fit$coefficients["(Intercept)"])
names(uktb_estimated) <- as.character(years)


uktb_estimated <- c(rep(0, length(activeTBcases)), uktb_estimated[-(1:length(activeTBcases))])

# fillin missing years with NA
activeTBcases <- c(activeTBcases, rep(0, length(cum_year_total.diff) - length(activeTBcases)))


# fillin later years with estimates (exponential)
# activeTBcases <- c(activeTBcases, uktb_estimated)
activeTBcases <- rbind(activeTBcases,
                       uktb_estimated)
colnames(activeTBcases) <- seq_len(ncol(activeTBcases))

barplot(height = activeTBcases,
        ylim = c(0, 200),
        main = "Raw number of active TB cases",
        xlab = "Time in UK (years)", ylab = "Cases")



# including LTBI indiv who have left EWNI -----------------------------------

activeTBcases_UK_nonUK <- rbind("UK observed" = activeTBcases,
                                "Non-UK estimated" = cum_year_total.diff)
                                # "Non-UK estimated" = cum_year_total.diff[names(activeTBcases)])

x11()
barplot(height = activeTBcases_UK_nonUK,
        main = sprintf("Number of active TB cases\n in %s cohort", year_cohort),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases",
        col = c("lightgrey", "darkgrey","lightblue"),
        xlim = c(0,10), ylim = c(0, 270),
        density = c(5,60,20,30), angle = c(10,45,75,11))

legend("topright", legend = c("Observed EWNI", "Estimated EWNI", "Estimated exited EWNI"),
       density = c(5,60,20), angle = c(10,45,75), fill = c("lightgrey", "darkgrey","lightblue"), bg = "white")


#####################
## after screening ##
#####################

# e.g.
# basecase?
SCENARIO <- 300

p.completeTx <- subset(x = LTBI_prob_lookup,
                       scenario==as.character(SCENARIO) & who_prev_cat_Pareek2011=="(50,150]",
                       select = value) %>% as.numeric

activeTBcases_after_screen <- activeTBcases * (1 - p.completeTx)

barplot(height = activeTBcases_after_screen,
        main = sprintf("Raw number of active TB cases after screening in %s cohort", year_cohort),
        xlab = "Time in UK (years)", ylab = "Cases")



# including LTBI indiv who have left UK -----------------------------------

activeTBcases_UK_nonUK_after_screen <- rbind("UK observed" = activeTBcases_after_screen,
                                             "Non-UK estimated" = cum_year_total.diff * (1 - p.completeTx))
x11()
barplot(height = activeTBcases_UK_nonUK_after_screen,
        main = sprintf("Number of active TB cases after screening\n in %s cohort", year_cohort),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases",
        col = c("lightgrey", "darkgrey","lightblue"),
        xlim = c(0,10), ylim = c(0, 270),
        density = c(5,60,20,30), angle = c(10,45,75,11))

legend("topright", legend = c("Observed EWNI", "Estimated EWNI", "Estimated exited EWNI"),
       density = c(5,60,20), angle = c(10,45,75), fill = c("lightgrey", "darkgrey","lightblue"), bg = "white")


##TODO##
# binomial confidence interval normal approximation
# do we need n denominator as number of LTBI individuals?
# would the error be too small to make any difference anyway?
# p.completeTx + 1.96*p.completeTx*(1-p.completeTx)/pop_year






#####################################
## combined before/after screening ##
#####################################

# in UK only --------------------------------------------------------------

# stack missed and avoided active TB cases
missed_avoided_activeTBcases <- rbind("Missed" = activeTBcases_after_screen,
                                      "Avoided" = activeTBcases - activeTBcases_after_screen)

# single scenario bar plot
x11()
barplot(height = missed_avoided_activeTBcases,
        main = paste("Observed active TB incidence:", diroutput, "\n scenario", SCENARIO),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases",
        col = c("lightgrey", "darkgrey","lightblue","darkblue"),
        xlim = c(0,10), #ylim = c(0, 270),
        density = c(5,60,20,100), angle = c(10,45,75,11))

legend("topright", legend = c("Observed EWNI", "Estimated EWNI", "Cases avoided EWNI", "Estimated Cases avoided EWNI"),
       density = c(5,60,20,100), angle = c(10,45,75,11), fill = c("lightgrey", "darkgrey","lightblue","darkblue"), bg = "white")

# cumulative counts
# h <- hist(rNotificationDate_issdt.years, breaks = 0:5, plot = FALSE)
# h$counts <- cumsum(h$counts)
# plot(h)




# including LTBI indiv who have left UK -----------------------------------

# stack missed and avoided active TB cases
missed_avoided_UK_nonUK_activeTBcases <- rbind("Missed UK"  = activeTBcases_after_screen,
                                               "Avoided UK" = activeTBcases - activeTBcases_after_screen,
                                               "Missed non-UK"  = cum_year_total.diff * (1 - p.completeTx),
                                               "Avoided non-UK" = cum_year_total.diff * p.completeTx)

windows(rescale ="R")
barplot(height = missed_avoided_UK_nonUK_activeTBcases,
        main = sprintf("Number of active TB cases\n before and after screening in %s cohort", year_cohort),
        xlab = "Times since arrival to EWNI (years)",
        ylab = "Active TB cases in EWNI",
        col = c("lightgrey", "darkgrey","lightblue","darkblue","red","green"),
        xlim = c(0,10), #ylim = c(0, 270),
        density = c(5,60,20,100), angle = c(10,45,75,11,30,80))

legend("topright",
       legend = c("Observed missed EWNI", "Estimated missed EWNI", "Observed cases avoided EWNI", "Estimated cases avoided EWNI", "Estimated cases missed exit EWNI", "Estimated cases avoided exit EWNI"),
       density = c(5,60,20,100), angle = c(10,45,75,11,30,80), fill = c("lightgrey", "darkgrey","lightblue","darkblue","red","green"), bg = "white")


############################
## all scenarios combined ##
############################

p.completeTx_scenarios <- LTBI_prob_lookup %>%
  filter(who_prev_cat_Pareek2011=="(50,150]") %>%
  select(value)

counts.scenarios <- matrix(data = activeTBcases,
                           nrow = n.scenarios,
                           ncol = length(activeTBcases), byrow = TRUE)

for (i in seq_len(n.scenarios)) counts.scenarios[i, ] <- counts.scenarios[i, ] * p.completeTx_scenarios[i, ]


# all scenarios bar plot
counts.melt <- melt(counts.scenarios)


# ggplot2::ggplot(data = counts.melt, aes(x = X2, color = X1, group = X1)) +
#   geom_step(aes(y = value), alpha = .3) +
#   theme(legend.position = "none")
# geom_point(aes(y = value), alpha=.3)


# box plot
ggplot(data = counts.melt, aes(x = X2, y = value, group = X2)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) +
  theme_bw() +
  xlab("Times since arrival to the UK (years)") +
  ylab("Active TB cases avoided in EWNI")
