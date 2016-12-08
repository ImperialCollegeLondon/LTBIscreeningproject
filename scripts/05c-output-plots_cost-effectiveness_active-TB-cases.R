#
# project: LTBI screening
# N Green
# Dec 2016
#
# proportion of LTBI become active cases
# with screening for each scenario
# and plot bar and box plots


library(ggplot2)


if(!exists("p.complete_treat_scenarios")){

  p.complete_treat_scenarios <- read.csv(file = paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"), header = FALSE)
  names(p.complete_treat_scenarios) <- who_levels
  p.complete_treat_scenarios$scenario <- rownames(p.complete_treat_scenarios)
}

# LTBI probability look-up table
hash <-
  p.complete_treat_scenarios %>%
  gather("who_prev_cat_Pareek2011", "value", -scenario)


######################
## before screening ##
######################

rNotificationDate_issdt.years <- IMPUTED_sample_year_cohort$rNotificationDate_issdt.years
rNotificationDate_issdt.years <- ceiling(rNotificationDate_issdt.years[rNotificationDate_issdt.years>=0])
activeTBcases <- table(rNotificationDate_issdt.years)

barplot(height = activeTBcases,
        ylim = c(0, 200),
        main = "Raw number of active TB cases", xlab = "Time in UK (years)", ylab = "Cases")


# including LTBI indiv who have left UK

activeTBcases_UK_nonUK <- rbind("UK observed" = activeTBcases,
                                "Non-UK estimated" = cum_year_total.diff[names(activeTBcases)])
x11()
barplot(height = activeTBcases_UK_nonUK,
        main = "Number of active TB cases",
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases",
        col = c("lightgrey", "darkgrey"),
        legend = rownames(activeTBcases_UK_nonUK))


#####################
## after screening ##
#####################

# e.g.
scenario <- 30

p.completeTx <- hash[hash$scenario==as.character(scenario) &
                     hash$who_prev_cat_Pareek2011=="(50,150]", "value"]

activeTBcases_after_screen <- activeTBcases * (1 - p.completeTx)

barplot(height = activeTBcases_after_screen,
        main = "Raw number of active TB cases after screening",
        xlab = "Time in UK (years)", ylab = "Cases")


# including LTBI indiv who have left UK

activeTBcases_UK_nonUK <- rbind("UK observed" = activeTBcases_after_screen,
                                "Non-UK estimated" = cum_year_total.diff[names(activeTBcases_after_screen)] * (1 - p.completeTx))
x11()
barplot(height = activeTBcases_UK_nonUK,
        main = "Number of active TB cases after screening",
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases",
        col = c("lightgrey", "darkgrey"),
        legend = rownames(activeTBcases_UK_nonUK))



##TODO##
# binomial confidence interval normal approximation
# do we need n denominator as number of LTBI individuals?
# would the error be too small to make any difference anyway?
# p.completeTx + 1.96*p.completeTx*(1-p.completeTx)/pop_year






#####################################
## combined before/after screening ##
#####################################

# stack missed and avoided active TB cases
missed_avoided_activeTBcases <- rbind("Missed" = activeTBcases_after_screen,
                                      "Avoided" = activeTBcases - activeTBcases_after_screen)

# single scenario bar plot
x11()
barplot(height = missed_avoided_activeTBcases,
        main = paste("Observed active TB incidence:", diroutput, "\n scenario", scenario),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases in UK",
        col = c("lightgrey", "darkgrey"),
        legend = rownames(missed_avoided_activeTBcases))

# cumulative counts
# h <- hist(rNotificationDate_issdt.years, breaks = 0:5, plot = FALSE)
# h$counts <- cumsum(h$counts)
# plot(h)



# including LTBI indiv who have left UK

# stack missed and avoided active TB cases
missed_avoided_UK_nonUK_activeTBcases <- rbind("Missed UK"  = activeTBcases_after_screen,
                                               "Avoided UK" = activeTBcases - activeTBcases_after_screen,
                                               "Missed non-UK"  = cum_year_total.diff[names(activeTBcases)] * (1 - p.completeTx),
                                               "Avoided non-UK" = cum_year_total.diff[names(activeTBcases)] * p.completeTx)

x11()
barplot(height = missed_avoided_UK_nonUK_activeTBcases,
        main = paste("Observed active TB incidence:", diroutput, "\n scenario", scenario),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases in UK",
        col = c("white", "lightgrey", "darkgrey", "black"),
        legend = rownames(missed_avoided_UK_nonUK_activeTBcases))




###################
## all scenarios ##
###################

p.completeTx_scenarios <- hash %>%
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
  ylab("Active TB cases avoided in UK")
