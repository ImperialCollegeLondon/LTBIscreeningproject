#
# project: LTBI screening
# N Green
# Dec 2016
#
# proportion of LTBI become active cases
# with screening for each scenario
# bar plots & box plots


library(ggplot2)
library(denstrip)


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

##check are these for non-duplicate active tb cases only n.tb_year=368??

notifDate_issdt.years <- subset(IMPUTED_sample_year_cohort,
                                subset = (uk_tb==1 &
                                          rNotificationDate_issdt.years>=0),
                                select = rNotificationDate_issdt.years) %>%
                          ceiling()

activeTBcases <- table(notifDate_issdt.years)


# fit exponential to (latest) observed active TB
# yearly frequencies to predict missing years

activeTBcases.log <- model.frame(formula = logy.Freq ~ year,
                                 data = data.frame(logy = log(activeTBcases)[4:5], year = 4:5))
fit <- lm(activeTBcases.log)
years <- 1:8
uktb_estimated <- exp(years*fit$coefficients["year"] + fit$coefficients["(Intercept)"])
names(uktb_estimated) <- as.character(years)


uktb_estimated <- c(rep(0, length(activeTBcases)),
                    uktb_estimated[-(1:length(activeTBcases))])

# fillin missing years with NA
activeTBcases <- c(activeTBcases,
                   rep(0, length(uktb_estimated) - length(activeTBcases)))


# fillin later years with estimates (exponential)
# activeTBcases <- c(activeTBcases, uktb_estimated)
activeTBcases <- rbind(activeTBcases,
                       uktb_estimated)

colnames(activeTBcases) <- seq_len(ncol(activeTBcases))

png(paste(plots_folder, "/barplot_raw_num_aTB.png", sep=""))

barplot(height = activeTBcases,
        ylim = c(0, 150),
        main = "Raw number of active TB cases",
        xlab = "Time in UK (years)", ylab = "Cases")

dev.off()


# including LTBI indiv who have left EWNI -----------------------------------

activeTBcases_UK_nonUK <- rbind("UK observed" = activeTBcases,
                                "Non-UK estimated" = exituk_tb_year)
                                # "Non-UK estimated" = exituk_tb_year[names(activeTBcases)])

x11()
png(paste(plots_folder, "/barplot_aTB_with_exituk.png", sep = ""))

barplot(height = activeTBcases_UK_nonUK,
        main = sprintf("Number of active TB cases\n in %s cohort", year_cohort),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases",
        col = c("lightgrey", "darkgrey","lightblue"),
        xlim = c(0,10), ylim = c(0, 270),
        density = c(5,60,20,30), angle = c(10,45,75,11))

legend("topright", legend = c("Observed EWNI", "Estimated EWNI", "Estimated exited EWNI"),
       density = c(5,60,20), angle = c(10,45,75), fill = c("lightgrey", "darkgrey","lightblue"), bg = "white")

dev.off()


#####################
## after screening ##
#####################

# e.g.
# basecase?
SCENARIO <- 1#300

p.completeTx <- subset(x = LTBI_prob_lookup,
                       scenario==as.character(SCENARIO) & who_prev_cat_Pareek2011=="(50,150]",
                       select = value) %>%
                as.numeric()

activeTBcases_after_screen <- activeTBcases * (1 - p.completeTx)


png(paste(plots_folder, "/barplot_raw_num_aTB_screened.png", sep = ""))

barplot(height = activeTBcases_after_screen,
        main = sprintf("Raw number of active TB cases\n after screening in %s cohort", year_cohort),
        xlab = "Time in UK (years)", ylab = "Cases")

dev.off()


# using density strips
# to show uncertainty

plot(NA, xlim = c(0, 9), ylim = c(0, 170),
     xlab = "Time since arrival in EWNI (years)", ylab = "Number of notified active TB cases", type = "n")
axis(side = 1, at = 1:8)
combined_activeTBcases <- colSums(activeTBcases) %>% round()
x <- list()
for (i in seq_along(combined_activeTBcases)){

  x[[i]] <- rbinom(n = 1000, size = combined_activeTBcases[i], prob = 1 - p.completeTx)
  denstrip(x[[i]], colmax = "black", width = 1, at = i, horiz = FALSE, ticks = combined_activeTBcases[i], tlen = 1)
}



# including LTBI indiv who have left UK -----------------------------------

activeTBcases_UK_nonUK_after_screen <- rbind("UK observed" = activeTBcases_after_screen,
                                             "Non-UK estimated" = exituk_tb_year * (1 - p.completeTx))

x11()
png(paste(plots_folder, "/barplot_aTB_with_exituk_screened.png", sep = ""))

barplot(height = activeTBcases_UK_nonUK_after_screen,
        main = sprintf("Number of active TB cases after screening\n in %s cohort", year_cohort),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases",
        col = c("lightgrey", "darkgrey","lightblue"),
        xlim = c(0,10), ylim = c(0, 270),
        density = c(5,60,20,30), angle = c(10,45,75,11))

legend("topright", legend = c("Observed EWNI", "Estimated EWNI", "Estimated exited EWNI"),
       density = c(5,60,20), angle = c(10,45,75), fill = c("lightgrey", "darkgrey","lightblue"), bg = "white")

dev.off()


##TODO: binomial confidence interval normal approximation
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
png(paste(plots_folder, "/barplot_aTB_with_avoided.png", sep=""))

barplot(height = missed_avoided_activeTBcases,
        main = paste("Observed active TB incidence:\n", diroutput, "\n scenario", SCENARIO),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases",
        col = c("lightgrey", "darkgrey","lightblue","darkblue"),
        xlim = c(0,10), #ylim = c(0, 270),
        density = c(5,60,20,100), angle = c(10,45,75,11))

legend("topright", legend = c("Observed EWNI", "Estimated EWNI", "Cases avoided EWNI", "Estimated Cases avoided EWNI"),
       density = c(5,60,20,100), angle = c(10,45,75,11), fill = c("lightgrey", "darkgrey","lightblue","darkblue"), bg = "white")

dev.off()


# cumulative counts
# h <- hist(rNotificationDate_issdt.years, breaks = 0:5, plot = FALSE)
# h$counts <- cumsum(h$counts)
# plot(h)




# including LTBI indiv who have left UK -----------------------------------

# stack missed and avoided active TB cases
missed_avoided_UK_nonUK_activeTBcases <- rbind("Missed UK"  = activeTBcases_after_screen,
                                               "Avoided UK" = activeTBcases - activeTBcases_after_screen,
                                               "Missed non-UK"  = exituk_tb_year * (1 - p.completeTx),
                                               "Avoided non-UK" = exituk_tb_year * p.completeTx)

windows(rescale ="R")
png(paste(plots_folder, "/barplot_aTB_with_exituk_avoided.png", sep=""))

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

dev.off()


############################
## all scenarios combined ##
############################

p.completeTx_scenarios <- LTBI_prob_lookup %>%
                            filter(who_prev_cat_Pareek2011=="(50,150]") %>%
                            select(value)

counts.scenarios <- matrix(data = activeTBcases,
                           nrow = n.scenarios,
                           ncol = length(activeTBcases), byrow = TRUE)

for (i in seq_len(n.scenarios)){
  counts.scenarios[i, ] <- counts.scenarios[i, ] * p.completeTx_scenarios[i, ]
}


# all scenarios bar plot
counts.melt <- melt(counts.scenarios)


# ggplot2::ggplot(data = counts.melt, aes(x = X2, color = X1, group = X1)) +
#   geom_step(aes(y = value), alpha = .3) +
#   theme(legend.position = "none")
# geom_point(aes(y = value), alpha=.3)


# box ans whisker plot
ggplot(data = counts.melt, aes(x = X2, y = value, group = X2)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) +
  theme_bw() +
  xlab("Times since arrival to the UK (years)") +
  ylab("Active TB cases avoided in EWNI")

