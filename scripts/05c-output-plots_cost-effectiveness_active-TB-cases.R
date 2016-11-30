#
# project: LTBI screening
# N Green
# Oct 2016
#
# sample how many LTBI become active cases
# with screening for each scenario

##TODO##
# CEAC?


if(!exists("p.complete_treat_scenarios")){

  p.complete_treat_scenarios <- read.csv(file = paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"), header = FALSE)
  names(p.complete_treat_scenarios) <- who_levels
  p.complete_treat_scenarios$scenario <- rownames(p.complete_treat_scenarios)
}

hash <-
  p.complete_treat_scenarios %>%
  gather("who_prev_cat_Pareek2011", "value", -scenario)


rNotificationDate_issdt.years <- IMPUTED_sample_year_cohort$rNotificationDate_issdt.years
rNotificationDate_issdt.years <- ceiling(rNotificationDate_issdt.years[rNotificationDate_issdt.years>=0])
counts <- table(rNotificationDate_issdt.years)
barplot(counts)

scenario <- 441

p.completeTx <- hash[hash$scenario=="441" & hash$who_prev_cat_Pareek2011=="(50,150]", "value"]
counts.screen <- counts*p.completeTx
# barplot(counts.screen)


##TODO##
# binomial confidence interval normal approximation
#do we need n denominator as number of LTBI individuals?
#would the error be too small to make any difference anyway?
# p.completeTx + 1.96*p.completeTx*(1-p.completeTx)/pop_year



counts2 <- rbind(counts - counts.screen, counts.screen)
rownames(counts2) <- c("Missed",
                       "Avoided")


# single scenario bar plot
x11()
barplot(counts2,
        main = paste("Observed active TB incidence:", diroutput, "\n scenario", scenario),
        xlab = "Times since arrival to the UK (years)",
        ylab = "Active TB cases in UK",
        col = c("lightgrey", "darkgrey"),
        legend = rownames(counts2))

# cumulative counts
# h <- hist(rNotificationDate_issdt.years, breaks = 0:5, plot = FALSE)
# h$counts <- cumsum(h$counts)
# plot(h)


p.values <- hash %>% filter(who_prev_cat_Pareek2011=="(50,150]") %>% select(value)
counts.scenarios <- matrix(counts, nrow = nrow(values), ncol = length(counts), byrow = T)
for (i in seq_len(nrow(p.values))) counts.scenarios[i, ] <- counts.scenarios[i, ] * p.values[i,]


# all scenarios bar plot
counts.melt <- melt(counts.scenarios)
ggplot(data = counts.melt, aes(x=X2, color=X1, group=X1)) +
  geom_step(aes(y = value), alpha=.3) +
  theme(legend.position="none")
# geom_point(aes(y = value), alpha=.3)


# bar plot
ggplot(data = counts.melt, aes(x=X2, y = value, group=X2)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  theme_bw() +
  xlab("Times since arrival to the UK (years)") +
  ylab("Active TB cases avoided in UK")
