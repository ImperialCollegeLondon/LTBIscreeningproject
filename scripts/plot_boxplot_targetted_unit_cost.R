#
# two-way boxplot of targetting groups and unit cost of test
#
#


# load(here::here("ext-data", "runs_5", "5.123", "ce_res.RData"))
load(here::here("ext-data", "runs_5", "5.123", "ce_res_fixed.RData"))
load(here::here("ext-data", "runs_5", "5.123", "folders.RData"))

bcea_incr <- bcea_incremental(ce_res$ce_incr)

# run boxplot_INMB.R
# boxplot_INMB(bcea_incr, folders, oneway = TRUE)

dat$Agree_to_Screen_cost <- factor(dat$Agree_to_Screen_cost)
dat$Pop_cost <- as.factor(dat$Pop_cost)
dat$X2 <- as.factor(dat$X2)

# Rename the column and the values in the factor
levels(dat$Pop_cost)[levels(dat$Pop_cost) == "1"] <- "Total"
levels(dat$Pop_cost)[levels(dat$Pop_cost) == "2"] <- ">150/100,000"
levels(dat$Pop_cost)[levels(dat$Pop_cost) == "3"] <- ">250/100,000"

INMB_boxplot <-
  INMB_boxplot +
  geom_boxplot(show.legend = TRUE, lwd = 1.3) +
  theme(legend.justification = c(1,0), legend.position = c(0.99, 0.75)) +
  labs(color = 'Targetted group') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_jitterdodge(dodge.width = 0.7), alpha = 1/3)


ggplot2::ggsave(file = here::here("output", "plots", "runs_5", "INMB_boxplot_twoway.png"),
                plot = INMB_boxplot,
                width = 30, height = 20, units = "cm")

save(INMB_boxplot, file = here::here("output", "plots", "runs_5", "INMB_boxplot_twoway.RData"))
