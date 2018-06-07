#
# LTBI screening
# N Green
#
# care cascade plots
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization


library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(readr)

# cascade_data <- read_csv("C:\\Users\\ngreen1\\Dropbox\\TB\\LTBI\\R\\LTBIscreeningproject\\ext-data\\baseline-perfect\\18_to_35_in_2009\\combined_all_subsets.csv")
# cascade_data <- read_csv("ext-data/18_to_35_in_2009/combined_all_subsets.csv")

cascade_data$scenario <- as.factor(cascade_data$scenario)

cascade_LTBI <-
  cascade_data %>%
  subset(X2 %in% c("LTBI_completeTx","LTBI_positive","LTBI_startTx","LTBI_tests","p_LTBI_to_cured"))

cascade_all <-
  cascade_data %>%
  subset(X2 %in% c("LTBI_pre","tests","positive","startTx","completeTx","cured","LTBI_post"))

# reorder
cascade_all$X2 <- factor(cascade_all$X2,
                          levels = c("LTBI_pre","tests","positive","startTx","completeTx","cured","LTBI_post"))

cascade_LTBI$X2 <- factor(cascade_LTBI$X2,
                          levels = c("LTBI_tests","LTBI_positive","LTBI_startTx","LTBI_completeTx","p_LTBI_to_cured"))

# by scenario -------------------------------------------------------------

p <-
  ggplot(cascade_all, aes(x = X2, y = mean, fill = scenario)) +
  geom_bar(stat = "identity", color = "white",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = L95, ymax = U95), width = 0.2,
                position = position_dodge(0.9)) +
  ggplot2::ylim(0, 1) +
  theme_bw() +
  ylab('Number in cohort intended for screening') +
  xlab('')

p

p <-
  ggplot(cascade_LTBI, aes(x = X2, y = mean, fill = scenario)) +
  geom_bar(stat = "identity", color = "white",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = L95, ymax = U95), width = 0.2,
                position = position_dodge(0.9)) +
  ggplot2::ylim(0, 1) +
  theme_bw() +
  ylab('Number in cohort intended for screening') +
  xlab('')

p

p <-
  ggplot(cascade_LTBI, aes(x = X2, y = mean_2, fill = scenario)) +
  geom_bar(stat = "identity", color = "white",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = L95_2, ymax = U95_2), width = 0.2,
                position = position_dodge(0.9)) +
  ggplot2::ylim(0, 1) +
  theme_bw() +
  ylab('Number in cohort intended for screening') +
  xlab('')

p


# by policy ---------------------------------------------------------------

xx <-
  cascade_LTBI[ ,-1] %>%
  as.data.frame() %>%
  melt() %>%
  separate(variable,
           c("variable", "policy"))

xx$policy[is.na(xx$policy)] <- 1

cascade_policy <-
  xx %>%
  dcast(X2 + scenario + policy ~ variable)

p <-
  cascade_policy %>%
  dplyr::filter(scenario == 2) %>%
  ggplot(aes(x = X2, y = mean, fill = policy)) +
  geom_bar(stat = "identity", color = 'white',
           position = position_dodge()) +
  geom_errorbar(aes(ymin = L95, ymax = U95), width = 0.2,
                position = position_dodge(0.9)) +
  ggplot2::ylim(0, 1) +
  theme_bw() +
  ylab('Number in cohort intended for screening') +
  xlab('')

p


# line plot --------------------------------------------------------------

p <-
  ggplot(cascade_LTBI,
         aes(
           x = X2,
           y = mean,
           group = scenario,
           color = scenario
         )) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = L95, ymax = U95),
                width = .2,
                position = position_dodge(0.05)) +
  ggplot2::ylim(0, 1) +
  theme_bw() +
  ylab('Number in cohort intended for screening') +
  xlab('')
p


