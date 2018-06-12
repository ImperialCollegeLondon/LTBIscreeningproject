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
cascade_nums <- readr::read_csv(paste0(parent_folder, "/combined_all_subsets.csv"))
cascade_probs <- readr::read_csv(paste0(parent_folder, "/combined_prob_subset_dectree.csv"))

cascade_nums$scenario <- as.factor(cascade_nums$scenario)
names(cascade_nums)[names(cascade_nums) == "L95"] <- "L95_1"
names(cascade_nums)[names(cascade_nums) == "U95"] <- "U95_1"
names(cascade_nums)[names(cascade_nums) == "mean"] <- "mean_1"

cascade_probs$scenario <- as.factor(cascade_probs$scenario)
names(cascade_probs)[names(cascade_probs) == "L95"] <- "L95_1"
names(cascade_probs)[names(cascade_probs) == "U95"] <- "U95_1"
names(cascade_probs)[names(cascade_probs) == "mean"] <- "mean_1"


cascade_data <- cascade_probs

# select columns to plot

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

const_cols <- grepl(x = names(cascade_LTBI),
                    pattern = "X1|scenario|X2")

for (i in policies) {

  cols_policy <- const_cols | grepl(x = names(cascade_LTBI), pattern = paste0("._",i))
  dat <- cascade_LTBI[ ,cols_policy]

  names(dat) <- gsub(pattern = paste0("_",i),
                     replacement = "",
                     names(dat))

  ## box plot
  p <-
    ggplot(dat, aes(x = factor(X2), fill = scenario, col = scenario)) +
    geom_boxplot(aes(lower = mean, upper = mean, middle = mean, ymin = L95, ymax = U95), stat = "identity")

  ## bar plot
  p <-
    ggplot(dat, aes(x = X2, y = mean, fill = scenario)) +
    geom_bar(stat = "identity", color = "white",
             position = position_dodge()) +
    geom_errorbar(aes(ymin = L95, ymax = U95), width = 0.2,
                  position = position_dodge(0.9))

  p <-
    p + ggplot2::ylim(0, 1) +
    theme_bw() +
    ylab('Number in cohort intended for screening') +
    xlab('')

  # print(p)
  ggplot2::ggsave(p, file = paste(plots_folder, policies_ls[i],
                                  "cascade_LTBI.png", sep = "/"),
                  width = 30, height = 20, units = "cm")
}


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
  dplyr::filter(scenario == 1) %>%
  ggplot(aes(x = X2, y = mean, fill = policy)) +
  geom_bar(stat = "identity", color = 'white',
           position = position_dodge()) +
  geom_errorbar(aes(ymin = L95, ymax = U95), width = 0.2,
                position = position_dodge(0.9)) +
  ggplot2::ylim(0, 1) +
  theme_bw() +
  ylab('Number in cohort intended for screening') +
  xlab('')

ggplot2::ggsave(p, file = paste(plots_folder,
                                "cascade_LTBI_scenario1.png", sep = "/"),
                width = 30, height = 20, units = "cm")


# line plot --------------------------------------------------------------

# p <-
#   ggplot(cascade_LTBI,
#          aes(
#            x = X2,
#            y = mean,
#            group = scenario,
#            color = scenario
#          )) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = L95, ymax = U95),
#                 width = .2,
#                 position = position_dodge(0.05)) +
#   ggplot2::ylim(0, 1) +
#   theme_bw() +
#   ylab('Number in cohort intended for screening') +
#   xlab('')
# p


