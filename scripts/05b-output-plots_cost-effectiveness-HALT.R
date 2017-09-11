#
# project: LTBI screening HALT
# N Green
# Oct 2016
#
# output cost-effectiveness plots
# cost-effectiveness planes
# costs incurred and QALY gains


if (!exists("aTB_CE_stats")) load(pastef(diroutput, "aTB_CE_stats.RData"))

popscale <- 1#00000


###############
## active TB ##
###############

# convert active TB lists to dataframes
aTB_cost_melt <-
  Reduce(rbind,
         aTB_CE_stats$cost_incur_person) %>%
  data.frame(row.names = NULL)

aTB_QALYgain_melt <-
  Reduce(rbind,
         aTB_CE_stats$QALYgain_person) %>%
  data.frame(row.names = NULL)

# with status-quo
scenario.names <-
  c(0, seq_len(n.scenarios)) %>%
  as.character(.)


## BCEA format

aTB_cost.df <-
  t(rbind(0, aTB_cost_melt)) %>%
  as.data.frame() %>%
  set_names(scenario.names)

aTB_QALYgain.df <-
  t(rbind(0, aTB_QALYgain_melt)) %>%
  as.data.frame() %>%
  set_names(scenario.names)


####################
## LTBI screening ##
####################

if (cluster) {

  LTBI_cost_melt <- do.call(cbind.data.frame,
                            map(dectree_res, 1))

  LTBI_QALYloss_melt <- do.call(cbind.data.frame,
                                map(dectree_res, 2))

  ## BCEA format
  LTBI_cost.df <- data.frame('0' = 0, LTBI_cost_melt, check.names = FALSE)
  LTBI_QALYgain.df <- data.frame('0' = 0, -LTBI_QALYloss_melt, check.names = FALSE)

} else {

  # convert LTBI screening dataframes
  LTBI_cost_melt <- read.csv(file = pastef(diroutput, "mc_cost.csv"), header = FALSE)
  LTBI_QALYloss_melt <- read.csv(file = pastef(diroutput, "mc_health.csv"), header = FALSE)

  ## BCEA format

  # append status-quo scenario
  LTBI_cost.df <-
    t(rbind(0, LTBI_cost_melt)) %>%
    as.data.frame() %>%
    set_names(scenario.names)

  # NB negative QALY loss is QALY gain
  LTBI_QALYgain.df <-
    t(rbind(0, -LTBI_QALYloss_melt)) %>%
    as.data.frame() %>%
    set_names(scenario.names)
}

# discount due to delay to screening
LTBI_cost.df <- LTBI_cost.df * screen_discount
LTBI_QALYgain.df <- LTBI_QALYgain.df * screen_discount


############
## totals ##
############
# Q1 - Q0 different way round in function!

c.total <- as.matrix(LTBI_cost.df + aTB_cost.df) * popscale
e.total <- as.matrix(LTBI_QALYgain.df + aTB_QALYgain.df) * popscale

if (cluster & cluster_output_filename == "decisiontree-results-HALT.rds") {

  # screen.bcea <- bcea_multirefs(e = -e.total[ ,-1],
  #                               c = -c.total[ ,-1])
  screen.bcea <- bcea(e = -e.total[ ,-1],
                      c = -c.total[ ,-1])
}else{

  screen.bcea <- bcea(e = -e.total,
                      c = -c.total,
                      ref = 1,
                      interventions = colnames(e.total))
}


#########
# plots #
#########

# cost-effectiveness planes -----------------------------------------------

cbPalette <- colorRampPalette(c("red", "orange", "green", "blue"))(16)

ceplane.plot(screen.bcea, pos = "bottomright")
# contour(screen.bcea)

gg <- ceplane.plot(screen.bcea, graph = "ggplot2")
gg + scale_colour_manual(values = cbPalette)

gg <- contour2(screen.bcea, graph = "ggplot2", wtp = 30000)
gg + scale_colour_manual(values = cbPalette)

gg +
  # scale_color_brewer(palette = "Dark2") +
  # scale_colour_manual(values = cbPalette) +
  xlim(-0.0015, 0.0025) +
  ylim(-5, 10) +
  scale_color_discrete(labels = c("Mean weekly vs daily regimen")) +
  geom_abline(slope = 20000, lwd = 1) +
  geom_abline(slope = 30000, lwd = 0.5) +
  ggtitle("") + xlab("Incremental QALY gain") + ylab("Incremental cost incurred (£)")


# eib.plot(screen.bcea)

BCEA::ceac.plot(screen.bcea)

BCEA::ceac.plot(screen.bcea, graph = "ggplot2") + theme(legend.position = c(0.2, 0.4)) +
  geom_abline(slope = 0, intercept = 0.5, lty = 2) +
  geom_vline(xintercept = 30000, lwd = 0.5) +
  geom_vline(xintercept = 20000, lwd = 1) +
  scale_color_discrete(labels = c("Mean weekly vs Daily regimen")) +
  ggtitle("") + xlab("Cost-effectiveness threshold (£)")


ICER <- apply(screen.bcea$delta.c, 2, mean)/apply(screen.bcea$delta.e, 2, mean)


## table

apply(e.total, 2, mean) * 100000


