#
# project: LTBI screening
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
         aTB_CE_stats$aTB_cost_incur_person) %>%
  data.frame(row.names = NULL)

aTB_QALYgain_melt <-
  Reduce(rbind,
         aTB_CE_stats$aTB_QALYgain_person) %>%
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

c.total <- as.matrix(LTBI_cost.df + aTB_cost.df) * popscale
e.total <- as.matrix(LTBI_QALYgain.df + aTB_QALYgain.df) * popscale


screen.bcea <- bcea(e = -e.total,  # Q1 - Q0 different way round in function!
                    c =  -c.total,
                    ref = 1,
                    interventions = colnames(e.total))

#########
# plots #
#########

##TODO: how to get more than 8 colours in ggplot??
# COL <- colorRampPalette(brewer.pal(11, "Spectral"))(10)


# cost-effectiveness planes -----------------------------------------------

cbPalette <- colorRampPalette(c("red", "orange", "green", "blue"))(16)

ceplane.plot(screen.bcea, pos = "bottomright")
# contour(screen.bcea)

gg <- ceplane.plot(screen.bcea, graph = "ggplot2")

gg <- contour2(screen.bcea, graph = "ggplot2", wtp = 30000)

gg + scale_colour_manual(values = cbPalette)

gg +
  # scale_color_brewer(palette = "Dark2") +
  # scale_colour_manual(values = cbPalette) +
  # xlim(0, 0.008) +
  scale_color_discrete(labels = c("1: Baseline (6m iso £low)",
                                  "2: Agree to screen prob 0.1",
                                  "3: Agree to screen prob 1",
                                  "4: Start treatment prob 0.25",
                                  "5: Start treatment prob 1",
                                  "6: Complete treatment prob 0.5",
                                  "7: Complete treatment prob 1",
                                  "8: LTBI test cost £20",
                                  "9: LTBI test cost £100",
                                  "10: LTBI Treatment: 3m iso + rif £low",
                                  "11: LTBI Treatment: 6m iso £high",
                                  "12: LTBI Treatment: 3m iso + rif £high",
                                  "13: Best case: screening probs = 1",
                                  "14: Best case: probs = 1, perfect test/treat",
                                  "15: LTBI test cost £10, screening probs = 1",
                                  "16: LTBI test cost £0, screening probs = 1")) +
  annotate("text",
           x = apply(screen.bcea$delta.e, 2, mean),
           y = apply(screen.bcea$delta.c, 2, mean),
           label = 1:16) +
  theme(legend.position = c(0.2, 1))


eib.plot(screen.bcea)

BCEA::ceac.plot(screen.bcea)

BCEA::ceac.plot(screen.bcea, graph = "ggplot2") + theme(legend.position = c(0.2, 0.4)) +
  geom_abline(slope = 0, intercept = 0.5) +
  scale_color_discrete(labels = c("1: Baseline (6m iso £low)",
                                  "2: Agree to screen prob 0.1",
                                  "3: Agree to screen prob 1",
                                  "4: Start treatment prob 0.25",
                                  "5: Start treatment prob 1",
                                  "6: Complete treatment prob 0.5",
                                  "7: Complete treatment prob 1",
                                  "8: LTBI test cost £20",
                                  "9: LTBI test cost £100",
                                  "10: LTBI Treatment: 3m iso + rif £low",
                                  "11: LTBI Treatment: 6m iso £high",
                                  "12: LTBI Treatment: 3m iso + rif £high",
                                  "13: Best case: screening probs = 1",
                                  "14: Best case: probs = 1, perfect test/treat",
                                  "15: LTBI test cost £10, screening probs = 1",
                                  "16: LTBI test cost £0, screening probs = 1"))


ICER <- apply(screen.bcea$delta.c, 2, mean)/apply(screen.bcea$delta.e, 2, mean)


##TODO: update money saved/cases averted over time

# money saved/cases averted over time -------------------------------------

# status-quo

notif_eventtime <- na.omit(IMPUTED_sample_year_cohort$rNotificationDate_issdt)

CDF <- ecdf(notif_eventtime)

plot(CDF, yaxt = 'n',
     xlab = "Days since entry",
     main = "Raw cumulative counts of active TB cases\n in EWNI", pch = NA)

# axis(side = 2, at = 1, tck = 0.01, labels = sum(IMPUTED_sample$uk_tb), las = "2")
axis(side = 2, at = 1, tck = 0.01,
     labels = round(n.uktb_year * unit_cost$aTB_TxDx), las = "2") # direct cost of active TB diagnosis and treatment




# probability cost-effective for adherence vs uptake, for given co --------
##TODO: update contour plot adherence vs uptake

COST <- 50 #20, 50, 100

# expected incremental net benefit (INB)
e.INMB <- plyr::ldply(.data = INMB,
                      .fun = mean)

dat <- gdata::cbindX(scenario_parameter_cost,
                     scenario_parameter_p,
                     e.INMB)

names(dat) <-
  names(dat) %>%
  make.names(unique = TRUE)

dat.plot <- dat[dat$Agree.to.Screen == COST, ] #specific unit cost
# dat.plot <- dat.plot[dat.plot$Agree.to.Screen.1==0.1, ] #specific screening uptake


# contour
ggplot(data = dat.plot, aes(x = Start.Treatment, y = Complete.Treatment, z = V1)) +
  theme_bw() +
  stat_contour(show.legend = TRUE)

# coloured mesh
ggplot(dat.plot, aes(x = Start.Treatment, y = Complete.Treatment, z = V1)) +
  stat_contour(geom = 'polygon', aes(fill = ..level..)) +
  geom_tile(aes(fill = V1)) +
  #stat_contour(bins = 15) +
  xlab('Start treatment') +
  ylab('Complete treatment') +
  guides(fill = guide_colorbar(title = ''))


e.INMB.sq <- matrix(dat.plot$V1, 21)

filled.contour(x = seq(0, 1, by = 0.05),
               y = seq(0, 1, by = 0.05), e.INMB.sq,
               color = terrain.colors,
               xlab = "Completed Treatment", ylab = "Started Treatment",
               #plot.axes = { axis(1, seq(100, 800, by = 100))
               #  axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "INMB\n(GBP)"),
               nlevels = 5)
               #key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1




