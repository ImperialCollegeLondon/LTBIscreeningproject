# ***********************************
# project: LTBI screening
# N Green
# Oct 2016
#
# output cost-effectiveness plots
# cost-effectiveness planes


screen.bcea <- bcea(e = -e.total,  # Q1 - Q0 different way round in function!
                    c =  -c.total,
                    ref = 1,
                    interventions = colnames(e.total))

#########
# plots #
#########

# cost-effectiveness planes -----------------------------------------------

# SCENARIO_LABELS <- c("1: Baseline (6m iso £low)",
#                      "2: Agree to screen prob 0.1",
#                      "3: Agree to screen prob 1",
#                      "4: Start treatment prob 0.25",
#                      "5: Start treatment prob 1",
#                      "6: Complete treatment prob 0.5",
#                      "7: Complete treatment prob 1",
#                      "8: LTBI test cost £20",
#                      "9: LTBI test cost £100",
#                      "10: LTBI Treatment: 3m iso + rif £low",
#                      "11: LTBI Treatment: 6m iso £high",
#                      "12: LTBI Treatment: 3m iso + rif £high",
#                      "13: Best case: screening probs = 1, test £20",
#                      "14: Best case: probs = 1, perfect test/treat, test £20",
#                      "15: LTBI test cost £10, screening probs = 1",
#                      "16: LTBI test cost £0, screening probs = 1",
#                      "17: 90-90-90-90 agree-start-complete-effective & £20 test",
#                      "18: 90-90-90-95 agree-start-complete-effective  & £20 test & spec=1  & sens=0.95",
#                      "19: 90-90-100 start-complete-effective  & £20 test & spec=1",
#                      "20: 90-90-100 start-complete-effective & £20 test",
#                      "21: screen probs=1 sensitivity=1 & £20 test",
#                      "22: screen probs=1 sensitivity=0.95 & £20 test",
#                      "23: screen probs=1 sensitivity=0.98 & £20 test",
#                      "24: Best case: screen probs = 1, perfect test/treat, test £0")

cbPalette <- colorRampPalette(c("red", "orange", "green", "blue"))(screen.bcea$n.comparisons)

# ceplane.plot(screen.bcea, pos = "bottomright")
# contour(screen.bcea)

# gg <- ceplane.plot(screen.bcea, graph = "ggplot2")
# gg + scale_colour_manual(values = cbPalette)
#
#
# gg <- contour2(screen.bcea, graph = "ggplot2", wtp = 20000)
# gg + scale_colour_manual(values = cbPalette)


filename <- paste(plots_folder_scenario, "CE_plane2.png", sep = "/")

# png(filename)

##TODO: error when matrix isnt symmetric. Think its lack of variability/multimodal...
try(
  print(my_contour2(screen.bcea, graph = "ggplot2", wtp = 20000, CONTOUR_PC = "50%") +
        scale_colour_manual(values = cbPalette)),
  silent = TRUE)

ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")

# dev.off()


## annotations

# gg +
#   # scale_color_brewer(palette = "Dark2") +
#   # scale_colour_manual(values = cbPalette) +
#   # xlim(0, 0.008) +
#   scale_color_discrete(labels = SCENARIO_LABELS) +
#   annotate("text",
#            x = apply(screen.bcea$delta.e, 2, mean),
#            y = apply(screen.bcea$delta.c, 2, mean),
#            label = seq_along(SCENARIO_LABELS)) +
#   theme(legend.position = c(1, 0.2))


# with ICER values
# gg + annotate("text",
#            x = apply(screen.bcea$delta.e, 2, mean),
#            y = apply(screen.bcea$delta.c, 2, mean),
#            label = round(screen.bcea$ICER, 0))


