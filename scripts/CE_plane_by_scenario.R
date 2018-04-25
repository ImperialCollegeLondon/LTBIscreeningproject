# ***********************************
# project: LTBI screening
# N Green
#
# output cost-effectiveness plots
# cost-effectiveness planes by scenario


diroutput <- sprintf("%s/%s", plots_folder, "CE_plane_by_scenario")
dir.create(diroutput, showWarnings = FALSE)

for (i in seq_len(n.scenarios)) {

  screen.bcea <- BCEA::bcea(e = -e.total_scenario[[i]],
                            c =  -c.total_scenario[[i]],
                            ref = 1,
                            interventions = colnames(e.total_scenario))


  cbPalette <- colorRampPalette(c("red", "orange", "green", "blue"))(screen.bcea$n.comparisons)

  filename <- paste(diroutput, paste0("CE_plane_by_scenario_",i, ".png"), sep = "/")

  suppressMessages(
    try(
      print(my_contour2(screen.bcea,
                        graph = "ggplot2",
                        wtp = 20000,
                        CONTOUR_PC = "50%") +
              coord_cartesian(xlim = c(0, 0.04), ylim = c(-200, 200)) +
              scale_colour_manual(values = cbPalette)),
      silent = TRUE))

  ggplot2::ggsave(file = filename, width = 30, height = 20, units = "cm")
}


