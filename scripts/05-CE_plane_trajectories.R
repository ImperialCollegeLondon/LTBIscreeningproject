#
# 05-CE_plane_trajectories
#


dat <- data.frame(design_matrix,
                  mean_e = colMeans(screen.bcea$delta.e),
                  mean_c = colMeans(screen.bcea$delta.c))

Effective_dat <- dat[dat$Start == 60 & dat$Complete == 60 & dat$Agree == 60, ]
Start_dat <- dat[dat$Effective == 60 & dat$Complete == 60 & dat$Agree == 60, ]
Agree_dat <- dat[dat$Start == 60 & dat$Complete == 60 & dat$Effective == 60, ]
Complete_dat <- dat[dat$Start == 60 & dat$Effective == 60 & dat$Agree == 60, ]


filename <- paste(plots_folder_scenario, "ce_plane_trajectories.png", sep = "/")
png(filename)

plot(NULL, xlab = "Health gained (QALYs)", ylab = "Cost incurred (£)",
     xlim = c(0, 0.005), ylim = c(0, 50))
lines(Effective_dat$mean_e, Effective_dat$mean_c, type = 'l', col = "black")
lines(Start_dat$mean_e, Start_dat$mean_c, type = 'l', col = "red")
lines(Agree_dat$mean_e, Agree_dat$mean_c, type = 'l', col = "green")
lines(Complete_dat$mean_e, Complete_dat$mean_c, type = 'l', col = "blue")
abline(a = 0, b = 20000, lty = 2)
text(Effective_dat$mean_e, Effective_dat$mean_c, Effective_dat$Effective)
text(Start_dat$mean_e, Start_dat$mean_c, Start_dat$Start)
text(Agree_dat$mean_e, Agree_dat$mean_c, Agree_dat$Agree)
text(Complete_dat$mean_e, Complete_dat$mean_c, Complete_dat$Complete)
legend("topright", legend = c("Agree","Start","Complete","Effective"), col = c("green","red","blue","black"), lty = 1)

dev.off()
