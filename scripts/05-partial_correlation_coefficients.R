#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# partial correlation coefficients


library(ppcor)
# library(pse)


# all pairwise partial correlation
# pcor(design_matrix[ ,c("Agree", "Start", "Complete", "Effective")], method = "spearman")

# pcc of screening
pcor_screen <- pcor.test(x = design_matrix$NMB,
                         y = design_matrix$policy,
                         z = design_matrix[ ,c("Agree", "Start", "Complete", "Effective")], #controlling for
                         method = "spearman")

# pcc of each step
dm_screen <- subset(design_matrix, policy == "screened")

pcor_agree <- pcor.test(x = dm_screen$NMB,
                        y = dm_screen$Agree,
                        z = dm_screen[ ,c("Start", "Complete", "Effective")],
                        method = "spearman")

pcor_start <- pcor.test(x = dm_screen$NMB,
                        y = dm_screen$Start,
                        z = dm_screen[ ,c("Agree", "Complete", "Effective")],
                        method = "spearman")

pcor_complete <- pcor.test(x = dm_screen$NMB,
                           y = dm_screen$Complete,
                           z = dm_screen[ ,c("Agree", "Start", "Effective")],
                           method = "spearman")

pcor_effective <- pcor.test(x = dm_screen$NMB,
                            y = dm_screen$Effective,
                            z = dm_screen[ ,c("Agree", "Start", "Complete")],
                            method = "spearman")

out <- cbind(var = c("screen", "agree", "start", "complete", "effective"),
             rbind(pcor_screen, pcor_agree, pcor_start, pcor_complete, pcor_effective))

# save --------------------------------------------------------------------

write.csv(out, file = paste(diroutput, "pcc_table.csv", sep = "/"))

