#
# LTBI screening
# N Green
# July 2017

out_tab <- table_costeffectiveness(screen.bcea,
                                   wtp_threshold = wtp_threshold)

print(out_tab)
write.csv(x = out_tab,
            file = paste(diroutput, "costeffectiveness_table.csv", sep = "/"))
