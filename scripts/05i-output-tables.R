

out_tab <- table_costeffectiveness(screen.bcea)

print(out_tab)
write.csv(x = out_tab,
            file = paste(diroutput, "costeffectiveness_table.csv", sep = "/"))
