# *****************************
# LTBI screening
# N Green
# July 2017
#
# create output tables


# INMB --------------------------------------------------------------------
# compare with lm_multi

out_tab <- table_costeffectiveness(screen.bcea,
                                   wtp_threshold = wtp_threshold)

# print(out_tab)

write.csv(x = out_tab,
            file = paste(diroutput, "costeffectiveness_table.csv", sep = "/"))



# tb avoided --------------------------------------------------------------

out_tb_tab <- table_tb_avoided(map(dectree_res, "n_tb_screen_all"),
                               map(dectree_res, "n_tb_screen_uk"))

# print(out_tb_tab)

write.csv(x = out_tb_tab,
          file = paste(diroutput, "tb_avoided_table.csv", sep = "/"))
