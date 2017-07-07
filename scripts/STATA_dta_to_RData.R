#
# project: LTBI screening
# N Green
# Imperial College London
# 2016
#
# read-in STATA output and create RData and data dictionary



# convert from STATA to R -------------------------------------------------

# read-in WITHOUT labels but with correct date format
IMPUTED_IOM_ETS_WHO_merged_15_2_9 <- readstata13::read.dta13(file="T:\\STATA-model_incidence data_updated\\IMPUTED--IOM_ETS_WHO_merged_15_2_9.dta", convert.dates = TRUE)

# take random subsample
IMPUTED_sample <- IMPUTED_IOM_ETS_WHO_merged_15_2_9[sample.int(nrow(IMPUTED_IOM_ETS_WHO_merged_15_2_9), size = 1000), ]

save(IMPUTED_IOM_ETS_WHO_merged_15_2_9, file="T:\\STATA-model_incidence data_updated\\IMPUTED_IOM_ETS_WHO_merged_15_2_9.RData")
save(IMPUTED_sample, file="data\\sample_IMPUTED-IOM_ETS_WHO_merged.RData")



# data dictionary ---------------------------------------------------------

# read-in WITH labels but with dates as numeric
IMPUTED_IOM_ETS_WHO_merged_15_2_9 <- haven::read_dta("T:\\STATA-model_incidence data_updated\\IMPUTED--IOM_ETS_WHO_merged_15_2_9.dta")

# extract the column names descriptions (when they exist)
# attr(IMPUTED_IOM_ETS_WHO_merged_15_2_9$sex, which = "label") <- "description"
data_dic <- reshape2::melt(unlist(sapply(IMPUTED_IOM_ETS_WHO_merged_15_2_9, attr, which = "label")), value.name = "description")
data_dic$col_names <- row.names(data_dic)

# reattach the missing column names
data_dic <- merge(data.frame(col_names=names(IMPUTED_IOM_ETS_WHO_merged_15_2_9)), data_dic, all.x = TRUE)

save(data_dic, file="data/data_dic.RData")
write.table(data_dic, file="data/data_dic.txt")
