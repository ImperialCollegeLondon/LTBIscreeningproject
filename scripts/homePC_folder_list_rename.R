#
# temporary fix
# need to use on home PC
#
##TODO: replace this in body of code with here::here

folders$output <- lapply(folders$output, function(x) sub(pattern = "ngreen1", replacement = "Nathan", x = x))
folders$plots <- lapply(folders$plots, function(x) sub(pattern = "ngreen1", replacement = "Nathan", x = x))
