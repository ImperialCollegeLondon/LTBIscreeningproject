
# distributions of observed number of active TB
# after LTBI screening

lapply(n.tb_screen, function(x) hist(x$n[x$status=="uk_tb"], xlim = c(0, 200)))
