
#
screen_bcea <- function(total) {

  BCEA::bcea(e = -total$e,  # Q1 - Q0 different way round in original function!
             c =  -total$c,
             ref = 1,
             interventions = colnames(total$e))
}
