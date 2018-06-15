
#' sim_tb_times
#'
#' @param data
#' @param prob
#'
#' @return
#' @export
#'
#' @examples
#'
sim_tb_times <- function(data,
                         prob) {
  pop <- nrow(data)

  tb_year <- vector(length = pop,
                    mode = "double")

  mat <- cbind(death_issdt = ceiling(data$date_death1_issdt.years),
               exit_issdt = ceiling(data$date_exit_uk1_issdt.years),
               LTBI = as.logical(data$LTBI),
               # death = as.logical(data$death1),
               exit_uk = as.logical(data$exit_uk1)
               )

  for (i in seq_len(pop)) {

    mati <- mat[i, ]

    tb_year[i] <-

      if (!mati['LTBI']) {

        Inf

      }else {

        t_left_trunc <-
          if (mati['exit_uk']) {
            mati['exit_issdt']
          }else 0 #over-write observed times

        sample_tb_year(
          fup_issdt = t_left_trunc,
          mati['death_issdt'],
          prob)
      }
  }

  return(tb_year)
}
