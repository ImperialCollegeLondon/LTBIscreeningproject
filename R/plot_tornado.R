

#' Tornado Plot
#'
#' Create a tornado plot for a cost-effectiveness
#' sensitivity analysis.
#' Supply the parameter names and maximum and minimum values for an output
#' statistic of interest e.g. ICER or INMB.
#' These need to be calculated before hand.
#'
#' @param dat Data frame of output maximum and minimum values
#' @param baseline_output
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' #e.g. ICER
dat <- data.frame(name = c("Specificity (min:0.8; max:1)",
                           "Sensitivity (min:0.8; max:1)",
                           "Cost of rule-out test (min:£10; max:£100)",
                           "Prevalence (min:40%; max:50%)",
                           "Dosanjh category 3 status (all active TB/all non-TB)",
                           "TB ruled-out non-TB patient 6 week follow-up (min:0%; max:10%)"),
                  min = c(1,2,3,4,5,6),
                  max = c(3,5,6,7,8,9))
baseline_output <- 3

plot_tornado <- function(dat,
                         baseline_output, ...){

  extra_args <- list(...)

  if(!is.data.frame(dat)) stop("Input data must be data frame.")

  # bar colours
  # 1 - dark: min to baseline
  # 2 - light: baseline to max
  colLOW  <- 1
  colHIGH <- 2

  # duplicate each row
  datplot <- dat[rep(1:nrow(dat), each = 2), ]

  odd_rows <- seq(from = 1, to = nrow(datplot), 2)
  even_rows <- odd_rows + 1

  # define 'central' point
  datplot$max[odd_rows] <- baseline_output
  datplot$min[even_rows] <- baseline_output

  datplot$fill <- c(colLOW, colHIGH)

  ggplot2::ggplot(datplot, aes(reorder(name, abs(max - min)), ymin = min, ymax = max, colour = fill)) +
    geom_linerange(size = 10) +
    coord_flip() +
    ylab("Incremental Cost-Effectiveness Ratio (ICER) (£/QALY)") +
    xlab("") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = baseline_output, linetype = "dashed") +
    theme_bw() +
    theme(legend.position = "none", axis.text = element_text(size = 15))
}


s_analysis <- data.frame(output = c(1,10,11,5,3),
                         sens = c(2,2,3,1,2),
                         spec = c(1,3,2,2,2))

MINS <- apply(s_analysis[ ,-1], 2, min)
MAXS <- apply(s_analysis[ ,-1], 2, max)
BASELINE <- apply(s_analysis[ ,-1], 2, mean)

subgrid_max <- data.frame(apply(diag(length(MAXS)), 1, function(y) MAXS*y),
                          val = "max",
                          names = names(MAXS), row.names = NULL)

subgrid_min <- data.frame(apply(diag(length(MINS)), 1, function(y) MINS*y),
                          val = "min",
                          names = names(MINS), row.names = NULL)

SUBGRID <- rbind(subgrid_max, subgrid_min)

for(param in seq_len(nrow(SUBGRID))){

  where_baseline <- SUBGRID[ ,param]==0
  SUBGRID[where_baseline, param] <- BASELINE[param]
}

names(SUBGRID) <- c(names(MAXS), names(SUBGRID)[-c(1,2)])
SUBGRID <- merge(SUBGRID, s_analysis, by = c("sens", "spec"))


