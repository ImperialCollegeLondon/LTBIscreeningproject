
library(dplyr)
library(reshape2)
library(purrr)
library(magrittr)


#' Tornado Plot
#'
#' Create a tornado plot for a cost-effectiveness
#' sensitivity analysis.
#' Supply the parameter names and maximum and minimum values for an output
#' statistic of interest e.g. ICER or INMB.
#' These need to be calculated before hand.
#'
#' @param dat Data frame of output maximum and minimum values
#' @param baseline_output Values of outputs for baseline input paramater value to compare maximum and minimum against
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' #e.g. ICER
dat <- data.frame(names = c("Specificity (min:0.8; max:1)",
                           "Sensitivity (min:0.8; max:1)",
                           "Cost of rule-out test (min:£10; max:£100)",
                           "Prevalence (min:40%; max:50%)",
                           "Dosanjh category 3 status (all active TB/all non-TB)",
                           "TB ruled-out non-TB patient 6 week follow-up (min:0%; max:10%)"),
                  min = c(1,2,3,4,5,6),
                  max = c(3,5,6,7,8,9))

dat <- melt(dat, id.vars = "names",
            variable.name = "val",
            value.name = "output") %>%
              arrange(names)

class(dat) <- c("tornado", class(dat))

baseline_output <- 3

plot_tornado <- function(dat,
                         baseline_output = NA, ...){

  extra_args <- list(...)

  if(all(class(dat)!="tornado")) stop("Input data must be tornado class data frame.")
  if(length(baseline_output)!=1) stop("baseline_input must be length one.")

  # bar colours
  # 1 - dark: min to baseline
  # 2 - light: baseline to max
  colLOW  <- 1
  colHIGH <- 2

  # datplot <- dat %>%
  #   group_by(names) %>%
  #   summarise(max(output), min(output))
  #
  # # duplicate each row
  # datplot <- datplot[rep(1:nrow(datplot), each = 2), ]
  #
  # odd_rows <- seq(from = 1, to = nrow(datplot), 2)
  # even_rows <- odd_rows + 1
  #
  #   # define 'central' point
  #   datplot$max[odd_rows]  <- baseline_output
  #   datplot$min[even_rows] <- baseline_output

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

dat$baseline <- baseline_output

datplot <- dat[ ,c("output", "baseline")] %>%
            by_row(min, .collate = "cols") %>%
            by_row(max, .collate = "cols")

datplot <- cbind(dat, datplot[ ,-c(1,2)])


#' Convert Sensitivity Analysis Output Data to Tornado Plot Input Data
#'
#' @param s_analysis Model.frame object
#' @param baseline_input Vector of baseline parameter values
#'
#' @return Data frame of class tornado
#' @export
#'
#' @examples
#'
#' s_analysis <- data.frame(output = c(1,10,11,5,3),
#' sens = c(2,2,3,1,2),
#' spec = c(1,3,2,2,2))
#'
#' s_analysis <- model.frame(formula = output ~ sens + spec,
#'                           data = s_analysis)
#'
#' s_analysis_to_tornado_plot_data(s_analysis)
#'
s_analysis_to_tornado_plot_data <- function(s_analysis,
                                            baseline_input = NA){

  if(!(is.data.frame(s_analysis) & typeof(s_analysis)=="list")){
    stop("Require model.frame type as input data.")
  }

  if(!is.vector(baseline_input)) stop("baseline_input must be a vector.")

  output_name <- terms(s_analysis)[[2]]
  design_matrix <- subset(x = s_analysis,
                          select = -eval(parse(text = output_name)))

  MINS <- apply(design_matrix, 2, min)
  MAXS <- apply(design_matrix, 2, max)

  if(is.na(baseline_input)){
    baseline_input <- apply(design_matrix, 2, mean)
  }

  diag_array  <- diag(length(MAXS))
  param_names <- names(MAXS)

  subgrid_max <- data.frame(apply(diag_array, 1, function(y) MAXS*y),
                            val = "max",
                            names = param_names, row.names = NULL)

  subgrid_min <- data.frame(apply(diag_array, 1, function(y) MINS*y),
                            val = "min",
                            names = param_names, row.names = NULL)

  SUBGRID <- rbind(subgrid_max, subgrid_min) %>%
                    set_names(c(names(MINS), "val", "names"))

  # substitute in the baseline values
  for(param in seq_len(nrow(SUBGRID))){

    where_baseline <- SUBGRID[ ,param]==0
    SUBGRID[where_baseline, param] <- baseline_input[param]
  }

  # join output values
  SUBGRID <- merge(SUBGRID, s_analysis, by = c("sens", "spec")) %>%
                arrange(names)
  class(SUBGRID) <- c("tornado", class(SUBGRID))

  return(SUBGRID)
}
