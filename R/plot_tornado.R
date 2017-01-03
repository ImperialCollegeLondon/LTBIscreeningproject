
library(ggplot2)
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
#' @seealso \code{\link{s_analysis_to_tornado_plot_data}}
#' @examples
#'
#' ## user defined ##
#' dat <- data.frame(names = c("Specificity (min:0.8; max:1)",
#'                            "Sensitivity (min:0.8; max:1)",
#'                            "Cost of rule-out test (min:£10; max:£100)",
#'                            "Prevalence (min:40%; max:50%)",
#'                            "Dosanjh category 3 status (all active TB/all non-TB)",
#'                            "TB ruled-out non-TB patient 6 week follow-up (min:0%; max:10%)"),
#'                   min = c(1,2,2,2.4,2.5,1.6),
#'                   max = c(3,5,6,7,8,9))
#'
#' dat <- melt(dat, id.vars = "names",
#'             variable.name = "val",
#'             value.name = "output") %>%
#'               arrange(names)
#'
#' class(dat) <- c("tornado", class(dat))
#'
#' baseline_output <- 3
#' plot_tornado(dat, baseline_output)
#'
#' ## model ouput ##
#' s_analysis <- data.frame(output = c(10,1,11,5,3),
#'                          sens = c(2,2,3,0,2),
#'                          spec = c(1,4,2,2,2))
#'
#' s_analysis <- model.frame(formula = output ~ sens + spec,
#'                           data = s_analysis)
#'
#' dat <- s_analysis_to_tornado_plot_data(s_analysis)
#' plot_tornado(dat, 6)
#'
plot_tornado <- function(dat,
                         baseline_output,
                         YLAB = "", ...){

  extra_args <- list(...)

  if(all(class(dat)!="tornado")) stop("Input data must be tornado class data frame.")
  if(length(baseline_output)!=1) stop("baseline_input must be length one.")


  dat$baseline <- baseline_output

  # order output columns as decending and accending
  datplot <- dat[ ,c("output", "baseline")] %>%
    by_row(min, .collate = "cols", .to = "min") %>%
    by_row(max, .collate = "cols", .to = "max")

  # remove duplicate columns
  datplot <- cbind(dat, datplot[ ,-c(1,2)])

  # order by length of bars
  datplot$names = factor(as.character(datplot$names),
                         levels = rev(unique(datplot$names[order(datplot$min, decreasing = FALSE)])))

  ggplot2::ggplot(datplot, aes(names, ymin = min, ymax = max, colour = val)) +
    geom_linerange(size = 10) +
    coord_flip() +
    ylab(YLAB) +
    xlab("") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = baseline_output, linetype = "dashed") +
    theme_bw() +
    theme(legend.position = "none", axis.text = element_text(size = 15))
}




#' Convert Sensitivity Analysis Output Data to Tornado Plot Input Data
#'
#' @param s_analysis model.frame object
#' @param baseline_input Vector of baseline parameter values
#'
#' @return Data frame of class tornado
#' @export
#'
#' @seealso \code{\link{plot_tornado}}
#' @examples
#'
#' s_analysis <- data.frame(output = c(10,1,11,5,3),
#'                          sens = c(2,2,3,0,2),
#'                          spec = c(1,4,2,2,2))
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

  # find parameters upper and lower limits
  MINS <- apply(design_matrix, 2, min)
  MAXS <- apply(design_matrix, 2, max)

  # if baseline parameter not provided
  # use an average
  if(is.na(baseline_input)){
    baseline_input <- apply(design_matrix, 2, function(x) round(median(x)))
  }

  # create empty array for combinations of
  # max/min and baseline values for each parameter
  # i.e. one-way sensitivity analysis
  diag_array  <- diag(length(MAXS))
  diag_array[diag_array==0] <- NA

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

    where_baseline <- is.na(SUBGRID[ ,param])
    SUBGRID[where_baseline, param] <- baseline_input[param]
  }

  # join output values
  SUBGRID <- merge(SUBGRID, s_analysis, by = c("sens", "spec")) %>%
                arrange(names)

  class(SUBGRID) <- c("tornado", class(SUBGRID))

  return(SUBGRID)
}
