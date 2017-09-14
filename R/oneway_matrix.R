
#' Generate One-Way Model Matrix
#'
#' Otherwise known as one-factor-at-a-time.
#' For full-factorial design use \code{expand.grid}.
#'
#' @param mid Vector of middle values
#' @param high Vector of high values
#' @param low Vector of low values
#'
#' @return matrix
#' @export
#'
#' @examples
#' 
#' mid <- c(2,3,4,10)
#' high <- c(4,5,6,100)
#' low <- c(0,1,2,-2)
#' 
#' oneway_matrix(mid, high, low)
#' 
oneway_matrix <- function(mid, high, low){
  
  n_vars <- length(mid)
  
  mat <- matrix(mid,
                nrow = 1 + 2*n_vars,
                ncol = n_vars,
                byrow = TRUE)
  
  ## without loop
  # # diagonals
  # delta <- row(mat) - col(mat)
  # 
  # # shift columns down by col number
  # delta <- t(t(delta) - seq_along(mid))
  # 
  # mat[delta == 0] <- high
  # mat[delta == 1] <- low

  for (i in seq_len(n_vars)) {
    
    mat[2*i, i] <- high[i]
    mat[2*i + 1, i] <- low[i]
  }
  
  return(mat)
}
