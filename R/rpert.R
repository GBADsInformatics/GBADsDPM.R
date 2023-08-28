#' @description
#' Generate random variates from PERT distribution. Returns random variates sampled 
#' from a modified PERT distribution given by the following parameterization
#' 
#' @param n Number of variates to generate
#' @param x.min Minimum x value to sample
#' @param x.max Maximum x value to sample
#' @param x.mode Most frequent x value to sample
#' @param lambda Scaling factor
#' 
#' @returns A sample of random variates
#' 
#' @example
#' # rpert(10000, 1, 10, 5)
#' 

rpert <- function(n, x.min, x.max, x.mode, lambda = 4) {
  if (x.min > x.max || x.mode > x.max || x.mode < x.min) {
    stop("invalid parameters")
  }
  x.range <- x.max - x.min
  if (x.range == 0) {
    return(rep(x.min, n))
  }
  
  mu <- (x.min + x.max + lambda * x.mode) / (lambda + 2)
  
  # special case if mu == mode
  
  if (mu == x.mode) {
    v <- (lambda / 2) + 1
  }
  else {
    v <- ((mu - x.min) * (2 * x.mode - x.min - x.max)) /
      ((x.mode - mu) * (x.max - x.min))
  }
  
  w <- (v * (x.max - mu)) / (mu - x.min)
  return (rbeta(n, v, w) * x.range + x.min)
}