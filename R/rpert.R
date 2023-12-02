#' @title
#' Generate random variates from PERT distribution for AHLE modelling
#' 
#' @description
#' Returns random variates sampled from a modified PERT distribution given by the following parameterization
#' 
#' \eqn{\mu = \frac{x_{\mathrm{min}} + x_{\mathrm{max}} + \lambda x_{\mathrm{mode}}}{\lambda + 2}}
#' 
#' If \eqn{\mu = x_{\mathrm{mode}}}, then \eqn{v = \frac{\lambda}{2} + 1}, else \eqn{v = \frac{(\mu - x_\mathrm{min})(2x_\mathrm{mode} - x_\mathrm{min} - x_\mathrm{max})}{(x_\mathrm{mode} - \mu)(x_\mathrm{max} - x_\mathrm{min})}}
#' 
#' Define \eqn{w = \frac{v(x_\mathrm{max} - \mu)}{mu - x_\mathrm{min}}}, then generate variates from a \eqn{Beta(v, w)x_\mathrm{range} + x_\mathrm{min}} distribution, where \eqn{x_\mathrm{range} = x_\mathrm{max} - x_\mathrm{min}}
#' 
#' @param n Number of variates to generate
#' @param x_min Minimum x value to sample
#' @param x_max Maximum x value to sample
#' @param x_mode Most frequent x value to sample
#' @param lambda Scaling factor (set to 4 by default)
#' 
#' @returns A sample of random variates
#' 
#' @example
#' # rpert(10000, 1, 10, 5)
#' 

rpert <- function(n, x_min, x_max, x_mode, lambda = 4) {
  if (x_min > x_max || x_mode > x_max || x_mode < x_min) {
    stop("invalid parameters")
  }
  x_range <- x_max - x_min
  if (x_range == 0) {
    return(rep(x_min, n))
  }
  
  mu <- (x_min + x_max + lambda * x_mode) / (lambda + 2)
  
  # special case if mu == mode
  
  if (mu == x_mode) {
    v <- (lambda / 2) + 1
  }
  else {
    v <- ((mu - x_min) * (2 * x_mode - x_min - x_max)) /
      ((x_mode - mu) * (x_max - x_min))
  }
  
  w <- (v * (x_max - mu)) / (mu - x_min)
  return (rbeta(n, v, w) * x_range + x_min)
}
