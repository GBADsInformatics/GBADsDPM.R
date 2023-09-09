#' @title
#' Generate random variates from PERT distribution
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
#' @param x.min Minimum x value to sample
#' @param x.max Maximum x value to sample
#' @param x.mode Most frequent x value to sample
#' @param lambda Scaling factor (set to 4 by default)
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
