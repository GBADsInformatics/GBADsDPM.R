library(mc2d) # for rpert()

rpert_gemma <- function(n, x_min, x_max, x_mode, lambda = 4) {
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
      ((x_mode - mu) * x_range)
  }
  
  w <- (v * (x_max - mu)) / (mu - x_min)
  return (rbeta(n, v, w) * x_range + x_min)
}


# Function to compare output - tests for exact equality
# Since a seed was set the results should be the same

fun <- function(seed_value) {
  set.seed(seed_value)
  f <- rpert_gemma(n = 5, x_min = 1, x_max = 5, x_mode = 2)
  g <- rpert(n = 5, min = 1, mode = 2, max = 5) # from mc2d
  
  return(list("Gemma" = f,
              "mc2d" = g,
              "identical" = identical(f, g)))
}


seed_value <- 0673227 # set random seed for reproducibility - can be an integer of any length
fun(seed_value = seed_value)

