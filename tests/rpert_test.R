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

fun <- function(n, seed_value) {
  set.seed(seed_value)
  f <- rpert_gemma(n = n, x_min = 1, x_max = 5, x_mode = 2)
  g <- rpert(n = n, min = 1, mode = 2, max = 5) # from mc2d
  
  return(list("Gemma rpert" = f,
              "mc2d rpert" = g,
              "identical" = identical(f, g)))
}


# Test

n <- 5
seed_value <- 0673227 
fun(n = n, seed_value = seed_value)

