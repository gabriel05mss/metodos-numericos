uniforme <- function(n, seed, a = 16807, m = 2147483647, c = 1933) {
  x <- numeric(n)
  x[1] <- (a * seed + c) %% m
  for (i in 2:n) {
    x[i] <- (a * x[i-1] + c) %% m
  }
  return(x/m)
}


