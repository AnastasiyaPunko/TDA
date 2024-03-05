rnorm(50, 184, 20)

install.packages('openxlsx')
library(openxlsx)

rgbeta <- function(n, mean, var, min, max)
{
  dmin <- mean - min
  dmax <- max - mean
  
  if (dmin <= 0 || dmax <= 0)
  {
    stop(paste("mean must be between min =", min, "and max =", max)) 
  }
  
  if (var >= dmin * dmax)
  {
    stop(paste("var must be less than (mean - min) * (max - mean) =", dmin * dmax))
  }
  
  # mean and variance of the standard beta distributed variable
  mx <- (mean - min) / (max - min)
  vx <- var / (max - min)^2
  
  # find the corresponding alpha-beta parameterization
  a <- ((1 - mx) / vx - 1 / mx) * mx^2
  b <- a * (1 / mx - 1)
  
  # generate standard beta observations and transform
  x <- rbeta(n, a, b)
  y <- (max - min) * x + min
  
  return(y)
}

set.seed(1)

n <- 50


y <- rgbeta(n, mean = 4.33, var=10, min = 0.86, max = 2153)
sapply(list(mean, var, min, max), function(f) f(y))
mean(y)

df <- data.frame(y)
write.xlsx(df, '/Users/anastasiyapunko/Documents/IL10-p.xlsx', colNames = TRUE)

