numbers <- c(1, 34, 21, 66, 12, 98, 43, 22)

my_sd <- function(x) {
  len <- length(x)
  total <- 0
  
  # First loop: Calculate sum for mean
  for (i in 1:len) {
    total <- total + x[i]
  }
  
  mean <- total / len
  
  # Second loop: Calculate squared differences
  sum_sq_diff <- 0
  for (i in 1:len) {
    sum_sq_diff <- sum_sq_diff + (x[i] - mean)^2
  }
  
  # Return standard deviation (sample standard deviation)
  return(sqrt(sum_sq_diff / (len - 1)))
}

my_sd(numbers)

sd(numbers)
