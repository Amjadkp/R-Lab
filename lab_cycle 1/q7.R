generate_fibonacci_twist <- function(n) {
  if (n <= 0) {
    return(c())
  } else if (n == 1) {
    return(c(0))
  } else if (n == 2) {
    return(c(0, 1))
  } else if (n == 3) {
    return(c(0, 1, 1))
  }
  
  series <- c(0, 1, 1)
  for (i in 4:n) {
    next_term <- sum(series[(i-3):(i-1)])
    series <- c(series, next_term)
  }
  
  return(series)
}

num_terms <- as.integer(readline(prompt = "Enter the number of terms: "))
series <- generate_fibonacci_twist(num_terms)
cat("The Fibonacci-like series up to", num_terms, "terms is:\n")
cat(series, "\n")
