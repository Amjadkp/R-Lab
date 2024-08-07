calculate_series_sum <- function(n) {
  sum <- 0
  
  for (i in 1:n) {
    term <- i / (2 * i - 1)
    if (i %% 2 == 0) {
      term <- -term
    }
    sum <- sum + term
  }
  
  return(sum)
}

num_terms <- as.integer(readline(prompt = "Enter the number of terms: "))
series_sum <- calculate_series_sum(num_terms)
cat("The sum of the series up to", num_terms, "terms is:", series_sum, "\n")
