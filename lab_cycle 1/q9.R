run_length_encoding <- function(input_string) {
  n <- nchar(input_string)
  if (n == 0) return("")  
  
  encoded_string <- ""
  count <- 1
  
  for (i in 2:n) {
    if (substr(input_string, i, i) == substr(input_string, i - 1, i - 1)) {
      count <- count + 1
    } else {
      encoded_string <- paste0(encoded_string, substr(input_string, i - 1, i - 1), count)
      count <- 1
    }
  }
  
  encoded_string <- paste0(encoded_string, substr(input_string, n, n), count)
  
  return(encoded_string)
}

input_string <- readline(prompt = "Enter a string to compress: ")
compressed_string <- run_length_encoding(input_string)
cat("Compressed string:", compressed_string, "\n")
