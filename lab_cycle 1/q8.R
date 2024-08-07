library(stringr)

is_palindrome <- function(input_string) {
  normalized_string <- tolower(str_replace_all(input_string, "[[:punct:]\\s]", ""))
  
  return(normalized_string == stri_reverse(normalized_string))
}

input_string <- readline(prompt = "Enter a string: ")
if (is_palindrome(input_string)) {
  cat("The string is a palindrome.\n")
} else {
  cat("The string is not a palindrome.\n")
}
