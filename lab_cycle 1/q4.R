generate_password <- function(length) {
  if (length < 4) {
    stop("Password length should be at least 4 to include all character types.")
  }
  
  uppercase <- sample(LETTERS, 1) # One uppercase letter
  lowercase <- sample(letters, 1) # One lowercase letter
  digits <- sample(0:9, 1) # One digit
  special_characters <- sample(c('!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '=', '+'), 1) # One special character
  
  all_characters <- c(LETTERS, letters, 0:9, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '=', '+')
  remaining_length <- length - 4
  remaining_characters <- sample(all_characters, remaining_length, replace = TRUE)
  
  password <- c(uppercase, lowercase, digits, special_characters, remaining_characters)
  password <- sample(password) # Shuffle the characters
  
  return(paste(password, collapse = ""))
}

password_length <- as.integer(readline(prompt = "Enter the desired password length: "))
password <- generate_password(password_length)
cat("Generated password:", password, "\n")
