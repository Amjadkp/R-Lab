is_prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  if (n == 2) {
    return(TRUE)
  }
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

find_primes_in_range <- function(start, end) {
  primes <- c()
  for (n in start:end) {
    if (is_prime(n)) {
      primes <- c(primes, n)
    }
  }
  return(primes)
}

cat("1. Check if a single number is prime\n")
cat("2. Find all prime numbers in a range\n")
choice <- as.integer(readline(prompt = "Enter your choice (1 or 2): "))

if (choice == 1) {
  number <- as.integer(readline(prompt = "Enter a number to check if it is prime: "))
  if (is_prime(number)) {
    cat(number, "is a prime number.\n")
  } else {
    cat(number, "is not a prime number.\n")
  }
} else if (choice == 2) {
  start <- as.integer(readline(prompt = "Enter the start of the range: "))
  end <- as.integer(readline(prompt = "Enter the end of the range: "))
  primes <- find_primes_in_range(start, end)
  if (length(primes) > 0) {
    cat("Prime numbers in the range", start, "to", end, "are:\n")
    cat(primes, "\n")
  } else {
    cat("There are no prime numbers in the range", start, "to", end, ".\n")
  }
} else {
  cat("Invalid choice. Please enter 1 or 2.\n")
}
