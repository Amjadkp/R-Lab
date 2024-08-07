reverse_list <- function(lst) {
  if (length(lst) == 0) {
    return(lst)  # Base case: empty list
  } else {
    return(c(rev_tail(lst), head(lst, 1)))
  }
}

rev_tail <- function(lst) {
  if (length(lst) == 1) {
    return(lst)
  } else {
    return(reverse_list(tail(lst, -1)))
  }
}

input_list <- as.list(readline(prompt = "Enter elements of the list separated by spaces: "))
input_list <- unlist(strsplit(input_list, " "))
reversed_list <- reverse_list(input_list)
cat("Reversed list:", paste(reversed_list, collapse = " "), "\n")
