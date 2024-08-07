validate_age <- function(age) {
  return(!is.na(as.integer(age)) && as.integer(age) > 0)
}

validate_grade <- function(grade) {
  valid_grades <- c("A", "B", "C", "D", "F")
  return(grade %in% valid_grades)
}

read_student_records <- function() {
  student_records <- list()
  continue <- TRUE
  
  while (continue) {
    name <- readline(prompt = "Enter student's name (or type 'done' to finish): ")
    if (tolower(name) == 'done') {
      break
    }
    
    age <- readline(prompt = "Enter student's age: ")
    grade <- readline(prompt = "Enter student's grade (A, B, C, D, F): ")
    
    if (validate_age(age) && validate_grade(grade)) {
      student_records[[length(student_records) + 1]] <- list(
        name = name,
        age = as.integer(age),
        grade = grade
      )
      cat("Record added successfully!\n")
    } else {
      cat("Invalid input. Please try again.\n")
    }
  }
  
  return(student_records)
}

calculate_average_age <- function(records) {
  if (length(records) == 0) {
    cat("No valid records to calculate average age.\n")
    return(NULL)
  }
  
  total_age <- sum(sapply(records, function(record) record$age))
  average_age <- total_age / length(records)
  
  cat("Average age of valid student records:", average_age, "\n")
}

student_records <- read_student_records()
calculate_average_age(student_records)
