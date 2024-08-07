caesar_shift <- function(char, shift) {
  if (grepl("[a-zA-Z]", char)) {
    if (char %in% LETTERS) {
      base <- utf8ToInt('A')
    } else {
      base <- utf8ToInt('a')
    }
    
    shifted_char <- intToUtf8(((utf8ToInt(char) - base + shift) %% 26) + base)
    return(shifted_char)
  } else {
    return(char)
  }
}

encrypt_sentence <- function(sentence, shift) {
  encrypted_sentence <- sapply(strsplit(sentence, NULL)[[1]], caesar_shift, shift)
  return(paste(encrypted_sentence, collapse = ""))
}

sentence <- readline(prompt = "Enter a sentence to encrypt: ")
shift <- as.integer(readline(prompt = "Enter the shift value: "))

encrypted_sentence <- encrypt_sentence(sentence, shift)

cat("Encrypted sentence:", encrypted_sentence, "\n")
