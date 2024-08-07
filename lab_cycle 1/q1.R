install.packages("stringr")

library(stringr)
analyze_text<-function(text,word_to_replace,replacement_word){
  text<-as.character(text)
  word_to_replace<-as.character(word_to_replace)
  replacement_word<-as.character(replacement_word)
  
  words<-unlist(strsplit(text,"\\s+"))
  total_words<-length(words)
  
  word_lengths<-nchar(words)
  average_word_length<-mean(word_lengths)
  longest_word<-words[which.max(word_lengths)]
  
  modified_text<-str_replace_all(text,word_to_replace,replacement_word)
  
  cat("total no of words : ",total_words,"\n")
  cat("average word length in the text : ",average_word_length,"\n")
  cat("the longest word in the text : ",longest_word,"\n")
  cat("the newly modified text : \n",modified_text,"\n")
}
text<-readline(prompt = "enter the text : ")
word_to_replace<-readline(prompt = "enter the word to replace : ")
replacement_word<-readline(prompt = "enter the repplacement word : ")

analyze_text(text,word_to_replace,replacement_word)

