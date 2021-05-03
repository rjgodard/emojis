lookup_emojis <- function(input, type) {
  output <- data.frame(matrix(nrow=length(input), ncol=3))
  colnames(output) <- c("emoji", "id", "name")
  if (type=="emoji") {
    output$emoji <- input
    for (i in 1:length(input)) {
      output$id[i] <- all_emojis$number[which(all_emojis$emoji==input[i])]
      output$name[i] <- all_emojis$name[which(all_emojis$emoji==input[i])]
    }
  }
  if (type=="id") {
    output$id <- input
    for (i in 1:length(input)) {
      output$emoji[i] <- all_emojis$emoji[which(all_emojis$number==input[i])]
      output$name[i] <- all_emojis$name[which(all_emojis$number==input[i])]
    }
  }
  if (type=="name") {
    input <- tolower(input)
    output$name <- input
    for (i in 1:length(input)) {
      output$id[i] <- all_emojis$number[which(all_emojis$name==input[i])]
      output$emoji[i] <- all_emojis$emoji[which(all_emojis$name==input[i])]
    }
  }
  return(output)
}
