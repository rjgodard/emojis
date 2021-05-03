count_emojis <- function(text) {
  if (is.character(text)==FALSE) {
    stop("Input must be a character vector")
  }
  output <- data.frame(matrix(nrow=length(text), ncol=nrow(all_emojis)))
  colnames(output) <- c(1:ncol(output))
  for (i in 1:nrow(output)) {
    for (j in 1:ncol(output)) {
      output[i,j] <- stringr::str_count(text[i], pattern = all_emojis$emoji[j])
    }
  }
  colnames(output) <- c(all_emojis$name)
  return(output)
}
