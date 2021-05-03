get_emoji_nrc <- function(text = NULL, emoji_counts = NULL) {
  if (is.null(text)==TRUE & is.null(emoji_counts)==TRUE) {
    stop("You must provide either a text or emoji count input.")
  }
  if (is.null(text)==FALSE) {
    if (is.character(text)==FALSE) {
      stop("Input must be a character vector")
    }
    emoji_counts <- count_emojis(text)
  }
  output <- data.frame(matrix(nrow=nrow(emoji_counts), ncol=10))
  for (i in 1:nrow(output)) {
    for (j in 1:10) {
      output[i,j] <- sum(emoji_counts[i,]*emoji_lexicon[,(j+3)])
    }
  }
  colnames(output) <- c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surpise", "Trust", "Negative", "Positive")
  return(output)
}
