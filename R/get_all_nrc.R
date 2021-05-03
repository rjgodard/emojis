get_all_nrc <- function(text, emoji_counts = NULL) {
  if (is.character(text)==FALSE) {
    stop("Input must be a character vector")
  }
  if (is.null(text)==FALSE) {
    emoji_counts <- count_emojis(text)
  }
  emoji_scores <- data.frame(matrix(nrow=nrow(emoji_counts), ncol=10))
  for (i in 1:nrow(emoji_scores)) {
    for (j in 1:10) {
      emoji_scores[i,j] <- sum(emoji_counts[i,]*emoji_lexicon[,(j+3)])
    }
  }
  nrc_scores <- syuzhet::get_nrc_sentiment(text)
  output <- data.frame(matrix(nrow=nrow(nrc_scores), ncol=ncol(nrc_scores)))
  for (i in 1:nrow(output)) {
    for (j in 1:ncol(output)) {
      output[i,j] <- emoji_scores[i,j] + nrc_scores[i,j]
    }
  }
  colnames(output) <- c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surpise", "Trust", "Negative", "Positive")
  return(output)
}
