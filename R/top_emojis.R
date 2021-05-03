top_emojis <- function(text = NULL, emoji_counts = NULL) {
  if (is.null(text)==TRUE & is.null(emoji_counts)==TRUE) {
    stop("You must provide either a text or emoji count input.")
  }
  if (is.null(text)==FALSE) {
    if (is.character(text)==FALSE) {
      stop("Input must be a character vector")
    }
    counts <- count_emojis(text)
  } else {
    counts <- emoji_counts
  }
  sums <- data.frame(matrix(nrow=nrow(all_emojis), ncol=2))
  for (i in 1:nrow(sums)) {
    sums[i,1] <- all_emojis[i,2]
    sums[i,2] <- sum(counts[,i])
  }
  sums_nonzero <- sums[which(sums[,2]!=0),]
  colnames(sums_nonzero) <- c("Emoji", "Number")
  output <- sums_nonzero[order(sums_nonzero$Number, decreasing = TRUE),]
  return(output)
}
