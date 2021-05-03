\name{get_all_nrc}
\alias{get_all_nrc}
\title{
Run sentiment analysis on text containing emojis.
}
\description{
Uses the NRC Emotion Lexicon and the [NRC Emoji Lexicon] to run sentiment analysis on texts containing emojis. Produces total scores on eight emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, trust) and two sentiments (negative, positive), taking into account both words and emojis. Input can be either a character vector or a data frame containing emoji counts (e.g., from the count_emojis function).
}
\usage{
get_all_nrc(text, emoji_counts = NULL)
}
\arguments{
  \item{text}{
  A character vector.
}
\item{emoji_counts}{
  An optional argument containing a data frame with nrow = number of elements in the input character vector and ncol = number of emojis currently supported by the package (see all_emojis). This data frame can be produced by the count_emojis function.
}
}
\value{
Produces a data frame with scores for eight emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, trust) and two sentiments (negative, positive). Each column represents an emotion or sentiment, and each row represents an item in the input character vector.
}
\references{
Mohammad, S. M., & Turney, P. D. (2013). NRC emotion lexicon. National Research Council, Canada.
[insert emoji reference here]
}
\author{
Rebecca Godard
}
\seealso{
get_emoji_nrc()
}
\examples{
get_all_nrc(c("I am feeling happy \U001f600", "I am feeling sad \U001f622"))
Note: Emojis are not supported by the software used to generate package manuals, so
the unicode character codes are used instead.
}
