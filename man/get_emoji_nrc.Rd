\name{get_emoji_nrc}
\alias{get_emoji_nrc}
\title{
Run sentiment analysis on emojis.
}
\description{
Uses the [NRC Emoji Lexicon] to run sentiment analysis on emojis. Produces total scores on eight emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, trust) and two sentiments (negative, positive), taking into account emojis (but not words). Input can be either a character vector or a data frame containing emoji counts (e.g., from the count_emojis function).
}
\usage{
get_emoji_nrc(text = NULL, emoji_counts = NULL)
}
\arguments{
  \item{text}{
  A optional character vector.
}
\item{emoji_counts}{
  An optional argument containing a data frame with nrow = number of elements in the input character vector and ncol = number of emojis currently supported by the package (see all_emojis). This data frame can be produced by the count_emojis function.
}
}
\value{
Produces a data frame with scores for eight emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, trust) and two sentiments (negative, positive). Each column represents an emotion or sentiment, and each row represents an item in the input character vector.
}
\references{
[insert reference here]
}
\author{
Rebecca Godard
}
\seealso{
get_all_nrc
}
\examples{
get_emoji_nrc(c("I am feeling happy \U001f600", "I am feeling sad \U001f622"))
Note: Emojis are not supported by the software used to generate package manuals, so
the unicode character codes are used instead.
}
