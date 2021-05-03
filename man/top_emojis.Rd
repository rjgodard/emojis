\name{top_emojis}
\alias{top_emojis}
\title{
Identifies the most common emojis in a text or collection of texts.
}
\description{
Identifies the most common emojis in either an input character vector or an input data frame containing emoji counts (e.g., from the count_emojis function).
}
\usage{
top_emojis(text = NULL, emoji_counts = NULL)
}
\arguments{
  \item{text}{
  An optional character vector.
}
\item{emoji_counts}{
  An optional argument containing a data frame with nrow = number of elements in the input character vector and ncol = number of emojis currently supported by the package (see all_emojis). This data frame can be produced by the count_emojis function.
}
}
\value{
Proudces a data frame with the number of occurrences of each emoji present in the input vector, sorted in descending order.
}
\references{
https://unicode.org/emoji/charts/full-emoji-list.html
}
\author{
Rebecca Godard
}
\examples{
top_emojis(c("Hello \U001f600", "How are you today? \U001f600",
"I am feeling happy \U001f600", "I am feeling sad \U001f622"))
Note: Emojis are not supported by the software used to generate package manuals, so
the unicode character codes are used instead.
}
