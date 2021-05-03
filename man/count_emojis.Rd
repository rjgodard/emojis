\name{count_emojis}
\alias{count_emojis}
\title{
Count emojis in character vectors
}
\description{
Function to count the number of each emoji in each item in a character vector.
}
\usage{
count_emojis(text)
}
\arguments{
  \item{text}{
  A character vector.
}
}
\value{
A data frame with rows representing items in the input character vector and columns representing emojis. Each value in the data frame reflects the number of one emoji (column) in one element of the input character vector (row).
}

\author{
Rebecca Godard
}
\examples{
count_emojis(c("Hello \U001f600", "Hello \U001f622"))
Note: Emojis are not supported by the software used to generate package manuals, so
the unicode character codes are used instead.
}
