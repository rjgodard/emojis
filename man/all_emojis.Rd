\name{all_emojis}
\alias{all_emojis}
\docType{data}
\title{
Emoji names, IDs, and categories
}
\description{
Names, IDs, and categories for all emojis in the emoji lexicon.
}
\usage{data("all_emojis")}
\format{
  A data frame with 1719 observations on the following 4 variables.
  \describe{
    \item{\code{emoji}}{a character vector containing emoji images}
    \item{\code{name}}{a character vector containing emoji names as assigned by Unicode}
    \item{\code{category}}{a character vector containing emoji categories as assigned by Unicode}
    \item{\code{number}}{a numeric vector containing emoji IDs, which are unique to this package}
  }
}
\details{
Includes emojis up to Unicode 12.1.
}
\source{
https://unicode.org/emoji/charts-12.1/emoji-list.html
}
\examples{
table(all_emojis$category)
}
\keyword{datasets}
