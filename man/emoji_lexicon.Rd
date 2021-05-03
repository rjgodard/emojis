\name{emoji_lexicon}
\alias{emoji_lexicon}
\docType{data}
\title{
Emoji NRC Lexicon
}
\description{
Contains ratings of 1719 emojis on eight emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, trust) and two sentiments (negative, positive). Contains emojis up to and including Unicode 12.1.0.
}
\usage{data("all_emojis")}
\format{
  A data frame with 1719 observations on the following 11 variables.
  \describe{
    \item{\code{emoji}}{a character vector containing emoji images}
    \item{\code{anger}}{a numeric vector containing anger scores}
    \item{\code{anticipation}}{a numeric vector containing anticipation scores}
    \item{\code{disgust}}{a numeric vector containing disgust scores}
    \item{\code{fear}}{a numeric vector containing fear scores}
    \item{\code{joy}}{a numeric vector containing joy scores}
    \item{\code{sadness}}{a numeric vector containing sadness scores}
    \item{\code{surprise}}{a numeric vector containing surprise scores}
    \item{\code{trust}}{a numeric vector containing trust scores}
    \item{\code{negative}}{a numeric vector containing negative scores}
    \item{\code{positive}}{a numeric vector containing positive scores}
  }
}
\source{
[insert publication here]
}
\references{
Mohammad, S. M., & Turney, P. D. (2013). NRC emotion lexicon. National Research Council, Canada.
}
\examples{
head(emoji_lexicon)
}
\keyword{datasets}
