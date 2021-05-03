\name{lookup_emojis}
\alias{lookup_emojis}
\title{
Looks up names, IDs, and/or categories of emojis.
}
\description{
Looks up names, IDs, and/or categories of emojis based on an input of either names, IDs, or emojis.
}
\usage{
lookup_emojis(input, type)
}
\arguments{
  \item{input}{
  A character or integer vector, depending on the type argument.
}
  \item{type}{
  Either "id" (for integer IDs), "emoji" (for emojis), or "name" (for Unicode-assigned names).
}
}
\details{
Names and categories are assigned by Unicode. IDs are unique to the emoji package. Updated as of Unicode 12.1.
}
\value{
Produces a data frame with four columns for each emoji in the input vector - emoji, name, ID, and category.
}
\references{
https://unicode.org/emoji/charts/full-emoji-list.html
}
\author{
Rebecca Godard
}
\seealso{
all_emojis
}
\examples{
lookup_emojis(input = c(1:10), type = "id")
lookup_emojis(input = c("\U001f600", "\U002764", "\U001f4aa"), type = "emoji")
Note: Emojis are not supported by the software used to generate package manuals, so
the unicode character codes are used instead.
lookup_emojis(input = c("grinning cat", "hundred points", "monkey face"), type = "name")
}
