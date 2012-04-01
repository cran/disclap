\name{ddisclap}
\alias{ddisclap}
\title{
Probability mass function of the Discrete Laplace distribution
}
\description{
Calculates the mass of observations from the Discrete Laplace distribution.
}
\usage{
ddisclap(d, p)
}
\arguments{
  \item{d}{the observation}
  \item{p}{the parameter with 0 < p < 1}
}
\details{
If \code{d} is a vector, then \code{p} must have either length 1 or the same length as \code{d}. If \code{p} has length 1, then the value will be reused for all values in \code{d}.
}
\value{
The probability of observing \code{d} in a DiscreteLaplace(\code{p}) distribution.
}
\author{
Mikkel Meyer Andersen <mikl@math.aau.dk> and Poul Svante Eriksen
}
\seealso{
\code{\link{DiscreteLaplace}}
}
\examples{
p <- 0.3
ds <- (-4):4
probd <- ddisclap(ds, p)
data.frame(ds, probd)
plot(ds, probd, type = "l", xlab = "d", ylab = "P(D = d; p)")
}

