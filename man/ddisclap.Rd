\name{Discrete Laplace distribution}
\alias{ddisclap}
\alias{pdisclap}
\alias{rdisclap}
\title{
Probability mass function, distribution function, and random generation for the discrete Laplace distribution with parameter \eqn{0 <= p < 1}.
}
\description{
Calculates the mass of observations from the discrete Laplace distribution.
}
\usage{
ddisclap(x, p)
pdisclap(x, p, lower.tail = TRUE)
rdisclap(n, p)
}
\arguments{
  \item{x}{vector of observations}
  \item{p}{the parameter with 0 <= p < 1}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].}
  \item{n}{number of observations to generate}
}
\details{
The probability mass function for the discrete Laplace distribution with parameter \eqn{0 < p < 1} is given by \eqn{P(X = x) = \frac{1-p}{1+p} p^x}{P(X = x) = ((1-p)/(1+p))*p^x} for \eqn{x \in \mathbf{Z}}{x = ..., -1, 0, 1, ...}.

If \code{x} is a vector, then \code{p} must have either length 1 or the same length as \code{x}. If \code{p} has length 1, then the value will be reused for all values in \code{x}.
}
\value{
'ddisclap' gives the probability mass, 'pdisclap' gives the distribution function, and 'rdisclap' generates random deviates.
}
\author{
Mikkel Meyer Andersen <mikl@math.aau.dk> and Poul Svante Eriksen
}
\seealso{
\code{\link{DiscreteLaplace}}
}
\examples{
p <- 0.3
xs <- (-4):4
probd <- ddisclap(xs, p)
data.frame(xs, probd)
plot(xs, probd, type = "l", xlab = "x", ylab = "P(X = x; p)")
}

