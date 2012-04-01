\name{disclap-package}
\alias{disclap-package}
\alias{disclap}
\docType{package}
\title{Discrete Laplace Family}
\description{Discrete Laplace Family for models such as a generalized linear model.}
\details{
\tabular{ll}{
Package: \tab disclap\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-04-01\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
DiscreteLaplace()
ddisclap(d, p)
}
\author{
Mikkel Meyer Andersen and Poul Svante Eriksen

Maintainer: Mikkel Meyer Andersen <mikl@math.aau.dk>
}
\keyword{package}
\seealso{
\code{\link{DiscreteLaplace}}
\code{\link{ddisclap}}
}
\examples{
fit <- glm(rpois(10, 1) ~ 1, family = DiscreteLaplace())
summary(fit)
}

