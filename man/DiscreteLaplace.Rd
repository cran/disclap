\name{DiscreteLaplace}
\alias{DiscreteLaplace}
\title{
Discrete Laplace Family
}
\description{
Discrete Laplace Family for models such as a generalized linear model.
}
\usage{
DiscreteLaplace()
}
\details{
This family can be used in for example fitting a generalized linear model using the \code{\link{glm}} or \code{\link{glm.fit}} function.
}
\value{
See \code{\link{binomial}} or \code{\link{poisson}}
}
\author{
Mikkel Meyer Andersen <mikl@math.aau.dk> and Poul Svante Eriksen
}

\seealso{
\code{\link{glm}}
\code{\link{glm.fit}}
\code{\link{ddisclap}}
\code{\link{binomial}}
\code{\link{poisson}}
}
\examples{
fit <- glm(rpois(10, 1) ~ 1, family = DiscreteLaplace())
summary(fit)
}

