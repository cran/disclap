\name{Discrete Laplace exponential family}
\alias{DiscreteLaplace}
\title{
Discrete Laplace exponential family
}
\description{
Discrete Laplace exponential family for models such as a generalized linear model.
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
xs <- abs(rdisclap(100, 0.1))
fit <- glm(xs ~ 1, family = DiscreteLaplace())
summary(fit)
theta <- as.numeric(coef(fit)[1])
mu <- DiscreteLaplace()$linkinv(theta)
p <- (sqrt(1 + mu^2) - 1) / mu
p
}

