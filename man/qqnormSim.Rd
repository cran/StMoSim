\name{qqnormSim}
\alias{qqnormSim}
\alias{qqnormSim,lm-method}
\alias{qqnormSim,numeric-method}

\title{
Simulate QQ-Norm-Plot
}
\description{
Simulate QQ-Norm-Plot
}

\usage{
\S4method{qqnormSim}{lm}(x, n_sim = 500)
\S4method{qqnormSim}{numeric}(x, n_sim = 500)
}

\arguments{
  \item{x}{is a lm-object or a numeric vector. If it is an lm-object its residuals are plotted.}
  \item{n_sim}{is an optional argument. If you like to have more or less than 500 simulations you can specify this parameter.}
}

\keyword{ qqnorm }

