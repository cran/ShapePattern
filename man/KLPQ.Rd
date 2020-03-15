\name{KLPQ}
\alias{KLPQ}
\title{
Computes and returns the Kullback-Leibler divergence between two probability distributions
}

\description{
Given two probability distributions (vectors) of the same length, this function computes the Kullback-Leibler divergence (relative entropy) between them. If any entries in the distribution are 0, then the argument JITTER can be used to add a tiny offset. 
}

\usage{
KLPQ(P = c(0.36, 0.48, 0.16), Q = c(0.333, 0.333, 0.333), JITTER = 0.0)
}

\arguments{
  \item{P}{
A first probability distribution vector.
}
  \item{Q}{
A second probability distribution vector.
}
  \item{JITTER}{
An optional tiny value to be added to the probabilities to avoid non-zero entries (e.g., 0.000000001).
}
}

\value{
The function returns a numeric value that is the Kullback-Leibler divergence. If this value is 0 (zero), then the two distributions are identical.
}

\references{
None currently.
}

\author{
Tarmo K. Remmel
}

\note{
Used to provide a uni-directional divergence measure. Note that P||Q does not necessarily equal Q||P.
}

\seealso{
See Also \code{\link{patternbits}}
}

\examples{
KLPQ(P = c(0.36, 0.48, 0.16), Q = c(0.333, 0.333, 0.333), JITTER = 0.0)
}

\keyword{ manip }
