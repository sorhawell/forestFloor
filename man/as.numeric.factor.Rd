\name{as.numeric.factor}
\alias{as.numeric.factor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a factor to numeric.vector.
}

\description{
Internal function which will drop unused levels and convert remaining to a number from 1 to n levels.
}
\usage{
as.numeric.factor(x,drop.levels=TRUE)
}

\arguments{
\item{x}{
  noramlly a factor, can be a numeric vector(will be outputted unchanged)
}
\item{drop.levels}{
 booleen,  should unused levels be droppped?
}
}

\details{
simple internal function
}

\value{
a vector of same length, where each category/level is replaced with number from 1 to n
}

\author{
Soren Havelund Welling
}

\examples{
as.numeric.factor = forestFloor:::as.numeric.factor #import to environment
some.factor = factor(c("dog","cat","monkey")[c(1,3,2,1,3,2,1,1)]) #make factor
a.numeric.vector = as.numeric.factor(some.factor) #convert factor representation.

}

