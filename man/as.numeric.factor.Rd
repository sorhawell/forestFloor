\name{as.numeric.factor}
\alias{as.numeric.factor}
\title{
Convert a factor to numeric.vector.
}

\description{
Internal function which will drop unused levels and convert remaining to a number from 1 to n.levels.
}
\usage{
as.numeric.factor(x,drop.levels=TRUE)
}

\arguments{
\item{x}{
Normally a factor, can be a numeric vector(will be output unchanged)
}

\item{drop.levels}{
Boolean,  should unused levels be dropped?
}

}

\details{
Simple internal function, used to direct categorical variables to a 1 dimensional scale.
}

\value{
A vector of same length, where each category/level is replaced with number from 1 to n
}

\author{
Soren Havelund Welling
}

\examples{
as.numeric.factor = forestFloor:::as.numeric.factor #import to environment
some.factor = factor(c("dog","cat","monkey")[c(1,3,2,1,3,2,1,1)]) #make factor
a.numeric.vector = as.numeric.factor(some.factor) #convert factor representation.

}

