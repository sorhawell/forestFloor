\name{box.outliers}
\alias{box.outliers}

\title{
Box Outliers
}
\description{
squeeze all outliers onto standard.dev-limits and/or normalize to [0;1] scale
}
\usage{
box.outliers(x, limit = 1.5, normalize = TRUE)
}
\arguments{
  \item{x}{
numeric vector, matrix, array, data.frame
}
  \item{limit}{
limit(SD,standard diviation) any number devating more than limit from mean is an outlier 
}

\item{normalize}{
TRUE/FALSE
should output range be normalized to [0;1]?
}


}
\details{
Can be used to box high dimensional data, hence the name box.outliers. Box.outliers is used internally in forestFloor-package to compute colour gradients without assigning unique colours to few outliers. It's a box because the borders univariate/non-interacting.
}
\value{
matrix(n x p) of normalized values
}

\author{
Soren Havelund Welling, 2014
}

\seealso{
scale()
}

\examples{
#Example:
box.outliers = function (x, limit = 1.5)  {
  x = scale(x)
  x[ x > limit] =  limit
  x[-x > limit] = -limit
  x = x - min(x)
  x = x/(limit * 2)
  return(x)
}
n=1000 #some observations
p = 5  #some dimensions
X = data.frame(replicate(p,rnorm(n))) # a dataset
Xboxed =box.outliers(X,limit=1.5) #applying normalizatino
plot(Xboxed[,1],Xboxed[,2],col="#00000088") #plot output for first two dimensions
}

\keyword{outlier.filtration}
