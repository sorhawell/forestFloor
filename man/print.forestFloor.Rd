\name{print.forestFloor}
\alias{print.forestFloor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print summary of forestFloor.Object
}
\description{
summary comprise N_fold I_repeated cross validation statistics and aggragated importance of variables.
a forestFloor_Object is of class forestFloor and is the output of the forestFloor function.
}
\usage{
\method{print}{forestFloor}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  x also abbrivated ff, forestFloor_Object the output from the forestFloor function

}

  \item{...}{
  ... other arguments passed to generic print function

}

}
\details{
prints short help text for usage of a forestFloor_object
}

\author{
Soren Havelund Welling
}


\examples{
\dontrun{
#simulate data
obs=1000 
vars = 6 
X = data.frame(replicate(vars,rnorm(obs))) 
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 0.5 * rnorm(obs)) 

#grow a forest, remeber to include inbag
rfo=randomForest::randomForest(X,Y,keep.inbag=TRUE)

#compute topology
ff = forestFloor(rfo,X)

#print forestFloor
print(ff) 
}
}