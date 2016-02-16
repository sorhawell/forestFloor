\name{Xtestmerger}
\alias{Xtestmerger}

\title{
merge training set (X) and (test) set
}

\description{
... and expand inbag matrix and training target vector to compute FC for a test set.
}
\usage{
Xtestmerger(X,test,inbag=NULL,y=NULL)
}

\arguments{
\item{X}{
X , training set data.frame used to train a random forest model
}
\item{test}{
test, a test set data.frame which feature contributions should be computed for
}
\item{inbag}{
matrix of inbag sampling to expande with training set, which is set OOB for any tree
}
\item{y}{
random forest target vector, which is set to first value for observation
}

}

\details{
Xtestmerger is a low-level function to merge a test set with X training set. There can be no names, column class, column number mismatch. Moreover any level in any factor of test must be present in X, as RF/forestFloor cannot score a unknown factor level / category.
}

\value{
List of merged bigX, bigInbag and bigy. The two latter may be NULL if not provided.
}

\author{
Soren Havelund Welling
}

\examples{

library(randomForest)
library(forestFloor)
#X y could be a training set
X = data.frame(numeric = c(1,5,2,7,-4.3),
               factor1 = factor(c("jim","freddy","marley","marley","alfred")),
               factor2 = factor(c("jill","ann","liz","leila","vicky")))
y = factor(1:5)
set.seed(1)
rf = randomForest(X,y,keep.inbag=TRUE,ntree=7)
#should not raise any error
test = data.frame(numeric = rnorm(5),
                  factor1 = factor(c("jim","jim","jim","freddy","freddy")),
                  factor2 = factor(c("jill","jill","vicky","leila","vicky"))
                  )
out = Xtestmerger(X,test,inbag=rf$inbag,y=y)


}

