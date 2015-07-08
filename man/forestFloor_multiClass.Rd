\name{forestFloor_multiClass}
\alias{forestFloor_multiClass}


\title{
ForestFloor: Visualize topologies of randomForest model
}
\description{
((multiClass BETA))
The function forestFloor computes a feature contribution matrix from a randomForest model-fit and outputs a forestFloor S3 class object, including importance and the orginal training set. The output object is the basis all visualizations.

}
\usage{
forestFloor_multiClass(rfo,X)
}

\arguments{
  \item{rfo}{
  only classification
  for bootstrap with replacement use: 
  rfo, random forest object is the output from randomForest::randomForest or cinbag::trimTrees \cr
  for bootstrap withou replacement use:
  rfo = cinbag(X,Y,keep.inbag=T,keep.forest=T,importance=T) \cr
  Formula interface is not supported. Y is a factor of a number of levels or numeric vector or n_row elements.
  }
  
  \item{X}{
  data.frame of input variables, numeric(continnous), descrete(treated as continous) or factors(categoric).
  n_rows observations and n_columns features
  X MUST be the same data.frame as used to train the random forest, see above item.
  }
}

\details{
  forestFloor computes feature contributions for random forest regression as suggest by Kuz'min et al, and for binaray classification as suggested by Palczewska et al. Feature contributions is the sums over all local increments for each observation for each feature divided by the number of trees. A local increment is the change of node prediction for given observation in one node being split to a subnode by a given feature. forestFloor use inbag samples to calculate local increments, but only sum local increments over out-of-bag samples divided with OOBtimes. OOBtimes is the number of times a given observation have out-of-bag which normally is ~ trees / 3. This implementation, can be said to yield cross-validated feature contributions. In practices this lowers the leaverage of any observation to the feature contributions of this observation. Hereby becomes the visulization less noisy. In systems with low or no noise, this implementation have no particular advantage.
}

\value{
  the forestFloor function outputs an object of class "forestFloor" with following elements:

\item{X}{
  a copy of the training data or feature space matrix/data.frame, X. The copy is passed from the input of this function. X is used in all visualization to expand the feature contributions over the features of which they were recorded.
}

\item{Y}{
  a copy of the target vector, Y.
}

\item{importance}{
  The gini-importance or permutation-importance a.k.a varaiable importance of the random forest object \cr
  if rfo=randomForest(X,Y,importance=FALSE), gini-importance is used. \cr
  gini-importance is less reproducible and more biased. The extra time used to compute permutation importance is     negliable. 
}

\item{imp_ind}{
  imp_ind, the importance indices is the order to sort the features by descending importance. imp_ind is used by plotting functions to present must relevant feature contributions first. If using gini-importance, the order of plots is more random and will favor continous variables. The plots themselves will not differ.
}

\item{FCmatrix}{
  feature contributions in a array. Dimensions are: n_row class x samples x features. Values can be interpreted as the absolute change of predicted probability for given class for a given sample due to the information of a given feature.
}

}

\references{
Interpretation of QSAR Models Based on Random Forest Methods, http://dx.doi.org/10.1002/minf.201000173 \cr
Interpreting random forest classification models using a feature contribution method, http://arxiv.org/abs/1312.1121 \cr
}

\author{
Soren Havelund Welling
}

\note{
this version 
}

\seealso{
\code{\link{plot.forestFloor}}, \code{\link{show3d_new}}
}

\examples{
\dontrun{
## this example only tests feature contributions are calculated correctly
library(randomForest)
library(forestFloor)
require(utils)

data(iris)
iris
X = iris[,!names(iris) %in% "Species"]
Y = iris[,"Species"]
as.numeric(Y)
rf = randomForest(X,Y,keep.forest=T,replace=F,keep.inbag=T)
ff = forestFloor_multiClass(rf,X)
pred = sapply(1:3,function(i) apply(ff$FCmatrix[i,,],1,sum))+1/3
rfPred = predict(rf,type="vote",norm.votes=T)
rfPred[is.nan(rfPred)] = 1/3
if(cor(as.vector(rfPred),as.vector(pred))^2<0.99) stop("fail testMultiClass")
}
}

\keyword{ multivariate }
\keyword{ models }
\keyword{ nonlinear }
\keyword{ robust }