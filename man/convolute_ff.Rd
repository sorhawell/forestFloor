\name{convolute_ff}
\alias{convolute_ff}
\title{
Cross-validated main effects interpretation for all feature contributions.}

\description{
convolute_ff estimates feature contributions of each feature separately as a function of the corresponding variable/feature. The estimator is a k-nearest neighbor function with Gaussian distance weighting and LOO cross-validation see \code{\link{train.kknn}}.
}

\usage{
convolute_ff(ff,
             these.vars=NULL,
             k.fun=function() round(sqrt(n.obs)/2),
             userArgs.kknn = alist(kernel="gaussian"))
}
\arguments{
  
\item{ff}{
  forestFloor object "forestFloor_regression" or "forestFloor_multiClass"  consisting of at least ff$X and ff$FCmatrix with two matrices of equal size
}
\item{these.vars}{
  vector of col.indices to ff$X. Convolution can be limited to these.vars 
}
\item{k.fun}{
  function to define k-neighbors to consider. n.obs is a constant as number of observations in ff$X. Hereby k neighbors is defined as a function k.fun of n.obs. To set k to a constant use e.g. k.fun = function() 10. k can also be overridden with   userArgs.kknn = alist(kernel="Gaussian",kmax=10).
}
\item{userArgs.kknn}{
  argument list to pass to train.kknn function for each convolution. See (link) kknn.args.
Conflicting arguments to this list will be overridden e.g. k.fun.
}

  
}
\details{
convolute_ff uses train.kknn from kknn package to estimate feature contributions by their corresponding variables. The output inside a ff$FCfit will have same dimensions as ff$FCmatrix and the values will match quite well if the learned model structure is relative smooth and main effects are dominant. This function is e.g. used to estimate fitted lines in plot.forestFloor function "plot(ff,...)". LOO cross validation is used to quantify how much of feature contribution variation can be explained as a main effect.
}

\value{
ff$FCfit a matrix of predicted feature contributions has same dimension as ff$FCmatrix. The output is appended to the input "forestFloor" object as $FCfit.}

\author{
Soren Havelund Welling
}

\examples{
\dontrun{
library(forestFloor)
library(randomForest)

#simulate data
obs=1000
vars = 6 
X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + 2*sin(X2*pi) + 8 * X3 * X4)
Yerror = 5 * rnorm(obs)
cor(Y,Y+Yerror)^2
Y= Y+Yerror

#grow a forest, remeber to include inbag
rfo=randomForest(X,Y,keep.inbag=TRUE)

ff = forestFloor(rfo,X)

ff = convolute_ff(ff) #return input oject with ff$FCfit included

#the convolutions correlation to the feature contribution
for(i in 1:6) print(cor(ff$FCmatrix[,i],ff$FCfit[,i])^2)

#plotting the feature contributions 
pars=par(no.readonly=TRUE) #save graphicals
par(mfrow=c(3,2),mar=c(2,2,2,2))
for(i in 1:6) {
  plot(ff$X[,i],ff$FCmatrix[,i],col="#00000030",ylim=range(ff$FCmatrix))
  points(ff$X[,i],ff$FCfit[,i],col="red",cex=0.2)
}
par(pars) #restore graphicals
}
}

