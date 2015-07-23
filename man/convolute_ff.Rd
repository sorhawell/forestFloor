\name{convolute_ff}
\alias{convolute_ff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convolution SPECIFIC set of featureContributions by corresponding features with kknn-package}
\description{
Can do any order of convolution, whereas convolute_ff rather do batches of first order convolution.
With kNN- gaussian kernel and LOO cross-validation.
}
\usage{
convolute_ff(ff,
             these.vars=NULL,
             k.fun=function() round(sqrt(n.obs)/2),
             userArgs.kknn = alist(kernel="gaussian"))
}
\arguments{
  
\item{ff}{
  forestFloor object "forestFloor_regression" or "forestFloor_multiClass"  concisting of at least ff$X and ff$FCmatrix with two matrices of equal size
}
\item{these.vars}{
  vector of col.indices to ff$X. Convolution can be limited to these.vars 
}
\item{k.fun}{
  function to define k-neighbors to concider. n.obs is a constant as number of observations in ff$X. Hereby k neighbors is defined as a function k.fun of n.obs. To set k to a constant use e.g. k.fun = function() 10. k can also be overridden with   userArgs.kknn = alist(kernel="gaussian",kmax=10).
}
\item{userArgs.kknn}{
  argument list to pass to train.kknn function for each convolution. See (link) kknn.args.
Conflicting arguments to this list will be overridden e.g. k.fun.
}

  
}
\details{
convolute_ff uses train.kknn from kknn package to convolute featureContributions by their corresponding varialbles. The output inside a ff$FCfit will resemble ff$Fmatrix for any coloumn/variable which is well explained by its main effect. Guassian weighting of nearest neighbors lowers bias of the fit.

}

\value{
ff$FCfit, a matrix of convoluted featureContributions
}

\author{
Soren Havelund Welling
}

\examples{
\dontrun{
#simulate data
obs=2500
vars = 6 
X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + 2*sin(X2*pi) + 8 * X3 * X4)
Yerror = 5 * rnorm(obs)
cor(Y,Y+Yerror)^2
Y= Y+Yerror

#grow a forest, remeber to include inbag
rfo=randomForest(X,Y,keep.inbag=TRUE,ntree=1000,sampsize=800)

ff = forestFloor(rfo,X)

ff = convolute_ff(ff)

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

