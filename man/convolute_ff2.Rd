\name{convolute_ff2}
\alias{convolute_ff2}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Convolute SPECIFIC set of featureContributions by corresponding features with kknn-package.
}

\description{
Only one convolution - but any kind. Whereas convolute_ff rather do batches of first order convolution.
}

\usage{
convolute_ff2(ff,
              Xi,
              FCi = NULL,
              k.fun=function() round(sqrt(n.obs)/2),
              userArgs.kknn = alist(kernel="gaussian")            )
}
\arguments{
  
\item{ff}{
  forestFloor object class "forestFloor_regression" or "forestFloor_multiClass" concisting of at least ff$X and ff$FCmatrix with two matrices of equal size
}
\item{Xi}{
  integer vector, of col indices of ff$X to convolute by
}
\item{FCi}{
 integer vector, of col indices of ff$FCmatrix. Those feature contributions to conbine(sum) and convolute. 
}

\item{k.fun}{
  function to define k-neighbors to concider. n.obs is a constant as number of observations in ff$X. Hereby k neighbors is defined as a function k.fun of n.obs. To set k to a constant use e.g. k.fun = function() 10. k can also be overridden with   userArgs.kknn = alist(kernel="gaussian",kmax=10).
}

\item{userArgs.kknn}{
  argument list to pass to train.kknn function for each convolution. See (link) kknn.args.
  arguments in this list have priority of any passed by default by this wrapper function.
  see argument merger append.overwrite.alists
  
}
  
}
\details{
convolute_ff2 is a wrapper of train.kknn from kknn package to convolute featureContributions by their corresponding features. The output depends on paremets passer 

}

\value{
an numeric vector of with the convoluted value for any observation
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
Yerror = 15 * rnorm(obs)
cor(Y,Y+Yerror)^2  #relatively noisy system
Y= Y+Yerror

#grow a forest, remeber to include inbag
rfo=randomForest(X,Y,keep.inbag=TRUE,ntree=1000,sampsize=800)

#obtain 
ff = forestFloor(rfo,X)

#convolute the interacting feature contributions by their feature to understand relationship
fc34_convoluted = convolute_ff2(ff,Xi=3:4,FCi=3:4,  #arguments for the wrapper
                  userArgs.kknn = alist(kernel="gaussian",k=25)) #arguments for train.kknn

#plot the joined convolution
plot3d(ff$X[,3],ff$X[,4],fc34_convoluted,
       main="convolution of two feature contributions by their own vaiables",
       #add some colour gradients to ease visualization
       #box.outliers squese all observations in a 2 std.dev box
       #univariately for a vector or matrix and normalize to [0;1]
       col=rgb(.7*box.outliers(fc34_convoluted), 
               .7*box.outliers(ff$X[,3]),        
               .7*box.outliers(ff$X[,4]))
       )
}
}

