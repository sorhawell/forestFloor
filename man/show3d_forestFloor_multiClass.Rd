\name{show3d_forestFloor_multiClass}
\alias{show3d_forestFloor_multiClass}
\title{Metod: show3d_forestFloor_multiClass}
\description{Method to plot3d an object of forestFloor_multiClass}

\usage{
show3d_forestFloor_multiClass(x,Xvars,FCvars=NULL,label.seq=NULL,
user.grid.args=list(NULL),user.rgl.args=list(),compute_GOF=FALSE,
user.gof.args=list(NULL))
}

\arguments{
\item{x}{
foretFloor-object, also abbrivated ff.
Computed topology of randomForest-model, the output from the forestFloor function \cr
includes feature contributions, X and Y and importance data.
}
\item{Xvars}{
a vector of two elements, being the columns to plot ordered by training matrix
}
\item{FCvars}{
a vector of one or more elements, being the columns to plot ordered by feature contribution matrix. If multiple columns are chosen the row sum of columns will be plotted.
}
\item{label.seq}{
a numeric vector describing which classes and in what sequence to plot. NULL is all classes ordered is in levels in x$Y of forestFloor_mulitClass object x. 
}

\item{user.grid.args}{
list of arguments passed to internal function convolute_grid which again passes arguments to LOO-CV knearest neighbor - gaussion kernal estimation. Often needed to change anything here. The grid concist of a number of anchor points. eg. 30X30 which will represent a surface fit to the visualization. Each anchor points is fitted with kNN-gaussion. See documentaion for function convolute_grid.
}
\item{user.rgl.args}{
Argument list, any argument to modify the points, labels etc. can be passed through here.
}

\item{compute_GOF}{
Booleen TRUE/FALSE. Should the goodness of fit be computed? If false, no GOF input pars are useful.
}

\item{user.gof.args}{
argument list passed to internal function ff2, which can modify how goodness-of-fit is computed. Number of neighobers and kernel can be set manually with e.g. list(kmax=40,kernel="gaussion"). Default pars should work already in most cases. Function ff2 computed leave-one-out CV prediction the feature contributions from the chosen context of the visualization.
}

}

\details{
The method plot3d.forestFloor_multiClass visualizes partial a 3dplot of feature contributions(summed together of more than one column) in the context of two variables. Any class will be cloured separetly and one surface will be fitted to each class.
Is this partial function(PF) a fair generalization or subject to global or local interactions. \cr

}

\author{
Soren Havelund Welling
}



\examples{
\dontrun{
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
pred = sapply(1:3,function(i) apply(ff$FCarray[,,i],1,sum))+1/3
rfPred = predict(rf,type="vote",norm.votes=T)
rfPred[is.nan(rfPred)] = 1/3
if(cor(as.vector(rfPred),as.vector(pred))^2<0.99) stop("fail testMultiClass")
attributes(ff)
plot3d(ff) 

}
}
\keyword{ multivariate }
\keyword{ models }
\keyword{ nonlinear }
\keyword{ robust }
