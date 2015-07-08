\name{plot.forestFloor_multiClass}
\alias{plot.forestFloor_multiClass}
\title{
Metod: plot.forestFloor_multiClass
}
\description{
Method to plot an object of forestFloor_multiClass.
}
\usage{
\method{plot}{forestFloor_multiClass} (x,
plot_seq = NULL, label.seq = NULL, limitY = TRUE,colLists = NULL,
order_by_importance = TRUE, fig.columns = NULL, plot_GOF = FALSE,
GOF_col = NULL, jitter_these_cols = NULL, jitter.factor = NULL, compute_GOF = F, ...) 
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
foretFloor-object, also abbrivated ff..
Computed topology of randomForest-model, the output from the forestFloor function \cr
includes also X and Y and importance data
}

\item{plot_seq}{
a numeric vector describing which variables and in what sequence to plot,
ordered by importance as default, order_by_importance = F then by feature/coloumn order of training data.
}

\item{label.seq}{
a numeric vector describing which classes and in what sequence to plot. NULL is all classes ordered is in levels in x$Y of forestFloor_mulitClass object x. 
}


\item{limitY}{
TRUE/FLASE, constrain all Yaxis to same limits to ensure relevance of low importance features is not overinterpreted
}

\item{colLists}{
List of colour vectors of label.seq length. Each element is a colour vector colouring sample class prediction of one class. Vectors should either be of length 1 with one colour for class predictions or of length equal to number of training observations designating colours for all samples. NULL will choose standard one colour per class.
}

\item{order_by_importance}{
TRUE / FALSE
should plotting and plot_seq be ordered after importance. Most important feature plot first(TRUE)
}

\item{fig.columns}{
for multi plotting, how many columns per page.
default(NULL) is 1 for one plot, 2 for 2, 3 for 3, 2 for 4 and 3 for more. 
}


\item{plot_GOF}{
TRUE/FALSE should a kNN-gaussian fit be plotted? (see compute_GOF)
}
 
\item{GOF_col}{
vector colours of goodness of fits, one per class
}


\item{jitter_these_cols}{
vector to apply jitter to x-axis in plots. Will refer to variables. Useful to for categorical variables. Default=NULL is no jitter.
}


\item{jitter.factor}{
value to decide howmuch jitter to apply. often between .5 and 3
}

\item{compute_GOF}{
Booleen TRUE/FALSE. Should the goodness of fit be computed? If false, no GOF input pars are useful.
}

\item{...}{
... other arguments passed to generic plot functions
}

}
\details{
The method plot.forestFloor_multiClass visualizes partial plots of the most important variables first. Partial dependence plots are available in the randomForest package. But such plots are single lines(1d-slices) and do not answer the question:
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
plot(ff) 

}
}
\keyword{ multivariate }
\keyword{ models }
\keyword{ nonlinear }
\keyword{ robust }
