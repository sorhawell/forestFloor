\name{plot.forestFloor}
\alias{plot.forestFloor}
\title{
Metod: plot.forestFloor
}
\description{
Method to plot an object of forestFloor-class. Plot partial feature contributions of the most important variables. Colour gradients can be applied two show possible interactions.
}
\usage{
\method{plot}{forestFloor} (x,
                            plot_seq=NULL, 
                            limitY=TRUE,
                            order_by_importance=TRUE, 
                            cropXaxes=NULL, 
                            crop_limit=4,
                            plot_GOF = FALSE,
                            GOF_col = "#33333399",
                            ...)
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

\item{limitY}{
TRUE/FLASE, constrain all Yaxis to same limits to ensure relevance of low importance features is not overinterpreted
}

\item{order_by_importance}{
TRUE / FALSE
should plotting and plot_seq be ordered after importance. Most important feature plot first(TRUE)
}

\item{cropXaxes}{
a vector of indice numbers of which zooming of x.axis should look away from outliers
}

\item{crop_limit}{
a number often between 1.5 and 5, referring limit in std.devs from the mean defining outliers
if limit = 2, above selected plots will zoom to +/- 2 std.dev of the respective features.
}

\item{plot_GOF}{
Booleen TRUE/FALSE. Should the goodness of fit be plotted as a line?
}

\item{GOF_col}{
Color of plotted GOF line
}


\item{...}{
... other arguments passed to generic plot functions
}

}
\details{
The method plot.forestFloor visualizes partial plots of the most important variables first. Partial dependence plots are available in the randomForest package. But such plots are single lines(1d-slices) and do not answer the question:
Is this partial function(PF) a fair generalization or subject to global or local interactions. \cr

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

#plot partial functions of most important variables first
plot(ff,order_by_importance=TRUE) 

#Non interacting functions are well displayed, whereas X3 and X4 are not
#by applying different colourgradient, interactions reveal themself 
#also a k-nearest neighbor fit is applied to evaluate goodness of fit
Col=fcol(ff,3,orderByImportance=FALSE)
plot(ff,col=Col,plot_GOF=TRUE) 

#if needed, k-nearest neighbor parameters for goodness-of-fit can be access through convolute_ff
#a new fit will be calculated and added to forstFloor object as ff$FCfit
ff = convolute_ff(ff,userArgs.kknn=alist(kernel="epanechnikov",kmax=5))
plot(ff,col=Col,plot_GOF=TRUE)
 
}
}
\keyword{ multivariate }
\keyword{ models }
\keyword{ nonlinear }
\keyword{ robust }
