\name{ggPlotForestFloor}
\alias{ggPlotForestFloor}
\title{
plot forestFloor object with ggplot (beta)
}
\description{
Method to plot an object of forestFloor-class. Plot partial feature contributions of the most important variables. Colour gradients can be applied two show possible interactions.
}
\usage{
ggPlotForestFloor(ff,
                  plot_seq=NULL,
                  col=NULL,
                  orderByImportance = TRUE
                  )
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ff}{
foretFloor-object, also abbrivated ff..
Computed topology of randomForest-model, the output from the forestFloor function \cr
includes also X and Y and importance data
}

\item{plot_seq}{
a numeric vector describing which variables and in what sequence to plot,
ordered by importance as default, order_by_importance = F then by feature/coloumn order of training data.
}


\item{orderByImportance}{
TRUE / FALSE
should plotting and plot_seq be ordered after importance. Most important feature plot first(TRUE)
}

\item{col}{
color of points. Should either be one single colour or a vector of lenght N.samples with colour of each individual sample. Such vector is normally created with eg. ColVec=fcol(ff)
}

}
\details{
Beta ggplot2 will gradually be implemented here. This function relies on ggplot2 and extraGrid
}

\author{
Soren Havelund Welling
}



\examples{
\dontrun{
##testing ggplot functionality
rm(list=ls())
library(forestFloor)
library(randomForest)
#simulate data
obs=1000
vars = 18
X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 1 * rnorm(obs))
#grow a forest, remeber to include inbag
rfo=randomForest(X,Y,keep.inbag = TRUE,sampsize=250,ntree=50)
#compute topology
ff = forestFloor(rfo,X)
ggPlotForestFloor(ff,1:9)
plot(ff,1:9,col=fcol(ff))
}
}