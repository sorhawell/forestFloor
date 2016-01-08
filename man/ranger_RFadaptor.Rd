\name{ranger_RFadaptor.R}
\alias{ranger_RFadaptor.R}

\title{
Bridge ranger to forestFloor
}
\description{
Convert ranger model object to pseudo randomForest model object, which can be processed with forestFloor function
}

\usage{
ranger_RFadaptor = function(ra,y=NULL)
}

\arguments{
  \item{ra}{
  object of class ranger, model fit from ranger
  }
  \item{y}{
  the target vector
  }
}

\details{
so much, yet so little
}

\value{
pseudo randomForest model fit
}

\author{
Soren Havelund Welling
}

\examples{
\dontrun{
data(iris)
library(ranger) #version must be >0.3.6
library(randomForest)
library(forestFloor)

#data, make X,y binds
X = iris[,1:4]
Y = iris[,5]

ra = ranger(Species~.,data=iris,write.forest = T,verbose=T,importance = "permutation",
            replace = T,num.trees =500,keep.inbag = TRUE)
#ra$inbag.counts


#convet ranger
rf = ranger_RFadaptor(ra,Y)

#compute feature contributions
ff = forestFloor(rf,X)

#plot
plot(ff,plot_GOF=TRUE,cex=.7,
     colLists=list(c("#FF0000A5"),
                   c("#00FF0050"),
                   c("#0000FF35")))


#2 - Multi classification example:   (multi is more than two classes)
library(randomForest)

data(iris)
X = iris[,!names(iris) %in% "Species"]
Y = iris[,"Species"]

rf2 = randomForest(
  X,Y,               
  keep.forest=TRUE,  # mandatory
  replace=FALSE,     # if TRUE, use trimTrees::cinbag instead of randomForest
  keep.inbag=TRUE,   # mandatory
  #  samp=20,           # reduce complexity of mapping structure, with same OOB%-explained
  importance  = TRUE # recommended, else ordering by giniImpurity (unstable)
)

ff2 = forestFloor(rf2,X)

plot(ff2,plot_GOF=TRUE,cex=.7,
     colLists=list(c("#FF0000A5"),
                   c("#00FF0050"),
                   c("#0000FF35")))



}
}
