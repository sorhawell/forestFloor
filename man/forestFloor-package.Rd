\name{forestFloor-package}
\alias{forestFloorPackage}
\docType{package}
\title{
forestFloor: visualize the random forest topology
}
\description{
forrestFloor visualizes cross-validated topology-maps of randomForests(RF).
  Package enables users to understand a non-linear, regression problem or a binary
  classification problem through RF. In all, this package is intended to provide a fast
  overview of dynamics within a given system of interest, allowing the user to decide
  for apropiate further modeling  maybe within a classical statistical framework or to stay
  within the RF-modeling and look deep into  the alluring topology of correlations and
  local interactions.
}
\details{
\tabular{ll}{
Package: \tab forestFloor\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-12-07\cr
License: \tab GPL-2\cr
}
}

\author{
Soren Havelund Welling
}
\references{
Interpretation of QSAR Models Based on Random Forest Methods, http://dx.doi.org/10.1002/minf.201000173 \cr
Interpreting random forest classification models using a feature contribution method, http://arxiv.org/abs/1312.1121 \cr
}

\examples{
\dontrun{
rm(list=ls())
library(forestFloor)
#simulate data
obs=2500
vars = 6 

X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 1 * rnorm(obs))


#grow a forest, remeber to include inbag
rfo=randomForest(X,Y,keep.inbag = TRUE,sampsize=1500,ntree=500)

#compute topology
ff = forestFloor(rfo,X)


#print forestFloor
print(ff) 

#plot partial functions of most important variables first
plot(ff) 

#Non interacting functions are well displayed, whereas X3 and X4 are not
#by applying different colourgradient, interactions reveal themself 
Col = fcol(ff,3,orderByImportance=FALSE)
plot(ff,col=Col) 

#in 3D the interaction between X3 and X reveals itself completely
show3d_new(ff,3:4,col=Col,plot.rgl=list(size=5)) 

#although no interaction, a joined additive effect of X1 and X2
#colour by FC-component FC1 and FC2 summed
Col = fcol(ff,1:2,X.m=FALSE,RGB=TRUE,orderByImportance=FALSE) 
plot(ff,col=Col) 
show3d_new(ff,1:2,col=Col,plot.rgl=list(size=5)) 

#...or two-way gradient is formed from FC-component X1 and X2.
Col = fcol(ff,1:2,X.matrix=TRUE,alpha=0.8,orderByImportance=FALSE) 
plot(ff,col=Col) 
show3d_new(ff,1:2,col=Col,plot.rgl=list(size=5))
}
}


\keyword{ multivariate }
\keyword{ models }
\keyword{ non-linear }
\keyword{ robust}