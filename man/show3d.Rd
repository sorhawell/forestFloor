\name{show3d}
\alias{show3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make 3d-plot of forestFloor topology
}
\description{
2 features features(horizontal XY-plane) and one combined feature contribution (vertical Z-axis). Surface response layer will be estimated(gaussian-kNN by kknn package) and plotted alongside the datapoints. 3D grphic device is rgl. Will dispatch methods show3d.forestFloor for regression and show3d_forestFloor_multiClass for classification.

}
\usage{
show3d(x,...)

}
\arguments{
  
  \item{x}{
  S3 class object either "forestFloor"(regression/binary classification) or "forestFloor_multiClass"
  }
 
 \item{...}{
 arguments passed to either method. See help(forestFloor)
 }
 
}

\details{
show3d plot one or more combined feature contributions in the context of two features with points representing each data point.
The input object must be a "forestFloor" or "forestFloor_multiClass" S3 class object , and should at least contain $X the data.frame of training data, $FCmatrix the feature contributions matrix. Usually this object are formed with the function forestFloor having a random forest model as input.
Actual visualization differs for each class.

}

\value{
no value
}

\author{
Soren Havelund Welling
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
Col = fcol(ff,3)
plot(ff,col=Col) 

#in 3D the interaction between X3 and X reveals itself completely
show3d(ff,3:4,col=Col,plot.rgl=list(size=5)) 

#although no interaction, a joined additive effect of X1 and X2
Col = fcol(ff,1:2,X.m=FALSE,RGB=TRUE) #colour by FC-component FC1 and FC2 summed
plot(ff,col=Col) 
show3d(ff,1:2,col=Col,plot.rgl=list(size=5)) 

#...or two-way gradient is formed from FC-component X1 and X2.
Col = fcol(ff,1:2,X.matrix=TRUE,alpha=0.8) 
plot(ff,col=Col) 
show3d(ff,1:2,col=Col,plot.rgl=list(size=5))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
