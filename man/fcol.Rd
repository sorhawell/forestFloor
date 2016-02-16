\name{fcol}
\alias{fcol}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generic colour module for forestFloor objects
}

\description{
This colour module colour observations by selected variables. PCA decomposes a selection more than three variables. Space can be inflated by random forest variable importance, to focus coloring on influential variables. Outliers(>3std.dev) are automatically suppressed. Any colouring can be modified.
}

\usage{
fcol(ff, cols = NULL, orderByImportance = NULL, plotTest=NULL, X.matrix = TRUE, 
     hue = NULL, saturation = NULL, brightness = NULL,
     hue.range  = NULL, sat.range  = NULL, bri.range  = NULL,
     alpha = NULL, RGB = NULL, byResiduals=FALSE, max.df=3,
     imp.weight = NULL, imp.exp = 1,outlier.lim = 3,RGB.exp=NULL)
}

\arguments{
  
  \item{ff}{
  a object of class "forestFloor_regression" or "forestFloor_multiClass" or a matrix or a data.frame. No missing values. X.matrix must be set TRUE for "forestFloor_multiClass" as colouring by multiClass feature contributions is not supported.
  }
  \item{cols}{
  vector of indices of columns to colour by, will refer to ff$X if X.matrix=T and else ff$FCmatrix. If ff itself is a matrix or data.frame, indices will refer to these columns
  }
  
  \item{orderByImportance}{
  logical, should cols refer to X column order or columns sorted by variable importance. Input must be of forestFloor -class to use this. Set to FALSE if no importance sorting is wanted. Otherwise leave as is.
  }
  
  \item{plotTest}{
  NULL(plot by test set if available), TRUE(plot by test set), FALSE(plot by train), "andTrain"(plot by both test and train) 
  }
  
  \item{X.matrix}{
  logical, true will use feature matrix false will use feature contribution matrix. Only relevant if input is forestFloor object.
  }
  
  \item{hue}{
  value within [0,1], hue=1 will be exactly as hue = 0 
  colour wheel settings, will skew the colour of all observations without changing the contrast between any two given observations.
  }
  \item{saturation}{
  value within [0,1], mean saturation of colours, 0 is grey tone and 1 is maximal colourful.
  }
  \item{brightness}{
  value within [0,1], mean brightness of colours, 0 is black and 1 is lightly colours.
  }
  \item{hue.range}{
  value within [0,1], ratio of colour wheel, small value is small slice of colour wheel those little variation in colours. 1 is any possible colour except for RGB colour system.
  }
  \item{sat.range}{
  value within [0,1], for colouring of 2 or more variables, a range of saturation is needed to  obtain more degrees of freedom in the colour system. But as saturation of is preferred to be >.75 the range of saturation cannot here exceed .5. If NULL sat.range will set widest possible without exceeding range.
  }
  \item{bri.range}{
value within [0,1], for colouring of 3 or more variables, a range of brightness is needed to  obtain more degrees of freedom in the colour system. But as brightness of is preferred to be >.75 the range of saturation cannot here exceed .5. If NULL bri.range will set widest possible without exceeding range.
  }
  \item{alpha}{
  value within [0;1] transparency of colours.
  }
  \item{RGB}{
  logical TRUE/FALSE, \cr
  RGB=NULL: will turn TRUE if one variable selected
  RGB=TRUE: Red-Green-Blue colour: a system with fewer colours(~3) but more contrast. Can still   be altered by hue, saturation, brightness etc.\cr
  RGB=FALSE: True-colour-system: Maximum colour detail. Sometimes more confusing.
  }
  
  \item{byResiduals}{
  logical, should coloring be residuals of main effect fit(overrides X.matrix=). If no fit has been computed "is.null(ff$FCfit)", a temporarily main effect fit will be computed. Use ff = convolute_ff(ff) to only compute once and/or to modify fit parameters.
  }
  
  
  \item{max.df}{
  integer 1, 2, or 3 only. Only for true-colour-system, the maximal allowed degrees of freedom in a colour scale. If more variables selected than max.df, PCA decompose to request degrees of freedom. max.df = 1 will give more simple colour gradients
  }
  \item{imp.weight}{
  Logical?, Should importance from a forestFloor object be used to weight selected variables?
  obviously not possible if input ff is a matrix or data.frame. If randomForest(importance=TRUE) during training, variable importance will be used. Otherwise the more unreliable gini_importance coefficient. 
  }
  
  \item{imp.exp}{
exponent to modify influence of imp.weight. 0 is not influence. -1 is counter influence. 1 is linear influence. .5 is square root influence etc..
  }
  \item{outlier.lim}{
number from 0 to Inf. Any observation which univariately exceed this limit will be suppressed, as if it actually where on this limit. Normal limit is 3 standard deviations. Extreme outliers can otherwise reserve alone a very large part of a given linear colour gradient. This leads to visualization where outlier have one colour and any other observation another but same colour.
 }
 
 \item{RGB.exp}{
 value between ]1;>1]. Defines steepness of the gradient of the RGB colour system
 Close to one green middle area is missing.
 For values higher than 2, green area is dominating
 }
}
  
\details{
fcol produces colours for any observation. These are used plotting.

}
\value{
a character vector specifying the colour of any observations. Each elements is something like "#F1A24340", where F1 is the hexadecimal of the red colour, then A2 is the green, then 43 is blue and 40 is transparency. 
}

\author{
Soren Havelund Welling
}

\examples{
\dontrun{
#example 1 - fcol used on data.frame or matrix
library(forestFloor)
X = data.frame(matrix(rnorm(1000),nrow=1000,ncol=4))
X[] = lapply(X,jitter,amount = 1.5)

#single variable gradient by X1 (Unique colour system)
plot(X,col=fcol(X,1))
#double variable gradient by X1 and X2 (linear colour system)
plot(X,col=fcol(X,1:2))
#triple variable gradient (PCA-decomposed, linear colour system)
plot(X,col=fcol(X,1:3))
#higher based gradient    (PCA-decomposed, linear colour system)
plot(X,col=fcol(X,1:4))


#force linear col + modify colour wheel
plot(X,col=fcol(X,
                cols=1, #colouring by one variable
                RGB=FALSE,
                hue.range = 4, #cannot exceed 1, if colouing by more than one var
                               #except if max.df=1 (limits to 1D gradient)
                saturation=1,
                brightness = 0.6))

#colour by one dimensional gradient first PC of multiple variables
plot(X,col=fcol(X,
                cols=1:2, #colouring by multiple
                RGB=TRUE, #possible because max.df=1
                max.df = 1, #only 1D gradient (only first principal component)
                hue.range = 2, #can exceed 1, because max.df=1
                saturation=.95,
                brightness = 0.8))

##example 2 - fcol used with forestFloor objects
library(forestFloor)
library(randomForest)

X = data.frame(replicate(6,rnorm(1000)))
y = with(X,.3*X1^2+sin(X2*pi)+X3*X4)
rf = randomForest(X,y,keep.inbag = TRUE,sampsize = 400)
ff = forestFloor(rf,X)

#colour by most important variable
plot(ff,col=fcol(ff,1))

#colour by first variable in data set
plot(ff,col=fcol(ff,1,orderByImportance = FALSE),orderByImportance = FALSE)

#colour by feature contributions
plot(ff,col=fcol(ff,1:2,order=FALSE,X.matrix = FALSE,saturation=.95))

#colour by residuals
plot(ff,col=fcol(ff,3,orderByImportance = FALSE,byResiduals = TRUE))

#colour by all features (most useful for colinear variables)
plot(ff,col=fcol(ff,1:6))

#disable importance weighting of colour
#(important colours get to define gradients more)
plot(ff,col=fcol(ff,1:6,imp.weight = FALSE)) #useless X5 and X6 appear more colourful

#insert outlier in data set in X1 and X2
ff$X[1,1] = 10; ff$X[1,2] = 10

plot(ff,col=fcol(ff,1)) #colour not distorted, default: outlier.lim=3
plot(ff,col=fcol(ff,1,outlier.lim = Inf)) #colour gradient distorted by outlier 
plot(ff,col=fcol(ff,1,outlier.lim = 0.5)) #too little outlier.lim 

}
}
