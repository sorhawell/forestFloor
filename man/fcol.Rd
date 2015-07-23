\name{fcol}
\alias{fcol}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generic colour module for forestFloor obejcts
}

\description{
This colour module colour observations by selected variables. PCA decomposes a selection more than three variables. Space can be inflated by random forest variable importance, to focus colouring on influential variables. Outliers(>3std.dev) are automatically supressed. Any colouring can be modified.
}

\usage{
fcol(ff, cols = NULL, orderByImportance = NULL,  X.matrix = TRUE, 
     hue = NULL, saturation = NULL, brightness = NULL,
     hue.range  = NULL, sat.range  = NULL, bri.range  = NULL,
     alpha = NULL, RGB = NULL, max.df=3,
     imp.weight = NULL, imp.exp = 1,outlier.lim = 3,RGB.exp=NULL)
}

\arguments{
  
  \item{ff}{
  a obejct of class "forestFloor_regression" or "forestFloor_multiClass" or a matrix or a data.frame. No missing values. X.matrix must be set TRUE for "forestFloor_multiClass" as colouring by multiClass feature contributions is not supported.
  }
  \item{cols}{
  vector of indices of columns to colour by, will refer to ff$X if X.matrix=T and else ff$FCmatrix. If ff itself is a matrix or data.frame, indices will refer to these coloums
  }
  
  \item{orderByImportance}{
  logical, should cols refer to X column order or columns sorted by variable importance. Input must be of forestFloor -class to use this. Set to FALSE if no importance sorting is wanted. Otherwise leave as is.
  }
  
  \item{X.matrix}{
  logical, true will use feature matrix false will use feature contribution matrix. Only relvant if input is forestFloor object.
  }
  
  \item{hue}{
  value within [0,1], hue=1 will be exactly as hue = 0 
  colour wheel settings, will skew the colour of all observations without changing the contrast between any two given observations.
  }
  \item{saturation}{
  value within [0,1], mean saturation of colours, 0 is greytone and 1 is maximal colourfull.
  }
  \item{brightness}{
  value within [0,1], mean brightness of colours, 0 is black and 1 is lightly colours.
  }
  \item{hue.range}{
  value within [0,1], ratio of colour wheel, small value is small slice of colour whell those little variation in colours. 1 is any possible colour except for RGB colour system.
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
number from 0 to Inf. Any observation which univariately exceed this limit will be suppressed, as if it actually where on this limit. Normal limit is 3 standard deviations. Extreme outliers can otherwise reserve alone a very large part of a given linear colour gradient. This leeds to visulization where outlier have one colour and any other observation another but same colour.
 }
 
 \item{RGB.exp}{
 value between ]1;>1]. Defines steepness of the gradient of the RGB colour system
 Close to one green midle area is missing.
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
library(forestFloor)
obs=4000 
vars = 6 
X = data.frame(replicate(vars,rnorm(obs))) 
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 0.5 * rnorm(obs)) 

#grow a forest, remeber to include inbag
rfo=randomForest::randomForest(X,Y,keep.inbag=TRUE,
importance=TRUE,sampsize=700)

#compute topology
ff = forestFloor(rfo,X)

#print forestFloor
print(ff) 

#plot partial functions of most important variables first
Colours1=fcol(ff,1)
plot(ff,plot_seq=NULL,col=Colours1)

#try to colour by first four variables, uses PCA du reduce system to 3-way gradient
# (2.5 way more exactly as saturation and brightness by default have very limited ranges
# to avoid gray - or overexposed color tones).
Colours2=fcol(ff,1:4)
plot(ff,plot_seq=NULL,external.col=Colours2) 

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
