\name{show3d_new}
\alias{show3d_new}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make 3d-plot of forestFloor topology
}
\description{
2 features features(horizontal XY-plane) and one combined feature contribution (vertical Z-axis). Surface response layer will be estimated(gaussian-kNN by kknn package) and plotted alongside the datapoints. 3D grphic device is rgl.

}
\usage{
show3d_new(ff,
           Xi  = 1:2,
           FCi = NULL,
           col = "#12345678",    
           sortByImportance = TRUE,
           surface=TRUE,   
           combineFC = sum,  
           zoom=1.2,       
           grid.lines=30,  
           limit=3, 
           kknnGrid.args = alist(),  
           plot.rgl.args = alist(),  
           surf.rgl.args = alist()   
           )

}
\arguments{
  
  \item{ff}{
  forestFloor" class object
  }
  \item{Xi}{
  integer vector of length 2
  indices of feature columns
  }
  \item{FCi}{
  integer vector of length 1 to p variables
  indices of feature contributions columns
  }
  \item{col}{
  colour vector
  points colour or colour palette, can also be passed as promise in plot.rgl.args
  }
  \item{sortByImportance}{
  booleen
  should indices count 'variable importance' order or matrix/data.frame order

  }
  \item{surface}{
  should a surface be plotted also
  }
  \item{combineFC}{
  how should feature contributions be combined
  }
  \item{zoom}{
  #grid can be expanded in all directions by a factor ,zoom
  }
  \item{grid.lines}{
  #how many grid lines should be used
  }
  \item{limit}{
  #sizing of grid does not concider outliers, outside limit of e.g. 3 sd deviations univariately
  }

  \item{kknnGrid.args}{
  argument list, any possiple arguments to kknn{kknn} \cr
  These default wrapper arguments can hereby be overwritten: \cr
  wrapper = alist(
    formula=fc~.,      # do not change\cr
    train=Data,        # do not change\cr
    k=k,               # integer < n_observations. k>100 may run slow. \cr
    kernel="gaussian", #distance kernel, other is e.g. kernel="triangular" \cr
    test=gridX         #do not change \cr
    ) \cr
  see kknn{kknn} to understand paremters. k is set by default automatically to a half times the square root of observations, which often gives a reasonable balance between robustness and  apdeptness. k neighbors and distance kernel can be changed be passing kknnGrid.args = alist(k=5,kernel="triangular",scale=FALSE), hereby will default k and default kernel be overwritten. Moreover the scale argument was not specified by this wrapper and therefore not conflicting, the argument is simply appended.
  
  }
  \item{plot.rgl.args}{
  pass argument to rgl::plot3d, can override any argument of this wrapper, defines plotting space and plot points.
  See plot3d for documentation of graphical arguments.
  
  wrapper_arg = alist(
    x=xaxis, #do not change, x coordinates \cr
    y=yaxis, #do not change, y coordinates \cr
    z=zaxis, #do not change, z coordinates \cr
    col=col, #colouring evaluated within this wrapper function \cr
    xlab=names(X)[1], #xlab, label for x axis \cr
    ylab=names(X)[2], #ylab, label for y axis \cr
    zlab=paste(names(X[,FCi]),collapse=" - "), #zlab, label for z axis  \cr 
    alpha=.4, #points transparancy \cr
    size=3,   #point size \cr
    scale=.7, #z axis scaling \cr
    avoidFreeType = T, #disable freeType=T plug-in. (Postscript labels) \cr
    add=FALSE #do not change, should graphics be added to other rgl-plot? \cr
  )
  }
  
  \item{surf.rgl.args}{
  wrapper_arg = alist(
    x=unique(grid[,2]),  #do not change, values of x-axis \cr
    y=unique(grid[,3]),  #do not change, values of y-axis \cr
    z=grid[,1],          #do not change, response surface values \cr
    add=TRUE,            #do not change, surface added to plotted points \cr
    alpha=0.4            #transparency of surface, [0;1] \cr
  ) \cr         
  see rgl::persp3d for other graphical arguments
  notice the surface is added onto plotting of points, thus can e.g. labels not be changed from here.
  }
}
\details{
show3d plot one or more combined feature contributions in the context of two features with points representing each data point.
The input object must be of  forestFloor class, and should at least contain $X the data.frame of training data, $FCmatrix the feature contributions matrix.

This wrapper function will first plot points. Any graphical argument of the 3D plotting scene can be forwarded through plot.rgl.args, see plot3d{rgl}. The transparency of points could e.g. be lowered from 0.4 to 0.1 and the size increased by using plot.rgl.args = alist(alpha=0.1,size=5)  \cr
Secondly, a grid response surface layer will be estimated(gaussian-kNN), on basis of selected features and feature contributions. Any argument can passed to kknn{kknn} by kknnGrid.args, e.g. kknnGrid.args = alist(k=5, kernel="triangular") \cr
Thirdly the surface grid is plotted. Any argument can be forwarded to persp3d{rgl} through surf.rgl.args. The surface layer could be coloured and made more transparent by passing surf.rgl.args = alist(alpha=0.2, col=c("red","green","blue")). Notice some graphical settings, should as axis labels(xlab) must be defined by plot.rgl.args. \cr

policy of merging user arguments with wrapper arguments is handled by append.overwrite.alists{forestFloor}

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
show3d_new(ff,3:4,col=Col,plot.rgl=list(size=5)) 

#although no interaction, a joined additive effect of X1 and X2
Col = fcol(ff,1:2,X.m=FALSE,RGB=TRUE) #colour by FC-component FC1 and FC2 summed
plot(ff,col=Col) 
show3d_new(ff,1:2,col=Col,plot.rgl=list(size=5)) 

#...or two-way gradient is formed from FC-component X1 and X2.
Col = fcol(ff,1:2,X.matrix=TRUE,alpha=0.8) 
plot(ff,col=Col) 
show3d_new(ff,1:2,col=Col,plot.rgl=list(size=5))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
