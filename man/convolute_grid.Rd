\name{convolute_grid}
\alias{convolute_grid}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Predict grid topology by convolution
}

\description{
  n-dimensionl grid wrapper of kknn (not train.kknn). Predicts a grid on the basis of convolution of feature contributions. Can be used to construct one 2D surface in a 3D plot(see show3d example), or to construct multiple 2D slices of a 3D surface in 4D plot (see show4d).
}

\usage{
convolute_grid           (ff,
                          Xvars,
                          FCvars = NULL,
                          grid = 30,
                          limit = 3,
                          zoom = 3,
                          k.fun=function() round(sqrt(n.obs)/2),
                          userArgs.kknn = alist(kernel="gaussian") )
}
\arguments{
  
  \item{ff}{
    forestFloor object(class="forestFloor") concisting of at least ff$X and ff$FCmatrix with two matrices of equal size
  }
  \item{Xvars}{
    integer vector, of col indices of ff$X to convolute by, often of length 2 or 3. Note total number of predictions is a equal grid^"length of this vector". So computation and visualization might be tough.
  }
  \item{FCvars}{
    integer vector, of col indices of ff$FCmatrix. Those feature contributions to conbine(sum) and convolute.
    if none provided will copy Xvars vector, which is the trivial choice.
  }
  
  \item{grid}{
  Either, an integer describing the number of grid.lines in each dimension(trivial choice) or, a full defined matrix of any grid position as defined by this function. If ladder, this function will defining positions of a grid and use the provided one. 
  
  }
  
  \item{limit}{
  numeric scalar, number of stadard deviations away from mean by any dimension to disregard outliers when   spanning observations with grid. Set to limit=Inf outliers never should be disregarded.
  }
  
  \item{zoom}{
  numeric scaler, the size of the grid compared to the univariate range of data.  If zoom=2 the grid will by any dimension span the double range of the observations. Outliers are disregarded with limit argument.
  
  }
  
\item{k.fun}{
  function to define k-neighbors to concider. n.obs is a constant as number of observations in ff$X. Hereby k neighbors is defined as a function k.fun of n.obs. To set k to a constant use e.g. k.fun = function() 10. k can also be overridden with   userArgs.kknn = alist(kernel="gaussian",kmax=10).
}
  
  \item{userArgs.kknn}{
    argument list to pass to train.kknn function for each convolution. See (link) kknn.args.
    arguments in this list have priority of any passed by default by this wrapper function.
    see argument merger append.overwrite.alists
    
  }
  
}
\details{
  This function predicts a grid with kknn which is kNearest neighbor + gaussian weighting. This wrapper can be     used to systematically construct a surface of two feature contributions or a structure of three feature contributions on the basis of the convolution process feature contributions by features. This function is a little experimental for now, and I don't really know how th phrase this right :)
}

\value{
  a data frame, 1 + Xvariable coloumns.
  First column is the predicted summed feature contributions as a function of the following columns feature   coordinates.
}

\author{
  Soren Havelund Welling
}

\examples{
 
\dontrun{
#simulate data
obs=5000
vars = 6 
X = data.frame(replicate(vars,runif(obs)))*2-1
Y = with(X, X1*2 + 2*sin(X2*pi) + 3* (X3+X2)^2 )
Yerror = 1 * rnorm(obs)
var(Y)/var(Y+Yerror)
Y= Y+Yerror

#grow a forest, remember to include inbag
rfo=randomForest::randomForest(X,Y,
                               keep.inbag=TRUE,
                               ntree=1000,
                               replace=TRUE,
                               sampsize=1500,
                               importance=TRUE)

#compute topology
ff = forestFloor(rfo,X)

#print forestFloor
print(ff) 

#plot partial functions of most important variables first
Col=fcol(ff,1)
plot(ff,col=Col,order_by_importance=TRUE) 


#the pure feature contributions
plot3d::rgl(ff$X[,2],ff$X[,3],apply(ff$FCmatrix[,2:3],1,sum),
       #add some colour gradients to ease visualization
       #box.outliers squese all observations in a 2 std.dev box
       #univariately for a vector or matrix and normalize to [0;1]
       col=fcol(ff,2,orderByImportance=FALSE))

#add grid convolution/interpolation
#make grid with current function
grid23 = convolute_grid(ff,Xvars=2:3,userArgs.kknn= alist(k=25,kernel="gaus"),grid=50,zoom=1.2)
#apply grid on 3d-plot
persp3d(unique(grid23[,2]),unique(grid23[,3]),grid23[,1],alpha=0.3,col=c("black","grey"),add=TRUE)
#anchor points of grid could be plotted also
plot3d(grid23[,2],grid23[,3],grid23[,1],alpha=0.3,col=c("black"),add=TRUE)

## and we se that their is almost no variance out of the surface, thus is FC2 and FC3
## well explained by the feature context of both X3 and X4

### next example show how to plot a 3D grid + feature contribution
## this 4D application is very experimental 

#Make grid of three effects, 25^3 = 15625 anchor points
grid123 = convolute_grid(ff,
                         Xvars=c(1:3),
                         FCvars=c(1:3),
                         userArgs.kknn = alist(
                           k= 100,
                           kernel = "gaussian",
                           distance = 1),
                         grid=25,
                         zoom=1.2)

#Select a dimension to place in layers
uni2 = unique(grid123[,2])  #2 points to X1 and FC1
uni2=uni2[c(7,9,11,13,14,16,18)] #select some layers to visualize

## plotting any combination of X2 X3 in each layer(from red to green) having different value of X1
count = 0
add=FALSE
for(i in uni2) {
  count = count +1 
  this34.plane = grid123[grid123[,2]==i,]
  if (count==2) add=TRUE 
  
  #  plot3d(ff$X[,1],ff$X[,2]
  persp3d(unique(this34.plane[,3]),
          unique(this34.plane[,4]),
          this34.plane[,1], add=add, col=rgb(count/length(uni2),1-count/length(uni2),0),alpha=0.1)
}



## plotting any combination of X1 X3 in each layer(from red to green) having different value of X2
uni3 = unique(grid123[,4])  #2 points to X1 and FC1
uni3=uni3[c(7,9,11,13,14,16,18)] #select some layers to visualize
count = 0
add=FALSE
for(i in uni3) {
  count = count +1 
  this34.plane = grid123[grid123[,4]==i,]
  if (count==2) add=TRUE
  
  #plot3d(ff$X[,1],ff$X[,2])
  persp3d(unique(this34.plane[,2]),
          unique(this34.plane[,3]),
          this34.plane[,1], add=add,
          col=rgb(count/length(uni3),1-count/length(uni3),0),alpha=0.1)
} 
}

}
