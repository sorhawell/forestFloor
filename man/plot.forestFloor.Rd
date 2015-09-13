\name{plot.forestFloor}
\alias{plot.forestFloor}
\alias{plot.forestFloor_regression}
\alias{plot.forestFloor_multiClass}
\title{
plot.forestFloor_regrssion
}
\description{
A method to plot an object of forestFloor-class. Plot partial feature contributions of the most important variables. Colour gradients can be applied two show possible interactions.
}
\usage{
\method{plot}{forestFloor_regression} (
  x,
  plot_seq=NULL, 
  limitY=TRUE,
  orderByImportance=TRUE, 
  cropXaxes=NULL, 
  crop_limit=4,
  plot_GOF = FALSE,
  GOF_col = "#33333399",
  speedup_GOF = TRUE,
  ...)
                          
\method{plot}{forestFloor_multiClass} (
  x,
  plot_seq = NULL,
  label.seq = NULL,
  limitY = TRUE,
  colLists = NULL,
  orderByImportance = TRUE,
  fig.columns = NULL,
  plot_GOF = FALSE,
  GOF_col = NULL,
  speedup_GOF = TRUE,
  jitter_these_cols = NULL,
  jitter.factor = NULL,
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
ordered by importance as default, orderByImportance = F then by feature/coloumn order of training data.
}

\item{label.seq}{
a numeric vector describing which classes and in what sequence to plot. NULL is all classes ordered is in levels in x$Y of forestFloor_mulitClass object x. 
}
\item{fig.columns}{
for multi plotting, how many columns per page.
default(NULL) is 1 for one plot, 2 for 2, 3 for 3, 2 for 4 and 3 for more. 
}
\item{limitY}{
TRUE/FLASE, constrain all Yaxis to same limits to ensure relevance of low importance features is not overinterpreted
}
\item{colLists}{
List of colour vectors of label.seq length. Each element is a colour vector colouring sample class prediction of one class. Vectors should either be of length 1 with one colour for class predictions or of length equal to number of training observations designating colours for all samples. NULL will choose standard one colour per class.
}
\item{jitter_these_cols}{
vector to apply jitter to x-axis in plots. Will refer to variables. Useful to for categorical variables. Default=NULL is no jitter.
}
\item{jitter.factor}{
value to decide howmuch jitter to apply. often between .5 and 3
}
   
\item{orderByImportance}{
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

\item{speedup_GOF}{
Should GOF only computed on reasonable subsample of data set to speedup computation.
GOF estimation leave-one-out-kNN becomes increasingly slow for +1500 samples.
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
#example copied from forestFloor.rd 1.8.6

#disable RGL for headless machines, CRAN testing etc.
#(if you can read this and have a head, don't bother...)
if(!interactive()) {
  Sys.setenv(RGL_USE_NULL=TRUE)
  require(utils) #iris data set
}

###
#1 - Regression example:
set.seed(1234)
library(forestFloor)
library(randomForest)

#simulate data y = x1^2+sin(x2*pi)+x3*x4 + noise
obs = 5000 #how many observations/samples
vars = 6   #how many variables/features
#create 6 normal distr. uncorr. variables
X = data.frame(replicate(vars,rnorm(obs)))
#create target by hidden function
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 0.5 * rnorm(obs)) 

#grow a forest
rfo = randomForest(
  X, #features, data.frame or matrix. Recommended to name columns.
  Y, #targets, vector of integers or floats
  keep.inbag = TRUE,  # mandatory,
  importance = TRUE,  # recommended, else ordering by giniImpurity (unstable)
  sampsize = 1500 ,   # optional, reduce tree sizes to compute faster
  ntree = if(interactive()) 500 else 50 #speedup CRAN testing
)

#compute forestFloor object, often only 5-10\% time of growing forest
ff = forestFloor(
  rf.fit = rfo,       # mandatory
  X = X,              # mandatory
  calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
  binary_reg = FALSE  # takes no effect here when rfo$type="regression"
)

#print forestFloor
print(ff) #prints a text of what an 'forestFloor_regression' object is

plot(1:10,1:10)

plot(ff)

#plot partial functions of most important variables first
plot(ff,                       # forestFloor object
     plot_seq = 1:6,           # optional sequence of features to plot
     orderByImportance=TRUE    # if TRUE index sequence by importance, else by X column  
)
     
#Non interacting features are well displayed, whereas X3 and X4 are not
#by applying color gradient, interactions reveal themself 
#also a k-nearest neighbor fit is applied to evaluate goodness-of-fit
Col=fcol(ff,3,orderByImportance=FALSE) #create color gradient see help(fcol)
plot(ff,col=Col,plot_GOF=TRUE) 

#feature contributions of X3 and X4 are well explained in the context of X3 and X4
# as GOF R^2>.8
show3d(ff,3:4,col=Col,plot_GOF=TRUE,orderByImportance=FALSE)

#if needed, k-nearest neighbor parameters for goodness-of-fit can be accessed through convolute_ff
#a new fit will be calculated and saved to forstFloor object as ff$FCfit
ff = convolute_ff(ff,userArgs.kknn=alist(kernel="epanechnikov",kmax=5))
plot(ff,col=Col,plot_GOF=TRUE) #this computed fit is now used in any 2D plotting.


###
#2 - Multi classification example:   (multi is more than two classes)
set.seed(1234)
library(forestFloor)
library(randomForest)

data(iris)
X = iris[,!names(iris) \%in\% "Species"]
Y = iris[,"Species"]

rf = randomForest(
  X,Y,               
  keep.forest=TRUE,  # mandatory
  replace=FALSE,     # if TRUE, use trimTrees::cinbag instead of randomForest
  keep.inbag=TRUE,   # mandatory
  samp=20,           # reduce complexity of mapping structure, with same OOB\%-explained
  importance  = TRUE # recommended, else ordering by giniImpurity (unstable)
)

ff = forestFloor(rf,X)

plot(ff,plot_GOF=TRUE,cex=.7,
     colLists=list(c("#FF0000A5"),
                   c("#00FF0050"),
                   c("#0000FF35")))

#...and 3D plot, see show3d
show3d(ff,1:2,1:2,plot_GOF=TRUE)

#...and simplex plot (only for three class problems)
plot_simplex3(ff)
plot_simplex3(ff,zoom.fit = TRUE)

#...and 3d simplex plots (rough look, Z-axis is feature)
plot_simplex3(ff,fig3d = TRUE)

###
#3 - binary regression example
#classification of two classes can be seen as regression in 0 to 1 scale
set.seed(1234)
library(forestFloor)
library(randomForest)
data(iris)
X = iris[-1:-50,!names(iris) \%in\% "Species"] #drop third class virginica
Y = iris[-1:-50,"Species"]
Y = droplevels((Y)) #drop unused level virginica

rf = randomForest(
  X,Y,               
  keep.forest=TRUE,  # mandatory
  replace=FALSE,     # if TRUE, use trimTrees::cinbag instead of randomForest
  keep.inbag=TRUE,   # mandatory
  samp=20,           # reduce complexity of mapping structure, with same OOB\%-explained
  importance  = TRUE # recommended, else giniImpurity
)

ff = forestFloor(rf,X,
                 calc_np=TRUE,    #mandatory to recalculate
                 binary_reg=TRUE) #binary regression, scale direction is printed
Col = fcol(ff,1) #color by most important feature
plot(ff,col=Col)   #plot features 
show3d(ff,1:2,col=Col,plot.rgl.args = list(size=2,type="s",alpha=.5)) #interfacing with rgl::plot3d



}

}

\keyword{ multivariate }
\keyword{ models }
\keyword{ nonlinear }
\keyword{ robust }
