\name{forestFloor}
\alias{forestFloor}
\alias{forestFloor_randomForest_regression}
\alias{forestFloor_randomForest_classification}

\title{
ForestFloor: Visualize topologies of randomForest model
}
\description{
The function forestFloor computes a cross validated feature contribution matrix from a randomForest model-fit and outputs a forestFloor S3 class object, including importance and the orginal training set. The output object is the basis all visualizations.
}

\usage{
forestFloor(rf.fit,X,calc_np = FALSE, binary_reg = FALSE,...)
}

\arguments{
\item{rf.fit}{
rf.fit, a random forest object as the output from randomForest::randomForest or cinbag::trimTrees \cr
}

\item{X}{
data.frame of input variables, numeric(continnous), descrete(treated as continous) or factors(categoric).
n_rows observations and n_columns features
X MUST be the same data.frame as used to train the random forest, see above item.
}

\item{calc_np}{
  calculate Node Predictions(TRUE) or reuse information from rf.fit(FALSE)?
  slightly faster when FALSE for regression \cr
  This option will only take effect for rf.fit of class "randomForest" and type="regression".   This option, is only for developmental purposes. Set to FALSE always as function will override this choice anyway when needed.
}

\item{binary_reg}{
booleen, if TRUE binary classification can be changed to "percentage votes" of class 1, and thus be treated as regression.
}

\item{...}{
does nothing
}


}


\details{
  forestFloor computes out-of-bag cross validated feature contributions for a "randomForest" class object. Other packages will be supported in future, mail me a request. forestFloor guides you to discover the topology of a randomForest model fit. With input plot_GOF=T is tested for latent interactions for given visualization. If the GOF = .5, then some ~50\% of the variance of a given feature contributions is not explained in that visualization. Check examples how latent interactions can be identified with color gradients. Use show3d to visualize an interaction in an adequate vaiable context.
  
  What is FC?: Feature contributions are the sums over all local increments for each observation for each feature divided by the number of trees. A local increment is the change of node prediction from parent to daughter node over split by a given variable. forestFloor use inbag samples to calculate local increments, but only sum local increments over out-of-bag samples divided with OOBtimes. OOBtimes is the number of times a given observation have out-of-bag which normally is ~ trees / 3. In practice this lowers the leverage of any observation to the feature contributions of this observation. Hereby becomes the visualization less noisy.
  
  What is FC used for?: Feature contributions is smart way to separate a RF mapping topology into additive components. Plotting FC's against variables values yield plots similar to marginal-effect plots, partial dependence plots and vector effect characteristic plots. In this package FC's is identify latent interactions not realized in given visualization.
  
Node predictions, the average target value of inbag samples in any terminal or intermediary node of a rf.fit of class="randomForest"/type="regression" are already calculated and placed in rfo$forest$nodepred. setting input calc_np=TRUE can force re-calculation. Node predictions for classification are the class prevalences in any node of a random forest and must be re-calculated. input calc_np takes no effect.

RF regression takes input features and outputs a target value. RF classification can output a pseudo probability vector with predicted class probability for each sample. The RF mapping topology of classification is different regression as the output as no longer a scalar. For binary classification this topology can be simplified to regression-like as the probability of class_1 = 1 - class_2. Set binary_reg=TRUE for a binary RF classification to get regression like visualizations.

To plot forestFloor objects use plot-method plot.forestFloor and function show3d. Input parameters for classification or regression are not the same, as the visualization are neither. Check help-file \code{\link{plot.forestFloor}} and \code{\link{show3d}}. 

}

\value{
  the forestFloor function outputs(depending on type rf.fit) an object of either class "forestFloor_regression" or "forestFloor_multiClass" with following elements:

\item{X}{
  a copy of the training data or feature space matrix/data.frame, X. The copy is passed from the input of this function. X is used in all visualization to expand the feature contributions over the features of which they were recorded.
}

\item{Y}{
  a copy of the target vector, Y.
}

\item{importance}{
  The gini-importance or permutation-importance a.k.a varaiable importance of the random forest object \cr
  if rfo=randomForest(X,Y,importance=FALSE), gini-importance is used. \cr
  gini-importance is less reproducible and more biased. The extra time used to compute permutation importance is     negliable. 
}

\item{imp_ind}{
  imp_ind, the importance indices is the order to sort the features by descending importance. imp_ind is used by plotting functions to present must relevant feature contributions first. If using gini-importance, the order of plots is more random and will favor continous variables. The plots themselves will not differ.
}

\item{FC_matrix}{
  [ONLY forestFloor_regression.] feature contributions in a matrix. \cr
  n_row observations and n_column features - same dimensions as X.
}

\item{FC_array}{
  [ONLY forestFloor_multiClass.] feature contributions in a array. \cr
  n_row observations and n_column features and n_layer classes. First two dimensions will match dimensions of X.
}


}

\references{
Interpretation of QSAR Models Based on Random Forest Methods, http://dx.doi.org/10.1002/minf.201000173 \cr
Interpreting random forest classification models using a feature contribution method, http://arxiv.org/abs/1312.1121 \cr
}

\author{
Soren Havelund Welling
}

\note{
this version 
}

\seealso{
  \code{\link{plot.forestFloor}},
  \code{\link{show3d}},
}

\examples{

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


\keyword{ multivariate }
\keyword{ models }
\keyword{ nonlinear }
\keyword{ robust }