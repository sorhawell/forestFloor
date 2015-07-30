\name{plot_simplex3}
\alias{plot_simplex3}
\title{
3-class simplex forestFloor plot
}
\description{
3-class forestFloor as plotted as 2D simplex. Can also expand to the 2D simplex with one variable into a 3D plot.
}
\usage{
plot_simplex3(ff,
              Xi=NULL,
              includeTotal=TRUE,
              label.col=NULL,
              fig.cols=3,
              fig.rows= NULL,
              auto.alpha=0.25,
              fig3d=FALSE,
              restore_par=TRUE,
              set_pars=TRUE,
              zoom.fit=NULL,
              var.col = NULL,
              plot.sep.centroid=TRUE)
}

\arguments{
  \item{ff}{
  x also abbrivated ff, forestFloor_mulitClass the output from the forestFloor function.
   Must have 3 classes exactly.
}

\item{Xi}{
  vector of integer indices (refeering to columnorder of trainingset) to what feature contributions should be plotted in individual plots.
}

\item{includeTotal}{
  TRUE / FALSE. Combined separation of all feature contributions, which equal to the separation of the entire model can be included.
}

\item{label.col}{
a colour vector of K classes length defining the colour of each class for plotting. NULL is auto.
}

\item{fig.cols}{
  How many columns should be plotted side ways, is passed to par(mfrow=c(fig.rows,fig.cols))
}

\item{fig.rows}{
  How many rows should be plotted, is passed to par(mfrow=c(fig.rows,fig.cols))
  NULL is auto
}

\item{auto.alpha}{
  A scalar 0.5 to 1 most often. Low values increase transparancy of points used to avoid overplotting. auto.alpha is alpha corrected of samplesize such that less adjustment is needed.
}

\item{fig3d}{
  TRUE/FALSE, a 3D plot including the variable as a axis can be co-plotted.
}

\item{restore_par}{
  TRUE/FALSE, calls to graphics par() will be reset
}

\item{set_pars}{
  TRUE/FALSE, if FALSE plot function will rather inherrit plot settings global pars. USeful for multi plotting loops.
}

\item{zoom.fit}{
  NULL/TRUE, if TRUE zooming on samples will be applied. Do not set to FALSE.
}

\item{var.col}{
  NULL, a single colour or a  colour vector of N samples length. Samples will be coloured accordingly. use function fcol to make colour gradient e.g. by the variable values them selves. See example.
}

\item{plot.sep.centroid}{
  TRUE/FALSE. Should the average bootstrap prediction be plotted? If no bootstrap stratification, the average bootstrap prediction is equal to class distribution training set. RF model probalistic predictions is equal average bootstrap prediction plus all feature contributions.
}

}

\details{
Random forest 3 class maps from a feature space to a 3 dimensional (K-1) probability simplex space, which can be plotted in 2D as sum of class probabilities always is one. The mapping function can be plotted additivel for each variable(main effects plus interactions effects) 
}

\author{
Soren Havelund Welling
}


\examples{
\dontrun{
  library(randomForest)
  library(forestFloor)
  require(utils)
  
  data(iris)
  iris
  X = iris[,!names(iris) %in% "Species"]
  Y = iris[,"Species"]
  as.numeric(Y)
  rf.test42 = randomForest(X,Y,keep.forest=T,
    replace=F,keep.inbag=T,samp=15,ntree=100)
  ff.test42 = forestFloor(rf.test42,X,F,F)
  
  plot(ff.test42,compute_GOF=F,plot_GOF=T,cex=.7,
       colLists=list(c("#FF0000A5"),
                     c("#00FF0050"),
                     c("#0000FF35")))
  
  show3d(ff.test42,1:2,3:4,compute_GOF=T)
  
  #plot all effect 2D only
  pars = plot_simplex3(ff.test42,Xi=c(1:3),restore_par=F,zoom.fit=NULL,
    var.col=NULL,fig.cols=2,fig.rows=1,fig3d=F,includeTotal=T,auto.alpha=.4
    ,set_pars=T)
  
  pars = plot_simplex3(ff.test42,Xi=0,restore_par=F,zoom.fit=NULL,
    var.col=alist(alpha=.3,cols=1:4),fig3d=F,includeTotal=T,
    auto.alpha=.8,set_pars=F)
  
  for (I in ff.test42$imp_ind[1:4])  {
    #plotting partial OOB-CV separation(including interactions effects)
    #coloured by true class
    pars = plot_simplex3(ff.test42,Xi=I,restore_par=F,zoom.fit=NULL,
    var.col=NULL,fig.cols=4,fig.rows=2,fig3d=T,includeTotal=F,label.col=1:3,
    auto.alpha=.3,set_pars = (I==ff.test42$imp_ind[1]))
    
    #coloured by varaible value
    pars = plot_simplex3(ff.test42,Xi=I,restore_par=F,zoom.fit=T,
    var.col=alist(order=F,alpha=.8),fig3d=F,includeTotal=(I==4),
    auto.alpha=.3,set_pars=F)
  }
}
}
