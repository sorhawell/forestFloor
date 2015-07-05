\name{recTree}
\alias{recTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
recursiveTree: cross-validated feature contributions
}
\description{
internal C++ function to compute feature contributions for a random Forest

}
\usage{
recTree(vars, obs, ntree, calculate_node_pred, X, Y, leftDaughter, 
    rightDaughter, nodestatus, xbestsplit, nodepred, bestvar, 
    inbag, varLevels, OOBtimes, localIncrements) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
  number of variables in X
%%     ~~Describe \code{RFCV_object} here~~
  }
  \item{obs}{
  number of observations in X
  }
  \item{ntree}{
  number of trees starting from 1 function should iterate, cannot be higher than columns of inbag
  }
  \item{calculate_node_pred}{
  should the node predictions be recalculated(true) or reused from nodepred-matrix(false & regression)
  }
  \item{X}{
  X training matrix
  }
  \item{Y}{
  target vector, factor or regression
  }
  \item{leftDaughter}{
  a matrix from a the output of randomForest rfo$forest$leftDaughter
  the node.number/row.number of the leftDaughter in a given tree by column
  }
  \item{rightDaughter}{
  a matrix from a the output of randomForest rfo$forest$rightDaughter
  the node.number/row.number of the rightDaughter in a given tree by column
  }
  \item{nodestatus}{
  a matrix from a the output of randomForest rfo$forest$nodestatus
  the nodestatus of a given node in a given tree
  }
  \item{xbestsplit}{
  a matrix from a the output of randomForest rfo$forest$xbestsplit \cr
  the split point of numeric variables or the binary split of categorical variables \cr
  see details help(randomForest::getTree) for details of binary expansion of categorical splits
  }
  \item{nodepred}{
  a matrix from a the output of randomForest rfo$forest$xbestsplit
  the inbag target average for regression mode and the majority target class for classification
  }
  \item{bestvar}{
  a matrix from a the output of randomForest rfo$forest$xbestsplit
  the inbag target average for regression mode and the majority target class for classification
  }
  \item{inbag}{
  a matrix from the output of randomForest rfo$inbag for regression \cr
  a matrix from the output of cinbag::trimTrees cinbag.out$inbagCounts \cr
  contains...\cr
  numbers either 0, out of bag, 1 once or multiple times in bag for randomForest function \cr
  positive integer og how many times inbag for cinbag function \cr
  rows represent each observtion of training data and coloumns each tree \cr
  }
  
  \item{varLevels}{
  the number of levels of all varibles, 1 for continous and multinomal, >1 forcategorical variables. This is needed     for categorical variables to interpretate binary split from xbestsplit.
  }
  
  \item{OOBtimes}{
  number of times a certain observation was out of bag in the forest. Needed to compute feature contributions as    they are the sum local increments over out-of-bag obseravations over features divided by the OOBtimes. In previous implementation featurecontributions is summed all observations and is divived by ntrees.
  }
  \item{localIncrements}{
  an empty matrix to store localIncrements during computation. In the end the localIncrement matrix will become the feature contributions.
  }

}
\details{
This is function is excuted by the function forestFloor. \cr
This is a c++/Rcpp implementation computing feature contributions. The main differences from this implementation and the rfFC-package, is that these feature contributions is only summed over out-of-bag samples which give some kind of cross-validation. This implementation allows sample replacement but do not support more than binaray classification as rfFC do.
}

\value{
no output, the feature contributions are writtten directly to localIncrements input
}
\references{
Interpretation of QSAR Models Based on Random Forest Methods, http://dx.doi.org/10.1002/minf.201000173 \cr
Interpreting random forest classification models using a feature contribution method, http://arxiv.org/abs/1312.1121 \cr
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

#compute topology, Rectree is excuted within forestFloor.
#See source-code of forestFloor function to for more details.
ff = forestFloor(rfo,X)

#print forestFloor
print(ff) 

#plot partial functions of most important variables first
plot(ff) 
}

}
