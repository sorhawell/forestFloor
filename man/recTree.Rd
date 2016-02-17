\name{recTree}
\alias{recTree}
\alias{multiTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
recursiveTree: cross-validated feature contributions
}
\description{
internal C++ functions to compute feature contributions for a random Forest

}
\usage{
recTree(  vars, obs, ntree, calculate_node_pred, X,Y,majorityTerminal, leftDaughter, 
    rightDaughter, nodestatus, xbestsplit, nodepred, bestvar, 
    inbag, varLevels, OOBtimes, localIncrements)

multiTree(vars, obs, ntree, nClasses,            X,Y,majorityTerminal, leftDaughter,
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
  \item{nClasses}{
  number of classes in classification forest
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
  \item{majorityTerminal}{
  bool, majority vote in terminal nodes? Default is FALSE for regression. Set only to TRUE when binary_reg=TRUE.
  }
  \item{leftDaughter}{
  a matrix from a the output of randomForest rf$forest$leftDaughter
  the node.number/row.number of the leftDaughter in a given tree by column
  }
  \item{rightDaughter}{
  a matrix from a the output of randomForest rf$forest$rightDaughter
  the node.number/row.number of the rightDaughter in a given tree by column
  }
  \item{nodestatus}{
  a matrix from a the output of randomForest rf$forest$nodestatus
  the nodestatus of a given node in a given tree
  }
  \item{xbestsplit}{
  a matrix from a the output of randomForest rf$forest$xbestsplit. The split point of numeric variables or the binary split of categorical variables. See help file of randomForest::getTree for details of binary expansion for categorical splits.
  }
  \item{nodepred}{
  a matrix from a the output of randomForest rf$forest$xbestsplit. The inbag target average for regression mode and the majority target class for classification
  }
  \item{bestvar}{
  a matrix from a the output of randomForest rf$forest$xbestsplit the inbag target average for regression mode and the majority target class for classification
  }
  \item{inbag}{
  a matrix as the output of randomForest rf$inbag. Contain counts of how many times a sample was selected for a given tree.
  }
  
  \item{varLevels}{
  the number of levels of all variables, 1 for continuous or discrete, >1 for categorical variables. This is needed     for categorical variables to interpret binary split from xbestsplit.
  }
  
  \item{OOBtimes}{
  number of times a certain observation was out-of-bag in the forest. Needed to compute cross-validated feature contributions as these are summed local increments over out-of-bag observations over features divided by this number. In previous implementation(rfFC), articles(see references) feature contributions are summed by all observations and is divived by ntrees.
  }

  \item{localIncrements}{
  an empty matrix to store localIncrements during computation. As C++ function returns, the input localIncrement matrix contains the feature contributions.
  }

}
\details{
This is function is excuted by the function forestFloor. This is a c++/Rcpp implementation computing feature contributions. The main differences from this implementation and the rfFC-package(Rforge), is that these feature contributions are only summed over out-of-bag samples yields a cross-validation. This implementation allows sample replacement, binary and multi-classification.
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
