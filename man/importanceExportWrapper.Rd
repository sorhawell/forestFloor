\name{importanceExportWrapper}
\alias{importanceExportWrapper}

\title{
Importance Export Wrapper (internal)
}
\description{
wrapping randomForest::importance and check if one and only one importance is exported

}
\usage{
importanceExportWrapper(rf, impType = NULL,impClass = NULL, impScale = NULL)
}
\arguments{
  \item{rf}{
object of class randomForest
}
  \item{type}{
type of importance, 1 permutation based, 2 loss function based (gini/least squares) 
}

\item{class}{
character of factor level to get class specific importance, only for type 1 importance for classification random forest
}


}
\details{
internal wrapper to get importance
}

\value{
vector of importance for each feature
}

\author{
Soren Havelund Welling, 2017
}

\seealso{
importance
}

\examples{
#no examples only for internal use
}
