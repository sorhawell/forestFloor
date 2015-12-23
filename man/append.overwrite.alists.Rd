\name{append.overwrite.alists}
\alias{append.overwrite.alists}

\title{
Combine two argument lists
}

\description{
First argument list is master, second list slave
}
\usage{
append.overwrite.alists(masterArgs,slaveArgs)
}

\arguments{
\item{masterArgs}{
List of arguments, of which will stay unchanged
}
\item{slaveArgs}{
List of arguments, conflicts with masterArgs will be deleted. Additional args will be appended.
}
s}

\details{
This function combines to lists of arguments. Conflicts will be resolved by masterArgs.
}

\value{
List of arguments, being masterArgs appended by slaveArgs
}

\author{
Soren Havelund Welling
}

\examples{

arglist1 = alist(monkey="happy",telephone.no=53)
arglist2 = alist(monkey="sad",house.no=12)

#this should yield a alist(monkey="happy", telephone.no=53, house.no=12)
forestFloor:::append.overwrite.alists(arglist1,arglist2)

}

